
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author John Miller, Hao Peng
 * @version 1.3
 * @date Fri Oct 16 18:14:54 EDT 2015
 * @see LICENSE (MIT style license file).
 */

package scalation.analytics.classifier.par

import java.util.concurrent.ForkJoinPool

import scala.collection.parallel.ForkJoinTaskSupport
import scala.math.abs
import scala.util.control.Breaks.{break, breakable}

import scalation.analytics.classifier.{BayesMetrics, ClassifierInt, TabuFeatures}
import scalation.linalgebra._
import scalation.linalgebra.gen.HMatrix5
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream
import scalation.relalgebra.Relation

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesNetwork2` class implements an Integer-Based Bayesian Network Classifier,
 *  which is a commonly used such classifier for discrete input data.  Each node is
 *  limited to have at most 2 parents, and hence the "2" in the class name `BayesNetwork2`.
 *  The classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier supports limited dependency between features/variables.
 *------------------------------------------------------------------------------
 *  @param x      the integer-valued data vectors stored as rows of a matrix
 *  @param y      the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn     the names for all features/variables
 *  @param k      the number of classes
 *  @param cn     the names for all classes
 *  @param vc     the value count (number of distinct values) for each feature
 *  @param me     use m-estimates (me == 0 => regular MLE estimates)
 *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
 */
class BayesNetwork2 (x: MatriI, y: VectoI, fn: Array [String], private var vc: VectoI = null, k: Int,
                     cn: Array [String], thres: Double = 0.3, me: Int = 3)
    extends ClassifierInt (x, y, fn, k, cn) with BayesMetrics
{
    private val DEBUG       = false                    // debug flag
    private val PARALLELISM = 12                       // parallelism level
    private var g_cor       = calcCorrelation          // feature correlation matrix
    // transform cor into a full matrix from the lower triangular matrix
    for (j1 <- 0 until n; j2 <- 0 until j1) g_cor (j2, j1) = g_cor (j1, j2)

    private val maxRandomRestarts = 10                 // maximum number of random restarts
    private var g_parent = new MatrixI (n, 2)          // vector holding the parent for each feature/variable
    private var g_vcp1 = new VectorI (n)               // value count for parent1
    private var g_vcp2 = new VectorI (n)               // value count for parent2

    private val g_permutedVec = PermutedVecI (VectorI.range (0, n), ranStream)
    private var g_featureOrder = g_permutedVec.igen
    if (DEBUG) println ("Feature Order = " + g_featureOrder)
    private var g_tabu = new TabuFeatures ()           // the tabu list used in feature swapping

    private var g_popC  = new VectorI (k)              // frequency counts for classes 0, ..., k-1
    private var g_probC = new VectorD (k)              // probabilities for classes 0, ..., k-1
    private var g_popX  = new HMatrix5 [Int](k, n)     // conditional frequency counts for variable/feature j: xj
    private var g_probX = new HMatrix5 [Double](k, n)  // conditional probabilities for variable/feature j: xj

    val t_parent = computeParent (g_parent, g_cor, g_featureOrder)
    g_parent = t_parent
    val g_AIC = Array.ofDim [Double] (maxRandomRestarts)
    val g_optimalFeatureOrder = Array.ofDim [VectorI] (maxRandomRestarts)
    if (DEBUG) println ("g_parent Matrix = " + g_parent)
    if (vc == null) {
        shiftToZero; vc = vc_fromData
    } // if

    val (t_vcp1, t_vcp2) = computeVcp (g_vcp1, g_vcp2, g_parent)
    g_vcp1 = t_vcp1
    g_vcp2 = t_vcp2

    // allocate popX & probX
    g_popX.alloc (vc, g_vcp1, g_vcp2)
    g_probX.alloc (vc, g_vcp1, g_vcp2)

    if (DEBUG) {
        println ("value count vc      = " + vc)
        println ("value count for p1  = " + g_vcp1)
        println ("value count for p2  = " + g_vcp2)
        println ("correlation matrix  = " + g_cor)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  Feature 'x_i' is only a possible candidate for parent of feature 'x_j' if
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names of the features
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     */
    def this (xy: MatriI, fn: Array[String], k: Int, cn: Array[String])
    {
        this (xy (0 until xy.dim1, 0 until xy.dim2 - 1), xy.col (xy.dim2 - 1), fn, null, k, cn)
    } // constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature order.
     */
    def getFeatureOrder: VectoI = g_featureOrder

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parent.
     */
    def getParent: MatrixI = g_parent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  Feature 'x_i' is only a possible candidate for parent of feature 'x_j' if
     *  'x_i' appears before 'x_j' in 'featureOrder'.
     *  @param parent        vector holding the parent for each feature/variable
     *  @param cor           feature correlation matrix
     *  @param featureOrder  keep the order of the features
     */
    def computeParent (parent: MatrixI, cor: MatriD, featureOrder: VectorI): MatrixI =
    {
        val cparent = parent
        cparent.set (-1)
        for (j <- 0 until n) {
            val correl = cor (j).map ((x: Double) => abs(x))
            if (correl.max () > thres) {
                if (featureOrder.indexOf (correl.argmax ()) < featureOrder.indexOf(j)) {
                    cparent (j, 0) = correl.argmax ()
                    correl (cparent (j, 0)) = 0
                    if (correl.max () > thres && featureOrder.indexOf (correl.argmax()) < featureOrder.indexOf(j))
                        cparent (j, 1) = correl.argmax ()
                } else {
                    correl (correl.argmax ()) = 0
                    if (correl.max () > thres && featureOrder.indexOf (correl.argmax()) < featureOrder.indexOf(j))
                        cparent (j, 0) = correl.argmax ()
                } // if
            } // if
        } // for
        cparent
    } // computeParent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value counts of each parent feature based on the parent matrix.
     *  @param vcp1    value count for parent1
     *  @param vcp2    value count for parent2
     *  @param parent  vector holding the parent for each feature/variable
     */
    def computeVcp (vcp1: VectorI, vcp2: VectorI, parent: MatrixI): (VectorI, VectorI) =
    {
        val cvcp1 = vcp1
        val cvcp2 = vcp2
        cvcp1.set (1)                 // set default value count to 1 for parent 1
        cvcp2.set (1)                 // set default value count to 1 for parent 2
        for (j <- 0 until n if parent (j, 0) > -1) {
            cvcp1 (j) = vc (parent (j, 0))
            if (parent (j, 1) > -1) cvcp2 (j) = vc (parent (j, 1))
        } // for
        (cvcp1, cvcp2)
    } // computeVcp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     *  @param popC       frequency counts for classes 0, ..., k-1
     *  @param popX       conditional frequency counts for variable/feature j: xj
     *  @param parent     vector holding the parent for each feature/variable
     */
    private def frequencies (testStart: Int, testEnd: Int, popC: VectorI, popX: HMatrix5 [Int], parent: MatrixI):
                (VectorI, HMatrix5[Int]) =
    {
        val fpopC = popC
        val fpopX = popX
        for (l <- 0 until m if l < testStart || l >= testEnd) {                // l = lth row of data matrix x
            val i = y (l)                                                      // get the class
            fpopC (i) += 1                                                      // increment ith class
            for (j <- 0 until n) {
                if (parent (j, 1) > -1) fpopX (i, j, x (l, j), x (l, parent (j, 0)), x (l, parent (j, 1))) += 1
                else if (parent (j, 0) > -1) fpopX (i, j, x (l, j), x (l, parent (j, 0)), 0) += 1
                else fpopX (i, j, x (l, j), 0, 0) += 1
            } // for
        } // for
        (fpopC, fpopX)
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param itrain  indices of the instances considered train data
     *  @param popC    frequency counts for classes 0, ..., k-1
     *  @param popX    conditional frequency counts for variable/feature j: xj
     *  @param parent  vector holding the parent for each feature/variable
     */
    private def frequencies (itrain: Array[Int], popC: VectorI, popX: HMatrix5 [Int], parent: MatrixI):
                (VectorI, HMatrix5[Int]) =
    {
        val fpopC = popC
        val fpopX = popX
        for (l <- itrain) {                                     // l = lth row of data matrix x
            val i = y (l)                                       // get the class
            fpopC (i) += 1                                      // increment ith class
            for (j <- 0 until n) {
                if (parent (j, 1) > -1) fpopX (i, j, x (l, j), x (l, parent (j, 0)), x (l, parent (j, 1))) += 1
                else if (parent (j, 0) > -1) fpopX (i, j, x (l, j), x (l, parent (j, 0)), 0) += 1
                else fpopX (i, j, x (l, j), 0, 0) += 1
            } // for
        } // for
        (fpopC, fpopX)
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region. (exclusive) used in cross-validation.
     *  @param popC       frequency counts for classes 0, ..., k-1
     *  @param popX       conditional frequency counts for variable/feature j: xj
     *  @param probC      probabilities for classes 0, ..., k-1
     *  @param probX      conditional probabilities for variable/feature j: xj
     *  @param vcp1       value count for parent1
     *  @param vcp2       value count for parent2
     *  @param parent     vector holding the parent for each feature/variable
     */
    def train4order (testStart: Int = 0, testEnd: Int = 0, popC: VectorI, popX: HMatrix5 [Int], probC: VectorD,
                     probX: HMatrix5[Double], vcp1: VectorI, vcp2: VectorI, parent: MatrixI): (VectorD, HMatrix5 [Double]) =
    {
        val (tpopC, tpopX) = frequencies (testStart, testEnd, popC, popX, parent)     // compute frequencies skipping test region
        val tprobC = probC
        val tprobX = probX
        for (i <- 0 until k) {                                       // for each class i
            val pci = tpopC (i).toDouble                              // population of class i
            tprobC (i) = pci / md                                     // probability of class i
            for (j <- 0 until n) {                                    // for each feature j
                val me_vc = me / vc (j).toDouble
                for (xj <- 0 until vc (j); xp1 <- 0 until vcp1 (j); xp2 <- 0 until vcp2 (j)) {
                    probX (i, j, xj, xp1, xp2) = (tpopX (i, j, xj, xp1, xp2) + me_vc) / (pci + me)
                } // for
            } // for
        } // for
        (tprobC, tprobX)
    } // train4order

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param itrain  indices of the instances considered train data
     *  @param popC    frequency counts for classes 0, ..., k-1
     *  @param popX    conditional frequency counts for variable/feature j: xj
     *  @param probC   probabilities for classes 0, ..., k-1
     *  @param probX   conditional probabilities for variable/feature j: xj
     *  @param vcp1    value count for parent1
     *  @param vcp2    value count for parent2
     *  @param parent  vector holding the parent for each feature/variable
     */
    def train4order (itrain: Array [Int], popC: VectorI, popX: HMatrix5 [Int], probC: VectorD, probX: HMatrix5 [Double],
                     vcp1: VectorI, vcp2: VectorI, parent: MatrixI): (VectorD, HMatrix5 [Double]) =
    {
        val (tpopC, tpopX) = frequencies (itrain, popC, popX, parent)     // compute frequencies skipping test region
        val tprobC = probC
        val tprobX = probX
        for (i <- 0 until k) {                                    // for each class i
            val pci = tpopC (i).toDouble                          // population of class i
            tprobC (i) = pci / md                                 // probability of class i
            for (j <- 0 until n) {                                // for each feature j
                val me_vc = me / vc (j).toDouble
                for (xj <- 0 until vc (j); xp1 <- 0 until vcp1 (j); xp2 <- 0 until vcp2 (j)) {
                    probX (i, j, xj, xp1, xp2) = (tpopX (i, j, xj, xp1, xp2) + me_vc) / (pci + me)
                } // for
            } // for
        } // for
        (tprobC, tprobX)
    } // train4order

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param itrain  indices of the instances considered train data
     */
    override def train (itrain: Array [Int])
    {
        reset ()
        val (tprobC, tprobX) = train4order (itrain, g_popC, g_popX, g_probC, g_probX, g_vcp1, g_vcp2, g_parent)
        g_probC = tprobC
        g_probX = tprobX
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region. (exclusive) used in cross-validation.
     */
    def train (testStart: Int = 0, testEnd: Int = 0)
    {
        reset ()
        val (tprobC, tprobX) = train4order (testStart, testEnd, g_popC, g_popX, g_probC, g_probX, g_vcp1, g_vcp2, g_parent)
        g_probC = tprobC
        g_probX = tprobX
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        resetHelper (g_parent, g_cor, g_featureOrder, g_vcp1, g_vcp2, g_popC, g_probC, g_popX, g_probX)
    } // reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     *  @param parent        vector holding the parent for each feature/variable
     *  @param cor           feature correlation matrix
     *  @param featureOrder  keep the order of the features
     *  @param vcp1          value count for parent1
     *  @param vcp2          value count for parent2
     *  @param popC          frequency counts for classes 0, ..., k-1
     *  @param probC         probabilities for classes 0, ..., k-1
     *  @param popX          conditional frequency counts for variable/feature j: xj
     *  @param probX         conditional probabilities for variable/feature j: xj
     */
    def resetHelper (parent: MatrixI, cor: MatriD, featureOrder: VectorI, vcp1: VectorI, vcp2: VectorI, popC: VectorI, probC: VectorD,
                     popX: HMatrix5[Int], probX: HMatrix5[Double])
    {
        val r_parent = computeParent (parent, cor, featureOrder)
        val (r_vcp1, r_vcp2) = computeVcp (vcp1, vcp2, r_parent)

        popC.set (0)
        probC.set (0)
        popX.clear ()
        probX.clear ()

        popX.alloc (vc, r_vcp1, r_vcp2)
        probX.alloc (vc, r_vcp1, r_vcp2)
    } // resetHelper

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the Bayes Networks2 classier model by using the 'AIC' criterion.
     *  Limited dependencies between variables/features are also supported.
     *  Maximum number of parents for each feature is 2.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region. (exclusive) used in cross-validation.
     */
    def buildModel (testStart: Int = 0, testEnd: Int = 0)
    {
        var temprange = (0 until maxRandomRestarts).par
        temprange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))
        println ("threads num = " + PARALLELISM)
        for (counter <- temprange) {
            //initialize the model
            var b_parent = new MatrixI (n, 2)                   // vector holding the parent for each feature/variable
            var b_vcp1  = new VectorI (n)                       // value count for parent1
            var b_vcp2  = new VectorI (n)                       // value count for parent2

            val permutedVec = PermutedVecI (VectorI.range (0, n), ranStream)
            var b_featureOrder: VectorI = permutedVec.igen
            b_parent = computeParent (b_parent, g_cor, b_featureOrder)

            var b_tabu  = new TabuFeatures ()                   // the tabu list used in feature swapping
            var b_popC  = new VectorI (k)                       // frequency counts for classes 0, ..., k-1
            var b_probC = new VectorD (k)                       // probabilities for classes 0, ..., k-1
            var b_popX  = new HMatrix5 [Int](k, n)              // conditional frequency counts for variable/feature j: xj
            var b_probX = new HMatrix5 [Double](k, n)           // conditional probabilities for variable/feature j: xj

            val (t_vcp12, t_vcp22) = computeVcp (b_vcp1, b_vcp2, b_parent)
            b_vcp1 = t_vcp12
            b_vcp2 = t_vcp22

            // allocate popX & probX
            b_popX.alloc (vc, b_vcp1, b_vcp2)
            b_probX.alloc (vc, b_vcp1, b_vcp2)
            val (tprobC, tprobX) = train4order (testStart, testEnd, b_popC, b_popX, b_probC, b_probX,
                b_vcp1, b_vcp2, b_parent)
            b_probC = tprobC
            b_probX = tprobX

            var initAIC = aic (vc, b_vcp1, b_vcp2, b_popX, k, me)
            var globalMin = initAIC
            var optimalFeatureOrder = b_featureOrder.toInt
            if (DEBUG) println ("Initial AIC: " + initAIC)

            if (DEBUG) println ("Random Restart #" + counter)
            if (counter > 0) {
                b_featureOrder = permutedVec.igen
                b_tabu.clear ()
                resetHelper (b_parent, g_cor, b_featureOrder, b_vcp1, b_vcp2, b_popC, b_probC, b_popX, b_probX)

                val (tprobC2, tprobX2) = train4order (testStart, testEnd, b_popC, b_popX, b_probC, b_probX,
                    b_vcp1, b_vcp2, b_parent)
                b_probC = tprobC2
                b_probX = tprobX2
                initAIC = aic (vc, b_vcp1, b_vcp2, b_popX, k, me)
            } // if
            if (DEBUG) println ("Initial feature order after restart = " + b_featureOrder)

            //first round of AIC's, stored in a VectorD, of all possible neighboring pair-wise swapping
            val firstAIC = new VectorD (n - 1)
            for (j <- 0 until n - 1) {
                val bfeatureOrder = testSwapping (j, firstAIC, j, b_featureOrder, b_popC, b_popX, b_probC, b_probX, b_vcp1, b_vcp2, b_parent)
                b_featureOrder = bfeatureOrder
            } // for

            if (firstAIC.min () < initAIC) {
                var localMin = firstAIC.min ()
                var toSwap = firstAIC.argmin ()
                b_featureOrder = swapFeatures (toSwap, b_featureOrder, b_tabu)
                if (DEBUG) {
                    println ("Improving AIC locally by")
                    println ("Swapping features " + b_featureOrder (toSwap) + " and " + b_featureOrder (toSwap + 1))
                    println ("New feature order = " + b_featureOrder)
                } // if

                //After the first round of swaps, keep swapping j-1th & j+1th features, and
                //jth & j+2th features, where j is the smaller index of the last swap operation.
                val localAIC = new VectorD (2)

                breakable {
                    while (true) {
                        localAIC.set (Double.MaxValue)
                        // swapping j-1th & jth and j+1th & j+2th elements in the featureOrder
                        if (toSwap > 0 && b_tabu.notInTaboo (b_featureOrder (toSwap - 1), b_featureOrder (toSwap))) {
                            val bfeatureOrder2 = testSwapping (toSwap - 1, localAIC, 0, b_featureOrder, b_popC, b_popX, b_probC, b_probX, b_vcp1, b_vcp2, b_parent)
                            b_featureOrder = bfeatureOrder2

                        } // if
                        if (toSwap + 2 < n && b_tabu.notInTaboo (b_featureOrder (toSwap + 1), b_featureOrder (toSwap + 2))) {
                            val bfeatureOrder3 = testSwapping (toSwap + 1, localAIC, 1, b_featureOrder, b_popC, b_popX, b_probC, b_probX, b_vcp1, b_vcp2, b_parent)
                            b_featureOrder = bfeatureOrder3
                        } // if

                        if (localAIC.min () < localMin) {
                            localMin = localAIC.min
                            toSwap = if (localAIC.argmin () == 0) toSwap - 1 else toSwap + 1

                            if (DEBUG) {
                                println ("Improving AIC locally by")
                                println ("Swapping features " + b_featureOrder (toSwap) + " and " + b_featureOrder (toSwap + 1))
                                println ("New Local Min AIC = " + localMin)
                            } // if

                            b_featureOrder = swapFeatures (toSwap, b_featureOrder, b_tabu)
                        } else {
                            if (DEBUG) {
                                println ("Optimal AIC achieved for this round")
                                println ("Initial AIC for this round = " + initAIC)
                                println ("Final AIC for this round = " + localMin)
                            } // if

                            if (localMin < globalMin) {
                                g_AIC (counter) = localMin
                                g_optimalFeatureOrder (counter) = b_featureOrder
                                if (DEBUG) {
                                    println ("Improving Global Min AIC")
                                    println ("New optimal feature Order = " + b_featureOrder)
                                    println ("New Global Min AIC = " + globalMin)
                                } // if
                            } // if
                            break
                        } // if
                    }
                } // while
            } else {
                if (DEBUG) println ("Initial AIC and feature order for this round are already optimal.")
                if (initAIC < globalMin) {
                    g_AIC (counter) = initAIC
                    g_optimalFeatureOrder (counter) = b_featureOrder
                    if (DEBUG) {
                        println ("Improving Global Min AIC")
                        println ("New optimal feature Order = " + b_featureOrder)
                        println ("New Global Min AIC = " + globalMin)
                    } // if
                } // if
            } // if
        } // for
        g_featureOrder = g_optimalFeatureOrder (g_AIC.indexWhere (x => x == g_AIC.min))
    } // buildModel

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap 'j'th and j'+1'th element of 'featureOrder', and store the result of the
     *  calculated criterion in the 'c'th element of 'criterion', then swap the features back.
     *  @param j             index of 'featureOrder'
     *  @param c             index of criterion
     *  @param criterion     the vector that stores all the calculated criterions,
     *                       i.e., the 'AIC' criterion.
     *  @param featureOrder  keep the order of the features
     *  @param popC          frequency counts for classes 0, ..., k-1
     *  @param popX          conditional frequency counts for variable/feature j: xj
     *  @param probC         probabilities for classes 0, ..., k-1
     *  @param probX         conditional probabilities for variable/feature j: xj
     *  @param vcp1          value count for parent1
     *  @param vcp2          value count for parent2
     *  @param parent        vector holding the parent for each feature/variable
     */
    private def testSwapping (j: Int, criterion: VectoD, c: Int, featureOrder: VectorI, popC: VectorI,
                              popX: HMatrix5 [Int], probC: VectorD, probX: HMatrix5 [Double], vcp1: VectorI,
                              vcp2: VectorI, parent: MatrixI): VectorI =
    {
        val tfeatureOrder = featureOrder
        if (DEBUG) {
            println ("Test swapping...")
            println ("swapping features " + tfeatureOrder (j) + " and " + tfeatureOrder (j + 1) + "...")
        } // if
        tfeatureOrder.swap (j, j + 1)
        if (DEBUG) println ("resetting...")
        resetHelper (parent, g_cor, featureOrder, vcp1, vcp2, popC, probC, popX, probX)
        if (DEBUG) println ("train4ordering...")
        val (tprobC, tprobX) = train4order (0, 0, popC, popX, probC, probX, vcp1, vcp2, parent)

        if (DEBUG) println ("Computing AIC...")
        criterion (c) = aic (vc, vcp1, vcp2, popX, k, me)
        if (DEBUG) println ("New AIC = " + criterion (c))
        if (DEBUG) println ("swapping back features " + tfeatureOrder (j) + " and " + tfeatureOrder (j + 1) + "...")
        tfeatureOrder.swap (j, j + 1)
        tfeatureOrder
    } // testSwapping

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap 'j'th and 'j+1'th element in 'featureOrder', add the swap operation to
     *  the tabu list, reset, and re-train the classifier.
     *  @param j             index of 'featureOrder'
     *  @param featureOrder  keep the order of the features
     *  @param tabu          the tabu list used in feature swapping
     */
    private def swapFeatures (j: Int, featureOrder: VectorI, tabu: TabuFeatures): VectorI =
    {
        val sfeatureOrder = featureOrder
        sfeatureOrder.swap (j, j + 1)
        tabu.addTaboo (sfeatureOrder (j), sfeatureOrder (j + 1), n / 3)
        sfeatureOrder
    } // swapFeatures

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        val g_prob = new VectorD (k)
        for (i <- 0 until k) {
            g_prob (i) = g_probC (i)                // P(C = i)
            for (j <- 0 until n) {
                g_prob (i) *= (if (g_parent (j, 1) > -1) g_probX (i, j, z (j), z (g_parent (j, 0)), z (g_parent (j, 1)))
                // P(X_j = z_j | C = i), 2 parents
                else if (g_parent (j, 0) > -1) g_probX (i, j, z (j), z (g_parent (j, 0)), 0)
                // P(X_j = z_j | C = i), 1 parent
                else g_probX (i, j, z (j), 0, 0))   // P(X_j = z_j | C = i), no parent
            } // for
        } // for
        if (DEBUG) println ("prob = " + g_prob)
        val best = g_prob.argmax ()                 // class with the highest relative posterior probability
        (best, cn(best), g_prob(best))              // return the best class, its name and its probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' object to a string.  FIX - implement
     */
    override def toString: String = "showStructure"

} // BayesNetwork2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesNetwork2` object is the companion object for the `BayesNetwork2` class.
 */
object BayesNetwork2
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a 'BayesNetwork2 object, passing 'x' and 'y' together in one table.
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatriI, fn: Array[String], vc: VectoI = null, k: Int,
               cn: Array[String], thres: Double = 0.3, me: Int = 3) =
    {
        new BayesNetwork2 (xy (0 until xy.dim1, 0 until xy.dim2 - 1), xy.col (xy.dim2 - 1), fn, vc, k,
                           cn, thres, me)
    } // apply

} // BayesNetwork2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesNetwork2Test` object is used to test the `BayesNetwork2` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-BayesNetwork2-example.pdf
 *  > run-main scalation.analytics.classifier.par.BayesNetwork2Test
 */
object BayesNetwork2Test extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                 x0 x1 x2
    val x = new MatrixI ((10, 3), 1, 0, 1,                  // data matrix
                                  1, 0, 1,
                                  1, 0, 1,
                                  0, 0, 1,
                                  0, 0, 0,
                                  0, 1, 0,
                                  0, 1, 0,
                                  0, 1, 1,
                                  1, 1, 0,
                                  1, 0, 0)

    val y = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)          // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin")              // feature/variable names
    val cn = Array ("No", "Yes")                            // class names

    println ("xy = " + (x :^+ y))
    println ("---------------------------------------------------------------")

    val bn2 = new BayesNetwork2 (x, y, fn, null, 2, cn)     // create the classifier

    // train the classifier ---------------------------------------------------

    bn2.buildModel ()
    bn2.train ()

     //test sample ------------------------------------------------------------
      val z1 = VectorI (1, 0, 1)                            // existing data vector to classify
      val z2 = VectorI (1, 1, 1)                            // new data vector to classify
      println("classify (" + z1 + ") = " + bn2.classify (z1) + "\n")
      println("classify (" + z2 + ") = " + bn2.classify (z2) + "\n")

      bn2.crossValidate ()                                  // cross validate the classifier

} // BayesNetwork2Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesNetwork2Test2` object is used to test the `BayesNetwork2` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > run-main scalation.analytics.classifier.par.BayesNetwork2Test2
 */
object BayesNetwork2Test2 extends App
{
    // training-set -----------------------------------------------------------
    // x0: Fast
    // x1: Strong
    // y:  Classification (No/0, Yes/1)
    // features:                  x0 x1  y
    val xy = new MatrixI ((10, 3), 1, 1, 1,
                                   1, 1, 1,
                                   1, 0, 1,
                                   1, 0, 1,
                                   1, 0, 0,
                                   0, 1, 0,
                                   0, 1, 0,
                                   0, 1, 1,
                                   0, 0, 0,
                                   0, 0, 0)

    val fn = Array ("Fast", "Strong")                           // feature names
    val cn = Array ("No", "Yes")                                // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val bn2 = BayesNetwork2 (xy, fn, null, 2, cn, 0.0, 0)       // create the classifier

    // train the classifier ---------------------------------------------------
    bn2.train ()
    bn2.buildModel ()
    bn2.train ()
    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                     // new data vector to classify
    println ("classify (" + z + ") = " + bn2.classify (z) + "\n")
    //bn2.test2()
    bn2.crossValidate ()                                       // cross validate the classifier

} // BayesNetwork2Test2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesNetwork2Test3` object is used to test the `BayesNetwork2` class.
 * > run-main scalation.analytics.classifier.par.BayesNetwork2Test3
 */
object BayesNetwork2Test3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("p", "e") // class names
    val k = 2

    val bn2 = BayesNetwork2 (xy, fn, null, 2, cn, 0.5, 1)       // create the classifier

    bn2.buildModel ()
    bn2.train ()
    var testprint: (Array [Boolean], DAG) = null

    bn2.crossValidate ()

} // BayesNetwork2Test4 object

