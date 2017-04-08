
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author John Miller, Hao Peng, Zhe Jin
 *  @version 1.3
 *  @date Fri Oct 16 18:14:54 EDT 2015
 *  @see LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import java.util.Random

import scala.util.control.Breaks.{break, breakable}

import scalation.linalgebra._
import scalation.linalgebra.gen.{HMatrix2, HMatrix3, HMatrix5}
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream
import scalation.relalgebra.Relation

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OS` class implements an Integer-Based Bayesian Network Classifier,
 *  which is a commonly used such classifier for discrete input data.  Each node is
 *  limited to have at most 2 parents, and hence the "2" in the class name `TwoBAN_OS`.
 *  The classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier supports limited dependency between features/variables.
 *  ------------------------------------------------------------------------------
 *  @param x      the integer-valued data vectors stored as rows of a matrix
 *  @param y      the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn     the names for all features/variables
 *  @param k      the number of classes
 *  @param cn     the names for all classes
 *  @param vc     the value count (number of distinct values) for each feature
 *  @param me     use m-estimates (me == 0 => regular MLE estimates)
 *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
 */
class TwoBAN_OS (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                 private var vc: VectoI = null, thres: Double = 0.0, me: Float = me_default)
      extends BayesClassifier (x, y, fn, k, cn)
{
    private val DEBUG = false                                           // debug flag

    private val parent = new MatrixI (n, 2)                             // vector holding the parent for each feature/variable
    private val vcp1   = new VectorI (n)                                // value count for parent 1
    private val vcp2   = new VectorI (n)                                // value count for parent 2

    private val maxRandomRestarts = 48                                  // maximum number of random restarts
    private val permutedVec  = PermutedVecI (VectorI.range(0, n), ranStream)
    private var featureOrder = permutedVec.igen
    private val tabu = new TabuFeatures ()                              // the tabu list used in feature swapping

    private val f_C  = new VectorI (k)                                  // frequency counts for classes 0, ..., k-1
    private var p_C  = new VectorD (k)                                  // probabilities for classes 0, ..., k-1
    private val f_CXPP  = new HMatrix5 [Int] (k, n)                     // conditional frequency counts for variable/feature j: xj
    private val p_X_CPP = new HMatrix5 [Double] (k, n)                  // conditional probabilities for variable/feature j: xj

    if (vc == null) {
        shiftToZero; vc = vc_fromData                                   // set to default for binary data (2)
    } // if

    private val vca = vc.toArray

    private val N0 = 5.0                                 // parameter needed for smoothing

    //private val g_f_CXPP = new HMatrix5 [Int] (k, n)
    private val g_f_CXZ = new HMatrix5 [Int] (k, n, n, vca, vca)      // joint frequency of C, X, and Z, where X, Z are features/columns
    private val g_f_CX  = new HMatrix3 [Int] (k, n, vca)              // joint frequency of C and X
    private val g_f_C = new VectorI (k)
    private val g_f_X = new HMatrix2[Int] (n, vca)

    private val f_X = new HMatrix2[Int] (n, vca)
    private val f_CXZ = new HMatrix5 [Int] (k, n, n, vca, vca)        // joint frequency of C, X, and Z, where X, Z are features/columns
    private val f_CX  = new HMatrix3 [Int] (k, n, vca)                // joint frequency of C and X

    if (DEBUG) {
        println ("value count vc      = " + vc)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the model by finding the computing global frequencies.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     */
    def buildModel (testStart: Int = 0, testEnd: Int = 0): (Array [Boolean], DAG) =
    {
        // compute frequency values based on the entire dataset
        frequenciesAll ()

        (Array.fill(n)(true), new DAG (parent()))
    } // buildModel

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute frequency counts for each value in each variable from the entire
     *  data matrix.
     */
    def frequenciesAll ()
    {
        for (i <- 0 until m) {
            val yi = y(i)
            g_f_C(yi) += 1
            for (j <- 0 until n) {
                g_f_X(j, x(i, j)) += 1
                g_f_CX(yi, j, x(i, j)) += 1
                for (j2 <- j+1 until n) g_f_CXZ(yi, j, j2, x(i, j), x(i, j2)) += 1
            } // for
        } // for

        for (c <- 0 until k; j <- 0 until n; j2 <- j+1 until n; xj <- 0 until vc(j); xj2 <- 0 until vc(j2)) {
            g_f_CXZ(c, j2, j, xj2, xj) = g_f_CXZ(c, j, j2, xj, xj2)
        } // for
    } // frequenciesAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the conditional mutual information matrix
     */
    def calcCMIQ (itest: Array [Int]): MatrixD =
    {
        val p_CXZ = new HMatrix5 [Double] (k, n, n, vca, vca)    // Joint probability of C, X, and Z, where X, Z are features/columns
        val p_CX  = new HMatrix3 [Double] (k, n, vca)            // Joint probability of C and X
        var p_C: VectorD = null

        copyFreqCMI()
        for (i <- itest) decrementCMI(i)

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compute marginal and joint probabilities
         */
        def probabilities ()
        {
            val tiny = 1E-9
            for (j <- 0 until n) {
                //val me_vc = me / vc(j).toDouble
                for (xj <- 0 until vc(j)) {
                    for (c <- 0 until k) {
                        p_CX(c, j, xj) = (f_CX(c, j, xj) + tiny) / md
                        for (j2 <- j + 1 until n; xj2 <- 0 until vc(j2)) {
                            p_CXZ(c, j, j2, xj, xj2) = (f_CXZ(c, j, j2, xj, xj2) + tiny) / md
                        } // for
                    } // for
                } // for
            } // for
        } // probabilities

        p_C = f_C.toDouble / m
        probabilities ()

        cmiJoint (p_C, p_CX, p_CXZ)
    } // calcCMI

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Alternative constructor
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names of the features
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     */
    def this (xy: MatriI, fn: Array [String], k: Int, cn: Array [String])
    {
        this (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn)
    } // constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature order.
     */
    def getFeatureOrder: VectoI = featureOrder

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parent.
     */
    override def getParent: MatrixI = parent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  Feature 'x_i' is only a possible candidate for parent of feature 'x_j' if
     *  'x_i' appears before 'x_j' in 'featureOrder'.
     *  @param cmiMx  FIX - what ?
     */
    def computeParent (cmiMx: MatrixD)
    {
        parent.set (-1)
        for (j <- 0 until n) {
            val correl = cmiMx(j)
            val jidx = featureOrder.indexOf(j)
            for (k <- jidx until n) correl(featureOrder(k)) = 0
            if (correl.max() > thres) {
                parent(j, 0) = correl.argmax ()
                correl(parent(j, 0)) = 0
                if (correl.max () > thres) parent(j, 1) = correl.argmax ()
            } // if
        } // for
    } // computeParent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value counts of each parent feature based on the parent matrix.
     */
    def computeVcp ()
    {
        vcp1.set(1)                         // set default value count to 1 for parent 1
        vcp2.set(1)                         // set default value count to 1 for parent 2
        for (j <- 0 until n if parent(j, 0) > -1) {
            vcp1(j) = vc(parent(j, 0))
            if (parent(j, 1) > -1) vcp2(j) = vc(parent(j, 1))
        } // for
    } // computeVcp


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region. (exclusive) used in cross-validation.
     */
    def train (testStart: Int = 0, testEnd: Int = 0)
    {
        val cmiMx = calcCMIQ ((testStart until testEnd).toArray)
        learnStructure(cmiMx)
        copyFreqCXPP((testStart until testEnd).toArray)
        train2()
        if (smooth) smoothParam(testEnd - testStart)
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j. This is the quick version that uses
     *  the "subtraction" method to achieve efficiency.
     *  @param itest  indices of the instances considered testing data
     */
    def trainQ (itest: Array [Int])
    {
        val cmiMx = calcCMIQ (itest)
        learnStructure (cmiMx)
        copyFreqCXPP (itest)
        train2()
        if (smooth) smoothParam (itest.size)
    } // trainQ

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     */
    private def train2 ()
    {
        p_C = f_C.toDouble / md                               // prior probability for class yi
        for (i <- 0 until k; j <- 0 until n) {                           // for each class i
            val me_vc = me / vc(j).toDouble
            for (xj <- 0 until vc(j); xp <- 0 until vcp1(j); xp2 <- 0 until vcp2(j)) {
                val d = if (parent(j, 1) > -1) f_CXZ(i, parent(j, 0), parent(j, 1), xp, xp2) + me
                        else if (parent(j, 0) > -1) f_CX(i, parent(j, 0), xp) + me
                        else                        f_C(i) + me
                p_X_CPP(i, j, xj, xp, xp2) = (f_CXPP(i, j, xj, xp, xp2) + me_vc) / d.toDouble
            } // for
        } // for
    } // train2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        f_C.set (0)
        f_X.set(0)
        f_CX.set(0)
        f_CXZ.set(0)

        if (DEBUG) {
            println ("After Reset")
            println ("new feature order = " + featureOrder)
            println ("new parent = " + parent)
        } // if
    } // reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum up the conditional mutual information score of the BN structure
     *  represented by 'parent'
     *  @param parent  parent/predecessor matrix (dim = n x 2)
     *  @param cmiMx   the conditional mutual information matrix
     */
    def scoreCMI (parent: MatrixI = parent, cmiMx: MatrixD): Double =
    {
        var sum = 0.0
        for (j <- 0 until n if parent(j, 0) > -1) {
            sum += cmiMx (j, parent(j, 0))
            if (parent(j, 1) > -1) sum += cmiMx (j, parent(j, 1))
        } // for
        sum
    } // scoreCMI


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Learn the structure Bayes Networks2 classier model by using the 'CMI' criterion.
     *  Limited dependencies between variables/features are also supported.
     *  Maximum number of parents for each feature is 2.
     */
    def learnStructure (cmiMx: MatrixD) =
    {
        // initialize the model
        var optimalFeatureOrder = featureOrder
        computeParent (cmiMx)
        if (DEBUG) println ("Parent Matrix = " + parent)

        var initScore = scoreCMI (parent, cmiMx)
        var globalMax = initScore                           // based on the optimalFeatureOrder
        if (DEBUG) println ("Initial Score: " + initScore)

        for (counter <- 0 until maxRandomRestarts) {
            if (DEBUG) println ("Random Restart #" + counter)
            if (counter > 0) {
                featureOrder = permutedVec.igen
                computeParent (cmiMx)
                tabu.clear ()
                initScore = scoreCMI (parent, cmiMx)   // based on new FeatureOrder
            } // if
            if (DEBUG) println ("Initial feature order after restart = " + featureOrder)

            //first round of Scores, stored in a VectorD, of all possible neighboring pair-wise swapping
            val firstScores = new VectorD (n - 1)
            for (j <- 0 until n - 1) testSwapping (j, firstScores, j, cmiMx)

            if (firstScores.max () > initScore) {
                var localMax = firstScores.max ()
                var toSwap   = firstScores.argmax ()
                swapFeatures (toSwap)                        // optimalFeatureOrder = featureOrder.toInt

                if (DEBUG) {
                    println ("Improving Score locally by")
                    println ("Swapping features " + featureOrder(toSwap) + " and " + featureOrder(toSwap + 1))
                    println ("New feature order = " + featureOrder)
                } // if

                // After the first round of swaps, keep swapping j-1th & j+1th features, and
                // jth & j+2th features, where j is the smaller index of the last swap operation.
                val localScores = new VectorD (2)

                breakable { while (true) {
                    localScores.set(Double.MinValue)
                    // swapping j-1th & jth and j+1th & j+2th elements in the featureOrder
                    if (toSwap > 0 && tabu.notInTaboo(featureOrder(toSwap - 1), featureOrder(toSwap))) testSwapping(toSwap - 1, localScores, 0, cmiMx)
                    if (toSwap + 2 < n && tabu.notInTaboo(featureOrder(toSwap + 1), featureOrder(toSwap + 2))) testSwapping(toSwap + 1, localScores, 1, cmiMx)

                    if (localScores.max() > localMax) {
                        localMax = localScores.max
                        toSwap = if (localScores.argmax() == 0) toSwap - 1 else toSwap + 1

                        if (DEBUG) {
                            println ("Improving Score locally by")
                            println ("Swapping features " + featureOrder(toSwap) + " and " + featureOrder(toSwap + 1))
                            println ("New Local Max Score = " + localMax)
                        } // if

                        swapFeatures (toSwap)
//                      optimalFeatureOrder = featureOrder.toInt
                    } else {
                        if (DEBUG) {
                            println ("Optimal Score achieved for this round")
                            println ("Initial Score for this round = " + initScore)
                            println ("Final Score for this round = " + localMax)
                        } // if

                        if (localMax > globalMax) {
                            globalMax = localMax
                            optimalFeatureOrder = featureOrder
                            if (DEBUG) {
                                println ("Improving Global Max Score")
                                println ("New optimal feature Order = " + featureOrder)
                                println ("New Global Max Score = " + globalMax)
                            } // if
                        } // if
                        break
                    } // if
                }} // while

            } else {
                if (DEBUG) println ("Initial Score and feature order for this round are already optimal.")
                if (initScore > globalMax) {
                    globalMax = initScore
                    optimalFeatureOrder = featureOrder
                    if (DEBUG) {
                        println ("Improving Global Max Score")
                        println ("New optimal feature Order = " + featureOrder)
                        println ("New Global Max Score = " + globalMax)
                    } // if
                } // if
            } // if
        } // for

        featureOrder = optimalFeatureOrder
        computeParent (cmiMx)
        computeVcp ()
        p_X_CPP.alloc (vc, vcp1, vcp2)

        (Array.fill (n)(true), new DAG (parent()))
    } // learnStructure

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap 'j'th and j'+1'th element of 'featureOrder', and store the result of the
     *  calculated criterion in the 'c'th element of 'criterion', then swap the
     *  features back.
     *  @param j          index of 'featureOrder'
     *  @param criterion  the vector that stores all the calculated criterions,
     *                    i.e., the 'Conditional Mutual Information' criterion.
     *  @param c          index of criterion
     */
    private def testSwapping (j: Int, criterion: VectoD, c: Int, cmiMx: MatrixD)
    {
        if (DEBUG) {
            println ("Test swapping...")
            println ("swapping features " + featureOrder(j) + " and " + featureOrder(j + 1) + "...")
        } // if
        featureOrder.swap (j, j + 1)
//      if (DEBUG) println ("resetting...")
//      reset ()
//      if (DEBUG) println ("train4ordering...")
//      train4order()
        if (DEBUG) ("re-computing parent matrix...")
        computeParent(cmiMx)
        if (DEBUG) println ("Computing Score...")
        criterion(c) = scoreCMI(parent, cmiMx)
        if (DEBUG) println ("New Score = " + criterion(c))
        if (DEBUG) println ("swapping back features " + featureOrder(j) + " and " + featureOrder(j + 1) + "...")
        featureOrder.swap (j, j + 1)
    } // testSwapping

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap 'j'th and 'j+1'th element in 'featureOrder', add the swap operation to
     *  the tabu list, reset, and re-train the classifier.
     *  @param j index of 'featureOrder'
     */
    private def swapFeatures (j: Int)
    {
        featureOrder.swap (j, j + 1)
        tabu.addTaboo (featureOrder(j), featureOrder(j + 1), n / 3)
//      reset (); train4order ()
    } // swapFeatures

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its realtive probability
     *  @param z the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        val prob = new VectorD (p_C)
        for (i <- 0 until k; j <- 0 until n) {
            prob(i) *=  (if (parent(j, 1) > -1) p_X_CPP (i, j, z(j), z(parent(j, 0)), z(parent(j, 1)))
                         else if (parent(j, 0) > -1) p_X_CPP(i, j, z(j), z(parent(j, 0)), 0)    // P(X_j = z_j | C = i), parent
                         else                p_X_CPP(i, j, z(j), 0, 0))        // P(X_j = z_j | C = i), no parent (other than the class)
        } // for
        if (DEBUG) println("prob = " + prob)
        val best = prob.argmax()                // class with the highest relative posterior probability
        (best, cn(best), prob(best))            // return the best class, its name and its probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' object to a string.  FIX - to be implemented
     */
    override def toString: String = "showStructure"

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone/copy the values in global freq variables into local ones.
     */
    private def copyFreqCXPP (itest: Array[Int])
    {
        val itrain = (0 until m) diff itest

        f_CXPP.alloc (vc, vcp1, vcp2)
        // compute the joint frequencies of class, feature-X and its two parents
        for (i <- itrain) {
            val yi = y(i)
            for (j <- 0 until n if parent(j, 1) > -1) {
                f_CXPP(yi, j, x(i, j), x(i, parent(j, 0)), x(i, parent(j, 1))) += 1
            } // for
        } // for

        for (i <- 0 until k) {
            for (j <- x.range2; xj <- 0 until vc(j)) {
                for (xp <- 0 until vcp1(j); xp2 <- 0 until vcp2(j) if (parent(j, 1) == -1)) {
                    f_CXPP(i, j, xj, xp, xp2) = if (parent(j, 0) > -1) f_CXZ(i, j, parent(j, 0), xj, xp)
                                                else f_CX(i, j, xj)
                } // for
            } // for
        } // for
    } // copyFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone/copy the values in global freq variables into local ones.
     */
    private def copyFreqCMI ()
    {
        for (i <- 0 until k) {
            f_C(i) = g_f_C(i)
            for (j <- x.range2; xj <- 0 until vc(j)) {
                if (i == 0) f_X(j, xj) = g_f_X(j, xj)
                f_CX(i, j, xj) = g_f_CX(i, j, xj)
                for (j2 <- j+1 until n; xj2 <- 0 until vc(j2)) {
                    f_CXZ(i, j, j2, xj, xj2) = g_f_CXZ(i, j, j2, xj, xj2)
                    f_CXZ(i, j2, j, xj2, xj) = f_CXZ(i, j, j2, xj, xj2)
                } // for
            } // for
        } // for
    } // copyFreqCMI

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decrement frequency counters used in CMI calculations based on the 'i'th
     *  row of the data matrix.
     *  @param i  the index for current data row
     */
    private def decrementCMI (i: Int)
    {
        val yi   = y(i)                                       // get the class for ith row
        f_C(yi) -= 1                                          // decrement frequency for class yi
        for (j <- x.range2) {
            f_X(j, x(i, j)) -= 1
            f_CX (yi, j, x(i, j)) -= 1
            for (j2 <- j+1 until n) {
                f_CXZ (yi, j, j2, x(i, j), x(i, j2)) -= 1
                f_CXZ (yi, j2, j, x(i, j2), x(i, j)) -= 1
            } // for
        } // for
    } // decrement

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform smoothing operations on the learned parameters by using Dirichlet priors
     *  to compute the posterior probabilities of the parameters given the training dataset.
     *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.178.8884&rep=rep1&type=pdf
     *  @param testSize  size of the test size
     */
    private def smoothParam (testSize: Int = 0)
    {
        for (i <- 0 until k) {
            p_C(i) *= m / (m + N0)
            p_C(i) += N0 * k / (m + N0)
            for (j <- 0 until n) {
                val pj = parent(j, 0)
                val pj2 = parent(j, 1)
                for (xj <- 0 until vc(j); xp <- 0 until vcp1(j); xp2 <- 0 until vcp2(j)) {

                    val f_px = if (pj2 > -1) f_CXZ(i, pj, pj2, xp, xp2)
                               else if (pj > -1) f_CX(i, pj, xp)
                               else f_C(i)

                    // NOTE: two alternative priors, may work better for some datasets
//                  val theta0 = if (pj > -1) f_CXZ(i, j, pj, xj, xp) / (md - testSize)
//                               else f_CX(i, j, xj)
//                  val theta0 = f_CX(i, j, xj) / (md - testSize)
                    val theta0 = f_X(j, xj) / (md - testSize)

                    p_X_CPP(i, j, xj, xp, xp2) *= (f_px / (f_px + N0))
                    p_X_CPP(i, j, xj, xp, xp2) += (N0 / (f_px + N0) * theta0)
                } // for
            } // for
        } // for
    } // smoothParam

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the accuracy of the classified results by cross-validation, returning
     *  the accuracy. This version of cross-validation relies on "subtracting"
     *  frequencies from the previously stored global data to achieve efficiency.
     *  @param nx  number of crosses and cross-validations (defaults to 5x).
     */
    override def crossValidateRand (nx: Int = 10): Double =
    {
        val testSize = size / nx
        var sum      = 0.0
        val rng = new Random ()
        val permutedVec = PermutedVecI (VectorI.range(0, size), ranStream)
        val randOrder = permutedVec.igen
        val itestA = randOrder.split(nx)

        for (itest <- itestA) {
            reset ()
            trainQ (itest ().array)
            sum += test (itest)
        } // for

        sum / nx.toDouble
    } // crossValidateRand

} // TwoBAN_OS class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OS` object is the companion object for the `TwoBAN_OS` class.
 */
object TwoBAN_OS
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `TwoBAN_OS object, passing 'x' and 'y' together in one table.
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String], vc: VectoI = null,
               thres: Double = 0.3, me: Float = me_default) =
    {
        new TwoBAN_OS (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn,
                           vc, thres, me)
    } // apply

} // TwoBAN_OS object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OSTest` object is used to test the `TwoBAN_OS` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-TwoBAN_OS-example.pdf
 *  > run-main scalation.analytics.classifier.TwoBAN_OSTest
 */
object TwoBAN_OSTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                x0 x1 x2
    val x = new MatrixI((10, 3), 1, 0, 1,                 // data matrix
                                 1, 0, 1,
                                 1, 0, 1,
                                 0, 0, 1,
                                 0, 0, 0,
                                 0, 1, 0,
                                 0, 1, 0,
                                 0, 1, 1,
                                 1, 1, 0,
                                 1, 0, 0)

    val y = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)        // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin")            // feature/variable names
    val cn = Array ("No", "Yes")                          // class names

    println("xy = " + (x :^+ y))
    println("---------------------------------------------------------------")

    val bn2 = new TwoBAN_OS(x, y, fn, 2, cn)          // create the classifier

    // train the classifier ---------------------------------------------------
    // bn2.train ()
    bn2.buildModel()

    //test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1)                            // existing data vector to classify
    val z2 = VectorI (1, 1, 1)                            // new data vector to classify
    println("classify (" + z1 + ") = " + bn2.classify (z1) + "\n")
    println("classify (" + z2 + ") = " + bn2.classify (z2) + "\n")
    bn2.crossValidateRand ()                              // cross validate the classifier

} // TwoBAN_OSTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OSTest2` object is used to test the `TwoBAN_OS` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > run-main scalation.analytics.classifier.TwoBAN_OSTest2
 */
object TwoBAN_OSTest2 extends App
{
    // training-set -----------------------------------------------------------
    // x0: Fast
    // x1: Strong
    // y:  Classification (No/0, Yes/1)
    // features:                 x0 x1  y
    val xy = new MatrixI((10, 3), 1, 1, 1,
                                  1, 1, 1,
                                  1, 0, 1,
                                  1, 0, 1,
                                  1, 0, 0,
                                  0, 1, 0,
                                  0, 1, 0,
                                  0, 1, 1,
                                  0, 0, 0,
                                  0, 0, 0)

    val fn = Array ("Fast", "Strong")                   // feature names
    val cn = Array ("No", "Yes")                        // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val bn2 = TwoBAN_OS (xy, fn, 2, cn, null, 0)    // create the classifier

    // train the classifier ---------------------------------------------------
//  bn2.train ()
    bn2.buildModel ()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                              // new data vector to classify
    println ("classify (" + z + ") = " + bn2.classify (z) + "\n")

    bn2.crossValidateRand()                             // cross validate the classifier

} // TwoBAN_OSTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OSTest3` object is used to test the `TwoBAN_OS` class.
 *  > run-main scalation.analytics.classifier.TwoBAN_OSTest3
 */
object TwoBAN_OSTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("p", "e")                               // class names
    val k  = 2

    //val vc = VectorI (11, 11, 11, 11, 11, 11, 11, 11, 11)
    //  println ("---------------------------------------------------------------")
    //  println ("D A T A   M A T R I X")
    //  println ("xy = " + xy)
    val bn2 = TwoBAN_OS (xy, fn, 2, cn, null, 0.1, 1)       // create the classifier

    bn2.buildModel ()
    bn2.train ()
    var testprint: (Array [Boolean], DAG) = null

    println("cv accu = " + bn2.crossValidateRand())

} // TwoBAN_OSTest3 object

