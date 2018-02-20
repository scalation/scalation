
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author     John Miller, Hao Peng, Zhe Jin
 *  @version    1.3
 *  @date       Fri Oct 16 18:14:54 EDT 2015
 *  @see        LICENSE (MIT style license file).
 */

package scalation.analytics.classifier.par

import java.util.concurrent.ForkJoinPool

import scala.collection.parallel.ForkJoinTaskSupport
import scala.math.min
import scala.util.control.Breaks.{break, breakable}

import scalation.analytics.classifier.TabuFeatures
import scalation.columnar_db.Relation
import scalation.linalgebra._
import scalation.linalgebra.gen.{HMatrix2, HMatrix3, HMatrix5}
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OS0` class implements an Integer-Based Bayesian Network Classifier,
 *  which is a commonly used such classifier for discrete input data.  Each node is
 *  limited to have at most 2 parents, and hence the "2" in the class name `TwoBAN_OS`.
 *  The classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier supports limited dependency between features/variables.
 *
 *  This classifier uses the standard cross-validation technique.
 *  ------------------------------------------------------------------------------
 *  @param x            the integer-valued data vectors stored as rows of a matrix
 *  @param y            the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn           the names for all features/variables
 *  @param k            the number of classes
 *  @param cn           the names for all classes
 *  @param vc           the value count (number of distinct values) for each feature
 *  @param me           use m-estimates (me == 0 => regular MLE estimates)
 *  @param thres        the correlation threshold between 2 features for possible parent-child relationship
 *  @param PARALLELISM  the level of parallelism, the number of threads to use
 */
class TwoBAN_OS0 (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String], protected var vc: Array [Int] = null,
                  thres: Double = 0.0, me: Double = me_default, private val PARALLELISM: Int = Runtime.getRuntime ().availableProcessors ())
      extends BayesClassifier (x, y, fn, k, cn, PARALLELISM)
{
    private val DEBUG = false                                             // debug flag

    protected var parent = new MatrixI (n, 2)                             // vector holding the parent for each feature/variable
    protected val vcp1   = Array.ofDim [Int] (n)                          // value count for parent 1
    protected val vcp2   = Array.ofDim [Int] (n)                          // value count for parent 2

    protected val maxRandomRestarts = 48                                  // maximum number of random restarts
    protected val permutedVec  = PermutedVecI (VectorI.range(0, n), ranStream)
    protected var featureOrder = permutedVec.igen

    protected var f_CXPP  = new HMatrix5 [Int] (k, n)                     // conditional frequency counts for variable/feature j: xj
    protected val p_X_CPP = new HMatrix5 [Double] (k, n)                  // conditional probabilities for variable/feature j: xj

    if (vc == null) {
        shiftToZero; vc = vc_fromData                                     // set to default for binary data (2)
    } // if

    f_X   = new HMatrix2 [Int] (n, vc)                                    // Frequency of X
    f_CX  = new HMatrix3 [Int] (k, n, vc)                                 // Joint frequency of C and X
    f_CXZ = new HMatrix5 [Int] (k, n, n, vc, vc)                          // Joint frequency of C, X, and Z, where X, Z are features/columns

    protected val scoreArray = new VectorD (maxRandomRestarts)
    protected val featOrderArray = Array.ofDim [VectoI] (maxRandomRestarts)

    if (DEBUG) println ("value count vc = " + vc.deep)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j. This is the quick version that uses
     *  the "subtraction" method to achieve efficiency.
     *  @param itest  indices of the instances considered testing data
     */
    override def train (itest: IndexedSeq [Int]): TwoBAN_OS0 =
    {
        val idx = if (additive) 0 until m diff itest else itest
        val cmiMx = calcCMI (idx, vc)
        learnStructure (cmiMx)
        copyFreqCXPP (if (additive) idx else 0 until m diff itest)
        train2 ()
        if (smooth) smoothParam (itest.size)
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     */
    private def train2 ()
    {
        p_C = f_C.toDouble / md                               // prior probability for class yi
        for (i <- (0 until k).par; j <- (0 until n).par if fset(j)) {    // for each class i
        val me_vc = me / vc(j).toDouble
            for (xj <- (0 until vc(j)).par; xp <- (0 until vcp1(j)).par; xp2 <- (0 until vcp2(j)).par) {
                val d = if      (parent(j, 1) > -1) f_CXZ(i, parent(j, 0), parent(j, 1), xp, xp2) + me
                        else if (parent(j, 0) > -1) f_CX(i, parent(j, 0), xp) + me
                        else                        f_C(i) + me
                p_X_CPP(i, j, xj, xp, xp2) = (f_CXPP(i, j, xj, xp, xp2) + me_vc) / d.toDouble
            } // for
        } // for
    } // train2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone/copy the values in global freq variables into local ones.
     *  Also compute frequencies for features with exactly two features since
     *  this frequency computation is not done/needed in CMI calculations.
     *  @param itrain   indices of training region
     */
    private def copyFreqCXPP (itrain: IndexedSeq [Int])
    {
        val itrainA = split(itrain, PARALLELISM)

        f_CXPP.alloc (vc, vcp1, vcp2)       // compute the joint frequencies of class, feature-X and its two parents

        val f_CXPPw = Array.ofDim [HMatrix5 [Int]](PARALLELISM)
        for (w <- (0 until PARALLELISM).par) {
            f_CXPPw(w) = new HMatrix5 [Int] (k, n)
            f_CXPPw(w).alloc(vc, vcp1, vcp2)
        } // for

        val paraRange = (0 until PARALLELISM).par
        paraRange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))

        for (w <- paraRange; i <- itrainA(w)) {
            val yi = y(i)
            for (j <- 0 until n if fset(j) && parent(j, 1) > -1) {
                f_CXPPw(w)(yi, j, x(i, j), x(i, parent(j, 0)), x(i, parent(j, 1))) += 1
            } // for
        } // for

        for (w <- 0 until PARALLELISM) {
            f_CXPP += f_CXPPw (w)
        } // for

        for (i <- (0 until k).par) {
            for (j <- x.range2.par if fset(j); xj <- (0 until vc(j)).par) {
                for (xp <- (0 until vcp1(j)).par; xp2 <- (0 until vcp2(j)).par if (parent(j, 1) == -1)) {
                    f_CXPP(i, j, xj, xp, xp2) = if (parent(j, 0) > -1) f_CXZ(i, j, parent(j, 0), xj, xp)
                    else f_CX(i, j, xj)
                } // for
            } // for
        } // for
    } // copyFreqCXPP

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment frequency counters used in CMI calculations based on the 'i'th
     *  row of the data matrix.
     *  @param i        the index for current data row
     *  @param f_C      frequency table of class C
     *  @param f_X      frequency table of feature X
     *  @param f_CX     joint frequency table of C and X
     *  @param f_CXZ    joint frequency table of C, X, and Z, where X and Z are features/columns
     */
    protected override def updateFreq (i: Int, f_C: VectoI, f_X: HMatrix2 [Int], f_CX: HMatrix3 [Int], f_CXZ: HMatrix5 [Int])
    {
        val yi   = y(i)                                       // get the class for ith row
        f_C(yi) += 1                                          // decrement frequency for class yi
        for (j <- x.range2 if fset(j)) {
            f_X(j, x(i, j)) += 1
            f_CX (yi, j, x(i, j)) += 1
            for (j2 <- j+1 until n if fset(j2)) {
                f_CXZ (yi, j, j2, x(i, j), x(i, j2)) += 1
                f_CXZ (yi, j2, j, x(i, j2), x(i, j)) += 1
            } // for
        } // for
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Learn the structure of Bayes Networks2 classier model by using the 'CMI' criterion.
     *  Limited dependencies between variables/features are also supported.
     *  Maximum number of parents for each feature is 2.
     */
    def learnStructure(cmiMx: MatrixD) =
    {
        val paraRange = (0 until maxRandomRestarts).par
        paraRange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))

        for (counter <- paraRange) {
            if (DEBUG) println ("Random Restart #" + counter)
            val featureOrder = permutedVec.igen
            val tabu = new TabuFeatures ()
            val parent = computeParent (featureOrder, cmiMx)
            val initScore = scoreCMI (parent, cmiMx)   // based on new FeatureOrder

            if (DEBUG) println ("Initial feature order after restart = " + featureOrder)

            //first round of Scores, stored in a VectorD, of all possible neighboring pair-wise swapping
            val firstScores = new VectorD (n - 1)
            for (j <- 0 until n - 1) testSwapping (featureOrder, j, firstScores, j, cmiMx)

            if (firstScores.max () > initScore) {
                var localMax = firstScores.max ()
                var toSwap   = firstScores.argmax ()
                swapFeatures (featureOrder, tabu, toSwap)                        // optimalFeatureOrder = featureOrder.toInt

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
                    if (toSwap > 0 && tabu.notInTaboo (featureOrder(toSwap - 1), featureOrder(toSwap)))
                        testSwapping (featureOrder, toSwap - 1, localScores, 0, cmiMx)
                    if (toSwap + 2 < n && tabu.notInTaboo (featureOrder(toSwap + 1), featureOrder(toSwap + 2)))
                        testSwapping (featureOrder, toSwap + 1, localScores, 1, cmiMx)

                    if (localScores.max() > localMax) {
                        localMax = localScores.max
                        toSwap = if (localScores.argmax() == 0) toSwap - 1 else toSwap + 1

                        if (DEBUG) {
                            println ("Improving Score locally by")
                            println ("Swapping features " + featureOrder(toSwap) + " and " + featureOrder(toSwap + 1))
                            println ("New Local Max Score = " + localMax)
                        } // if

                        swapFeatures (featureOrder, tabu, toSwap)
                    } else {
                        if (DEBUG) {
                            println ("Optimal Score achieved for this round")
                            println ("Initial Score for this round = " + initScore)
                            println ("Final Score for this round = " + localMax)
                        } // if
                        featOrderArray(counter) = featureOrder
                        scoreArray(counter) = localMax
                        break
                    } // if
                }} // while

            } else {
                if (DEBUG) println("Initial Score and feature order for this round are already optimal.")
                featOrderArray(counter) = featureOrder
                scoreArray(counter) = initScore
            } // if
        } // for

        featureOrder = featOrderArray(scoreArray.argmax())
        parent = computeParent(featureOrder, cmiMx)
        computeVcp()
        p_X_CPP.alloc (vc, vcp1, vcp2)

    } // learnStructure

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  Feature 'x_i' is only a possible candidate for parent of feature 'x_j' if
     *  'x_i' appears before 'x_j' in 'featureOrder'.
     *  @param featureOrder     order of the features
     *  @param cmiMx            the Conditional Mutual Information matrix
     */
    def computeParent (featureOrder: VectoI = featureOrder, cmiMx: MatrixD): MatrixI =
    {
        val parent = new MatrixI (n, 2)
        parent.set(-1)
        for (j <- 0 until n if fset(j)) {
            val correl = cmiMx(j)
            val jidx = featureOrder.indexOf(j)
            for (k <- jidx until n) correl(featureOrder(k)) = 0
            if (correl.max() > thres) {
                parent(j, 0) = correl.argmax()
                correl(parent(j, 0)) = 0
                if (correl.max() > thres) parent(j, 1) = correl.argmax()
            } // if
        } // for
        parent
    } // computeParent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value counts of each parent feature based on the parent matrix.
     *  Let 1 be the default value count when there is no parent.
     */
    def computeVcp ()
    {
        for (j <- (0 until n).par) {
            vcp1(j) = if (fset(j) && parent(j, 0) > -1) vc(parent(j, 0)) else 1
            vcp2(j) = if (fset(j) && parent(j, 0) > -1 && parent(j, 1) > -1) vc(parent(j, 1)) else 1
        } // for
    } // computeVcp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum up the conditional mutual information score of the BN structure
     *  represented by 'parent'
     *  @param parent    parent/predecessor matrix (dim = n x 2)
     *  @param cmiMx     the conditional mutual information matrix
     */
    def scoreCMI (parent: MatrixI = parent, cmiMx: MatrixD): Double =
    {
        var sum = 0.0
        for (j <- 0 until n if fset(j) && parent(j, 0) > -1) {
            sum += cmiMx (j, parent(j, 0))
            if (parent(j, 1) > -1) sum += cmiMx (j, parent(j, 1))
        } // for
        sum
    } // scoreCMI

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap 'j'th and j'+1'th element of 'featureOrder', and store the result of the
     *  calculated criterion in the 'c'th element of 'criterion', then swap the
     *  features back.
     *
     *  @param j          index of 'featureOrder'
     *  @param criterion  the vector that stores all the calculated criterions,
     *                    i.e., the 'Conditional Mutual Information' criterion.
     *  @param c          index of criterion
     */
    private def testSwapping (featureOrder: VectoI, j: Int, criterion: VectoD, c: Int, cmiMx: MatrixD)
    {
        if (DEBUG) {
            println ("Test swapping...")
            println ("swapping features " + featureOrder(j) + " and " + featureOrder(j + 1) + "...")
        } // if
        featureOrder.swap (j, j + 1)
        if (DEBUG) ("re-computing parent matrix...")
        val parent = computeParent(featureOrder, cmiMx)
        if (DEBUG) println ("Computing Score...")
        criterion(c) = scoreCMI(parent, cmiMx)
        if (DEBUG) println ("New Score = " + criterion(c))
        if (DEBUG) println ("swapping back features " + featureOrder(j) + " and " + featureOrder(j + 1) + "...")
        featureOrder.swap (j, j + 1)
    } // testSwapping

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap 'j'th and 'j+1'th element in 'featureOrder', add the swap operation to
     *  the tabu list, reset, and re-train the classifier.
     *  @param featureOrder     order of features
     *  @param tabu             tabu list of features
     *  @param j                index of 'featureOrder'
     */
    private def swapFeatures (featureOrder: VectoI, tabu: TabuFeatures, j: Int)
    {
        featureOrder.swap (j, j + 1)
        tabu.addTaboo (featureOrder(j), featureOrder(j + 1), n / 3)
    } // swapFeatures

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform smoothing operations on the learned parameters by using Dirichlet priors
     *  to compute the posterior probabilities of the parameters given the training dataset.
     *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.178.8884&rep=rep1&type=pdf
     *  @param testSize  size of the test size
     */
    private def smoothParam (testSize: Int = 0)
    {
        for (i <- (0 until k).par) {
            p_C(i) *= m / (m + N0)
            p_C(i) += N0 * k / (m + N0)
            for (j <- (0 until n).par if fset(j)) {
                val pj = parent(j, 0)
                val pj2 = parent(j, 1)
                for (xj <- (0 until vc(j)).par; xp <- (0 until vcp1(j)).par; xp2 <- (0 until vcp2(j)).par) {

                    val f_px = if (pj2 > -1) f_CXZ(i, pj, pj2, xp, xp2)
                    else if (pj > -1) f_CX(i, pj, xp)
                    else f_C(i)

                    //NOTE: two alternative priors, may work better for some datasets
//                    val theta0 = if (pj > -1) f_CXZ(i, j, pj, xj, xp) / (md - testSize)
//                                 else f_CX(i, j, xj)
//                    val theta0 = f_CX(i, j, xj) / (md - testSize)
                    val theta0 = f_X(j, xj) / (md - testSize)

                    p_X_CPP(i, j, xj, xp, xp2) *= (f_px / (f_px + N0))
                    p_X_CPP(i, j, xj, xp, xp2) += (N0 / (f_px + N0) * theta0)
                } // for
            } // for
        } // for
    } // smoothParam

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its realtive probability
     *
     *  @param z the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        val prob = new VectorD (p_C)
        for (i <- 0 until k; j <- 0 until n if fset(j)) {
            prob(i) *=  (if      (parent(j, 1) > -1) p_X_CPP (i, j, z(j), z(parent(j, 0)), z(parent(j, 1)))
                         else if (parent(j, 0) > -1) p_X_CPP(i, j, z(j), z(parent(j, 0)), 0)
                         else                        p_X_CPP(i, j, z(j), 0, 0))
        } // for
        if (DEBUG) println("prob = " + prob)
        val best = prob.argmax()                // class with the highest relative posterior probability
        (best, cn(best), prob(best))            // return the best class, its name and its probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables to 0.
     */
    def reset ()
    {
        f_C.set (0)
        f_X.set (0)
        f_CX.set (0)
        f_CXZ.set (0)
    } // reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature order.
     */
    def getFeatureOrder: VectoI = featureOrder

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parent.
     */
    def getParent: MatrixI = parent

} // TwoBAN_OS0 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OS0` object is the companion object for the `TwoBAN_OS0` class.
 */
object TwoBAN_OS0
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `TwoBAN_OS0' object, passing 'x' and 'y' together in one table.
     *  @param xy    the data vectors along with their classifications stored as rows of a matrix
     *  @param fn           the names for all features/variables
     *  @param k            the number of classes
     *  @param cn           the names for all classes
     *  @param vc           the value count (number of distinct values) for each feature
     *  @param me           use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres        the correlation threshold between 2 features for possible parent-child relationship
     *  @param PARALLELISM  the level of parallelism, the number of threads to use
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String], vc: Array [Int] = null,
               thres: Double = 0.0, me: Double = me_default, PARALLELISM: Int = Runtime.getRuntime ().availableProcessors ()) =
    {
        new TwoBAN_OS0 (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn,
                            vc, thres, me, PARALLELISM)
    } // apply

} // TwoBAN_OS0 object


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
 *
 *  This classifier uses the standard cross-validation technique.
 *  ------------------------------------------------------------------------------
 *
 *  @param x            the integer-valued data vectors stored as rows of a matrix
 *  @param y            the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn           the names for all features/variables
 *  @param k            the number of classes
 *  @param cn           the names for all classes
 *  @param vc           the value count (number of distinct values) for each feature
 *  @param me           use m-estimates (me == 0 => regular MLE estimates)
 *  @param thres        the correlation threshold between 2 features for possible parent-child relationship
 *  @param PARALLELISM  the level of parallelism, the number of threads to use
 */
class TwoBAN_OS (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String], vc_ : Array [Int] = null,
                 thres: Double = 0.0, me: Double = me_default, private val PARALLELISM: Int = Runtime.getRuntime ().availableProcessors ())
        extends TwoBAN_OS0 (x, y, fn, k, cn, vc_, thres, me, PARALLELISM)
{
    private val DEBUG = false                                         // debug flag

    private var g_f_CXZ = new HMatrix5 [Int] (k, n, n, vc, vc)        // global joint frequency of C, X, and Z, where X, Z are features/columns
    private var g_f_CX  = new HMatrix3 [Int] (k, n, vc)               // global joint frequency of C and X
    private val g_f_C   = new VectorI (k)                             // global frequency of C
    private var g_f_X   = new HMatrix2[Int] (n, vc)                   // global frequency of X

    additive = false

    if (DEBUG) println ("value count vc = " + vc.deep)

    frequenciesAll ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute frequency counts for each value in each variable from the entire
     *  data matrix.
     */
    def frequenciesAll ()
    {
        val size   = m / PARALLELISM + 1
        val g_f_Cw   = Array.ofDim [VectorI] (PARALLELISM)
        val g_f_Xw   = Array.ofDim [HMatrix2 [Int]] (PARALLELISM)
        val g_f_CXw  = Array.ofDim [HMatrix3 [Int]](PARALLELISM)
        val g_f_CXZw = Array.ofDim [HMatrix5 [Int]](PARALLELISM)

        for (w <- 0 until PARALLELISM) {
            g_f_Cw (w)   = new VectorI (k)
            g_f_Xw (w)   = new HMatrix2 [Int] (n, vc)
            g_f_CXw (w)  = new HMatrix3 [Int] (k, n, vc)
            g_f_CXZw (w) = new HMatrix5 [Int] (k, n, n, vc, vc)
        } // for

        val paraRange = (0 until PARALLELISM).par
        paraRange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))

        for (w <- paraRange; i <- w * size until min ((w + 1) * size, m)) {
            val yi = y(i)
            g_f_Cw(w)(yi) += 1
            for (j <- 0 until n if fset(j)) {
                g_f_Xw(w)(j, x(i, j)) += 1
                g_f_CXw(w)(yi, j, x(i, j)) += 1
                for (j2 <- j+1 until n if fset(j2)) g_f_CXZw(w)(yi, j, j2, x(i, j), x(i, j2)) += 1
            } // for
        } // for

        for (w <- 0 until PARALLELISM) {
            g_f_C  += g_f_Cw (w)
            g_f_X  += g_f_Xw (w)
            g_f_CX += g_f_CXw (w)
            g_f_CXZ += g_f_CXZw(w)
        } // for

        for (c <- (0 until k).par; j <- (0 until n).par if fset(j); j2 <- (j+1 until n).par if fset(j2);
             xj <- (0 until vc(j)).par; xj2 <- (0 until vc(j2)).par) {
            g_f_CXZ(c, j2, j, xj2, xj) = g_f_CXZ(c, j, j2, xj, xj2)
        } // for
    } // frequenciesAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decrement frequency counters used in CMI calculations based on the 'i'th
     *  row of the data matrix.
     *  @param i        the index for current data row
     *  @param f_C      frequency table of class C
     *  @param f_X      frequency table of feature X
     *  @param f_CX     joint frequency table of C and X
     *  @param f_CXZ    joint frequency table of C, X, and Z, where X and Z are features/columns
     */
    protected override def updateFreq (i: Int, f_C: VectoI, f_X: HMatrix2[Int], f_CX: HMatrix3[Int], f_CXZ: HMatrix5[Int])
    {
        val yi   = y(i)                                       // get the class for ith row
        f_C(yi) -= 1                                          // decrement frequency for class yi
        for (j <- x.range2 if fset(j)) {
            f_X(j, x(i, j)) -= 1
            f_CX (yi, j, x(i, j)) -= 1
            for (j2 <- j+1 until n if fset(j2)) {
                f_CXZ (yi, j, j2, x(i, j), x(i, j2)) -= 1
                f_CXZ (yi, j2, j, x(i, j2), x(i, j)) -= 1
            } // for
        } // for
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables from global frequencies.
     */
    override def reset ()
    {
        for (i <- (0 until k).par) {
            f_C(i) = g_f_C(i)
            for (j <- x.range2.par if fset(j); xj <- (0 until vc(j)).par) {
                if (i == 0) f_X(j, xj) = g_f_X(j, xj)
                f_CX(i, j, xj) = g_f_CX(i, j, xj)
                for (j2 <- (j+1 until n).par if fset(j2); xj2 <- (0 until vc(j2)).par) {
                    f_CXZ(i, j, j2, xj, xj2) = g_f_CXZ(i, j, j2, xj, xj2)
                    f_CXZ(i, j2, j, xj2, xj) = f_CXZ(i, j, j2, xj, xj2)
                } // for
            } // for
        } // for
    } // reset

} // TwoBAN_OS class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OS` object is the companion object for the `TwoBAN_OS` class.
 */
object TwoBAN_OS
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `TwoBAN_OS object, passing 'x' and 'y' together in one table.
     *  @param xy           the data vectors along with their classifications stored as rows of a matrix
     *  @param fn           the names for all features/variables
     *  @param k            the number of classes
     *  @param cn           the names for all classes
     *  @param vc           the value count (number of distinct values) for each feature
     *  @param me           use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres        the correlation threshold between 2 features for possible parent-child relationship
     *  @param PARALLELISM  the level of parallelism, the number of threads to use
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String], vc: Array [Int] = null,
               thres: Double = 0.0, me: Double = me_default, PARALLELISM: Int = Runtime.getRuntime().availableProcessors()) =
    {
        new TwoBAN_OS (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn,
            vc, thres, me, PARALLELISM)
    } // apply

} // TwoBAN_OS object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OSTest` object is used to test the `TwoBAN_OS` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-TwoBAN_OS-example.pdf
 *  > runMain scalation.analytics.classifier.par.TwoBAN_OSTest
 */
object TwoBAN_OSTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                x0 x1 x2
    val x = new MatrixI((10, 3), 1, 0, 1,                           // data matrix
                                 1, 0, 1,
                                 1, 0, 1,
                                 0, 0, 1,
                                 0, 0, 0,
                                 0, 1, 0,
                                 0, 1, 0,
                                 0, 1, 1,
                                 1, 1, 0,
                                 1, 0, 0)

    val y = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)                  // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin")                      // feature/variable names
    val cn = Array ("No", "Yes")                                    // class names

    println("xy = " + (x :^+ y))
    println("---------------------------------------------------------------")

    val twoban0 = new TwoBAN_OS0 (x, y, fn, 2, cn)                  // create the classifier
    val twoban  = new TwoBAN_OS  (x, y, fn, 2, cn)                  // create the classifier

    // train the classifier ---------------------------------------------------
    twoban0.train ()
    twoban.train ()

    //test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1)                                      // existing data vector to classify
    val z2 = VectorI (1, 1, 1)                                      // new data vector to classify
    println ("Use twoban0 to classify (" + z1 + ") = " + twoban0.classify (z1))
    println ("Use twoban  to classify (" + z1 + ") = " + twoban.classify (z1))
    println ("Use twoban0 to classify (" + z2 + ") = " + twoban0.classify (z2))
    println ("Use twoban  to classify (" + z2 + ") = " + twoban.classify (z2))

    println ("twoban0 cv accu = " + twoban0.crossValidateRand ())   // cross validate the classifier
    println ("twoban  cv accu = " + twoban.crossValidateRand ())    // cross validate the classifier

} // TwoBAN_OSTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OSTest2` object is used to test the `TwoBAN_OS` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > runMain scalation.analytics.classifier.par.TwoBAN_OSTest2
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

    val fn = Array ("Fast", "Strong")                               // feature names
    val cn = Array ("No", "Yes")                                    // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val twoban0 = TwoBAN_OS0 (xy, fn, 2, cn, null, 0)               // create the classifier
    val twoban  = TwoBAN_OS  (xy, fn, 2, cn, null, 0)               // create the classifier

    // train the classifier ---------------------------------------------------
    twoban0.train ()
    twoban.train ()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                          // new data vector to classify
    println ("Use twoban0 to classify (" + z + ") = " + twoban0.classify (z))
    println ("Use twoban  to classify (" + z + ") = " + twoban.classify (z))

    println ("twoban0 cv accu = " + twoban0.crossValidateRand ())   // cross validate the classifier
    println ("twoban  cv accu = " + twoban.crossValidateRand ())    // cross validate the classifier

} // TwoBAN_OSTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TwoBAN_OSTest3` object is used to test the `TwoBAN_OS` class.
 *  > runMain scalation.analytics.classifier.par.TwoBAN_OSTest3
 */
object TwoBAN_OSTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray.slice(0, xy.dim2 - 1)
    val cn = Array ("p", "e")                                       // class names
    val k  = 2

    val twoban0 = TwoBAN_OS0 (xy, fn, 2, cn, null, 0.1, 1)          // create the classifier
    val twoban  = TwoBAN_OS  (xy, fn, 2, cn, null, 0.1, 1)          // create the classifier

    println ("twoban0 cv accu = " + twoban0.crossValidateRand ())   // cross validate the classifier
    println ("twoban  cv accu = " + twoban.crossValidateRand ())    // cross validate the classifier

    twoban0.featureSelection ()
    twoban.featureSelection ()

    println ("twoban0 cv accu = " + twoban0.crossValidateRand ())   // cross validate the classifier
    println ("twoban  cv accu = " + twoban.crossValidateRand ())    // cross validate the classifier

} // TwoBAN_OSTest3 object

