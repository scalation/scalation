
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author John Miller, Zhe Jin
 *  @version 1.2
 *  @date Sat Apr 30 13:53:16 EDT 2015
 *  @see LICENSE (MIT style license file).
 */

package scalation.analytics.classifier.par

import java.util.concurrent.ForkJoinPool

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ForkJoinTaskSupport

import scalation.linalgebra._
import scalation.linalgebra.gen.HMatrix3
import scalation.relalgebra.Relation

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SelNaiveBayes` class implements an Integer-Based Naive Bayes Classifier,
 *  which is a commonly used such classifier for discrete input data.  The
 *  classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier is naive, because it assumes feature independence and therefore
 *  simply multiplies the conditional probabilities.
 *  The version is "selective", since features whose impact is small are ignored.
 * ----------------------------------------------------------------------------
 *  @param x     the integer-valued data vectors stored as rows of a matrix
 *  @param y     the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn    the names for all features/variables
 *  @param k     the number of classes
 *  @param cn    the names for all classes
 *  @param fset  the array of selected features
 *  @param vc    the value count (number of distinct values) for each feature
 *  @param me    use m-estimates (me == 0 => regular MLE estimates)
 */
class SelNaiveBayes (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                    me: Int = 3, var fset: ListBuffer [Int] = null, private var vc: VectoI = null )
      extends BayesClassifier (x, y, fn, k, cn)
{
    private val DEBUG       = false                       // debug flag
    private val PARALLELISM = 12                          // parallelism level
    private val TOL         = 0.01                        // tolerance indicating negligible improvement adding features
    private val cor         = calcCorrelation             // feature correlation matrix

    private val popC  = new VectorI (k)                   // frequency counts for classes 0, ..., k-1
    private val probC = new VectorD (k)                   // probabilities for classes 0, ..., k-1
    private var popX:  HMatrix3 [Int] = null              // conditional frequency counts for variable/feature j
    private var probX: HMatrix3 [Double] = null           // conditional probabilities for variable/feature j

    if (vc == null) vc = vc_fromData                      // set to default for binary data (2)

    if (DEBUG) {
        println ("value count vc     = " + vc)
        println ("correlation matrix = " + cor)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select the features and save them into fset.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     */
    def buildModel (testStart: Int = 0, testEnd: Int = 0): (Array [Boolean], DAG) =
    {
        if (fset == null) {
            fset = ListBuffer [Int](); selectFeatures ()
        } // if
        (Array.fill (n)(true), new DAG(Array.ofDim [Int] (n, 0)))
    } // buildModel

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current size of the feature set (it varies as different
     *  features are selected).  If all features are selected 'nn = n'.
     */
    def nn = fset.size

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set/change the feature set to the given feature set.
     *  @param fset2  feature set to be changed
     */
    def setFset (fset2: ListBuffer [Int]) { fset = fset2 }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the accuracy of the classified results by cross-validation, returning
     *  the accuracy.  The "test data" starts at 'testStart' and ends at 'testEnd',
     *  the rest of the data is "training data'.
     *  @param nx     number of crosses and cross-validations (defaults to 5x).
     *  @param tfset  the array of selected features
     */
    def crossValidate (nx: Int, tfset: ListBuffer [Int]): Double =
    {
        val testSize = size / nx
        var sum = 0.0

        for (i <- 0 until nx) {
            val testStart = i * testSize
            val testEnd = testStart + testSize
            val tpopC = new VectorI (k)                           // frequency counts for classes 0, ..., k-1
            val tprobC = new VectorD (k)                          // probabilities for classes 0, ..., k-1
            val (tpopX, tprobX) = trainHelper (testStart, testEnd, tpopC, tprobC, tfset)
            sum += test (testStart, testEnd, tprobC, tprobX, tfset)
        } // for
        val avg = sum / nx.toDouble
        avg
    } // crossValidate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     *  @param tprobC     probabilities for classes 0, ..., k-1
     *  @param tprobX     conditional probabilities for variable/feature j
     *  @param tfset      the array of selected features
     */
    def test (testStart: Int, testEnd: Int, tprobC: VectorD, tprobX: HMatrix3 [Double], tfset: ListBuffer [Int]): Double =
    {
        var correct = 0
        for (i <- testStart until testEnd if classifyHelper (x (i), tprobC, tprobX, tfset)._1 == y (i)) correct += 1
        correct / (testEnd - testStart).toDouble
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Choose the next best feature, returning the next best feature and its score.
     *  FIX - don't hardcode threadpool size
     */
    private def nextBest (): (Int, Double) =
    {
        var maxscore = Array.ofDim [Double] (n)
        var jmax = -1
        val tempfeatures = Array.range (0, n).diff(fset)
        var temprange = (0 until tempfeatures.size).par
        temprange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))
        println("threads num = " + PARALLELISM)
        for (j <- temprange) {
            var tempfset = fset.clone ()
            tempfset += tempfeatures (j)
            val score = crossValidate (5, tempfset)
            if (DEBUG) println("j: " + j + " score: " + score)
            maxscore(j) = score
        } // for
        jmax = tempfeatures (maxscore.indexWhere (x => x == maxscore.max))
        if (DEBUG)
            println("add feature " + jmax + " to fset")
        (jmax, maxscore (jmax))
    } //nextBest

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select the best features by calling 'nextBest' until improvement is below
     *  the 'TOL' cutoff.
     */
    private def selectFeatures ()
    {
        var a = 0.0                                        // accumulated max score
        while (true) {
            val (j, m) = nextBest ()
            if (m - a > TOL) { a = m; fset += j } else return
        } // while
    } // selectFeatures

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     *  @param tpopC      frequency counts for classes 0, ..., k-1
     *  @param tpopX      conditional frequency counts for variable/feature j
     *  @param tfset      the array of selected features
     */
    private def frequencies (testStart: Int, testEnd: Int, tpopC: VectorI, tpopX: HMatrix3 [Int], tfset: ListBuffer [Int])
    {
        for (l <- 0 until m if l < testStart || l >= testEnd) {          // l = lth row of data matrix x
            val i = y (l)                                                 // get the class
            tpopC(i) += 1                                                 // increment ith class
            for (j <- 0 until tfset.size) tpopX (i, j, x(l, tfset (j))) += 1
        } // for
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     */
    def train (testStart: Int = 0, testEnd: Int = 0): Unit =
    {
        var (tpopX, tprobX) = trainHelper (testStart, testEnd, popC, probC, fset)
        popX = tpopX
        probX = tprobX
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     *  @param tpopC      frequency counts for classes 0, ..., k-1
     *  @param tprobC     probabilities for classes 0, ..., k-1
     *  @param tfset      the array of selected features
     */
    def trainHelper (testStart: Int = 0, testEnd: Int = 0, tpopC: VectorI, tprobC: VectorD, tfset: ListBuffer [Int]):
                    (HMatrix3 [Int], HMatrix3 [Double]) =
    {
        if (DEBUG) println ("train is using the following features " + fset)

        val tpopX  = new HMatrix3 [Int] (k, tfset.size)              // conditional frequency counts for variable/feature j
        val tprobX = new HMatrix3 [Double] (k, tfset.size)           // conditional probabilities for variable/feature j
        tpopX.alloc (vc.select (tfset.toArray)().toArray)            // allocate 3rd dimension
        tprobX.alloc (vc.select (tfset.toArray)().toArray)           // allocate 3rd dimension
        frequencies (testStart, testEnd, tpopC, tpopX, tfset)        // compute frequencies skipping test region

        for (i <- 0 until k) {                                       // for each class i
            val pci = tpopC(i).toDouble                              // population of class i
            tprobC(i) = pci / md                                     // probability of class i

            for (j <- 0 until tfset.size) {                          // for each feature j in fset
                val jj = tfset(j)                                    // jth feature in fset
                val me_vc = me / vc(jj).toDouble
                for (xj <- 0 until vc(jj)) {                         // for each value for feature j: xj
                    tprobX(i, j, xj) = (tpopX(i, j, xj) + me_vc) / (pci + me)
                } // for
            } // for
        } // for

        if (DEBUG) {
            println ("probC = " + tprobC)                            // P(C = i)
            println ("probX = " + tprobX)                            // P(X_j = x | C = i)
        } // if
        (tpopX, tprobX)
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        classifyHelper (z, probC, probX, fset)
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  @param z       the data vector to classify
     *  @param tprobC  probabilities for classes 0, ..., k-1
     *  @param tprobX  conditional probabilities for variable/feature j
     *  @param tfset   the array of selected features
     */
    def classifyHelper (z: VectoI, tprobC: VectorD, tprobX: HMatrix3 [Double], tfset: ListBuffer [Int]):
        (Int, String, Double) =
    {
        val prob = new VectorD (k)
        for (i <- 0 until k) {
            prob(i) = tprobC (i)                                                    // P(C = i)
            for (j <- 0 until tfset.size) prob (i) *= tprobX(i, j, z (tfset (j)))   // P(X_j = z_j | C = i)
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()                    // class with the highest relative posterior probability
        (best, cn (best), prob(best))                // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the population and probability vectors to 0
     *  and the hypermatrices to null.
     *  @param tpopC   frequency counts for classes 0, ..., k-1
     *  @param tprobC  probabilities for classes 0, ..., k-1
     */
    def resetHelper (tpopC: VectorI, tprobC: VectorD)
    {
        tpopC.set (0)
        tprobC.set (0)
    } // reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the population and probability vectors to 0
     *  and the hypermatrices to null.
     */
    def reset ()
    {
        resetHelper(popC, probC)
        popX = null
        probX = null
    } // reset

} // SelNaiveBayes class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `SelNaiveBayes` is the companion object for the `SelNaiveBayes` class.
 */
object SelNaiveBayes
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SelNaiveBayes` object, passing 'x' and 'y' together in one table.
     *  @param xy    the data vectors along with their classifications stored as rows of a matrix
     *  @param fn    the names of the features/variables
     *  @param k     the number of classes
     *  @param cn    the names for all classes
     *  @param fset  the array of selected features
     *  @param vc    the value count (number of distinct values) for each feature
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
              me: Int = 3, fset: ListBuffer [Int], vc: VectoI = null) =
    {
        new SelNaiveBayes (xy (0 until xy.dim1, 0 until xy.dim2 - 1), xy.col (xy.dim2 - 1), fn, k, cn,
                           me, fset, vc)
    } // apply

} // SelNaiveBayes object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SelNaiveBayesTest` object is used to test the `SelNaiveBayes` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > run-main scalation.analytics.classifier.par.SelNaiveBayesTest
 */
object SelNaiveBayesTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                 x0 x1 x2
    val x = new MatrixI ((10, 3), 1, 0, 1,                    // data matrix
                                  1, 0, 1,
                                  1, 0, 1,
                                  0, 0, 1,
                                  0, 0, 0,
                                  0, 1, 0,
                                  0, 1, 0,
                                  0, 1, 1,
                                  1, 1, 0,
                                  1, 0, 0)

    val y = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)             // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin")                 // feature/variable names
    val cn = Array ("No", "Yes")                               // class names
    val fset = null

    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    val snb = new SelNaiveBayes (x, y, fn, 2, cn, 1, null)     // create the classifier
    val snbstructure = snb.buildModel ()
    // train the classifier ---------------------------------------------------
    snb.train ()

    // test samples -----------------------------------------------------------
    val z1 = VectorI (1, 0, 1)                                 // existing data vector to classify
    val z2 = VectorI (1, 1, 1)                                 // new data vector to classify
    println("classify (" + z1 + ") = " + snb.classify (z1) + "\n")
    println("classify (" + z2 + ") = " + snb.classify (z2) + "\n")

    snb.crossValidate()                                        // cross validate the classifier

} // SelNaiveBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SelNaiveBayesTest2` object is used to test the `SelNaiveBayes` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > run-main scalation.analytics.classifier.par.SelNaiveBayesTest2
 */
object SelNaiveBayesTest2 extends App
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

    val fn = Array ("Fast", "Strong")
    val cn = Array ("No", "Yes")
    val fset = ListBuffer (1, 0)

    println ("xy = " + xy)
    println ("fset: " + fset)
    println ("--------------------------------------------------------------")

    val snb = SelNaiveBayes (xy, fn, 2, cn, 1, null)             // create the classifier

    // train the classifier ---------------------------------------------------
    val snbstructure = snb.buildModel ()
    snb.train ()

    snb.crossValidate ()                                         // cross validate the classifier

} // SelNaiveBayesTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SelNaiveBayesTest3` object is used to test the `SelNaiveBayes` class.
 *  > run-main scalation.analytics.classifier.par.SelNaiveBayesTest3
 */
object SelNaiveBayesTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1") // class names

    println ("---------------------------------------------------------------")
    val snb = SelNaiveBayes (xy, fn, 2, cn, 1, null)              // create the classifier
    snb.buildModel ()
    snb.train ()
    snb.crossValidate ()

} // SelNaiveBayesTest3 object

