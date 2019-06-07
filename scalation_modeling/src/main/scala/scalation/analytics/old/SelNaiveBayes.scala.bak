
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhe Jin
 *  @version 1.2
 *  @date    Sat Apr 30 13:53:16 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.ListBuffer

import scalation.linalgebra.{MatrixI, VectorD, VectorI}
import scalation.linalgebra.gen.HMatrix3

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
 *----------------------------------------------------------------------------
 *  @param x     the integer-valued data vectors stored as rows of a matrix
 *  @param y     the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn    the names for all features/variables
 *  @param k     the number of classes
 *  @param cn    the names for all classes
 *  @param fset  the array of selected features
 *  @param vc    the value count (number of distinct values) for each feature
 *  @param me    use m-estimates (me == 0 => regular MLE estimates)
 */
class SelNaiveBayes (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
                     private var fset: ListBuffer [Int] = null, private var vc: VectorI = null, me: Int = 3)
        extends ClassifierInt (x, y, fn, k, cn)
{
    private val DEBUG = true                           // debug flag
    private val TOL   = 0.1                            // tolerance indicating negligible improvement adding features
    private val cor   = calcCorrelation                // feature correlation matrix

    private val popC  = new VectorI (k)                // frequency counts for classes 0, ..., k-1
    private val probC = new VectorD (k)                // probabilities for classes 0, ..., k-1
    private var popX:  HMatrix3 [Int]    = null        // conditional frequency counts for variable/feature j
    private var probX: HMatrix3 [Double] = null        // conditional probabilities for variable/feature j

    if (vc == null) vc = vc_default                    // set to default for binary data (2)
    if (fset == null) { fset = ListBuffer [Int] ();  selectFeatures () }

    if (DEBUG) {
        println ("value count vc     = " + vc)
        println ("correlation matrix = " + cor)
    } // if

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
    /** Choose the next best feature, returning the next best feature and its score.
     */
    def nextBest (): (Int, Double) =
    {
        var maxscore = 0.0
        var jmax     = -1
        for (j <- 0 until n if ! (fset contains j)) {
            fset += j
            val score = crossValidate (5)
            if (DEBUG) println ("j: " + j + " score: " + score)
            if (score > maxscore) { maxscore = score; jmax = j }
            fset -= j
        } // for
        if (DEBUG) println ("add feature " + jmax + " to fset")
        (jmax, maxscore)
    } //nextBest

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select the best features by calling 'nextBest' until improvement is below
     *  the 'TOL' cutoff.
     */
    def selectFeatures ()
    {
        var a = 0.0                                   // accumulated max score
        while (true) {
            val (j, m) = nextBest ()
            if (m - a > TOL) { a = m; fset += j } else return
        } // while
    } // selectFeatures

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param testStart  beginning of test region. (inclusive)
     *  @param testEnd    end of test region. (exclusive)
     */
    def frequencies (testStart: Int, testEnd: Int)
    {
        for (l <- 0 until m if l < testStart || l >= testEnd) {    // l = lth row of data matrix x
            val i = y(l)                                           // get the class
            popC(i) += 1                                           // increment ith class
            for (j <- 0 until nn) popX(i, j, x(l, fset(j))) += 1
        } // for

        if (DEBUG) {
            println ("popC = " + popC)                             // #(C = i)
            println ("popX = " + popX)                             // #(X_j = x & C = i)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     */
    def train (testStart: Int = 0, testEnd: Int = 0)
    {
        if (DEBUG) println("train is using the following features " + fset)

        popX  = new HMatrix3 [Int] (k, nn)                         // conditional frequency counts for variable/feature j
        probX = new HMatrix3 [Double] (k, nn)                      // conditional probabilities for variable/feature j
        println ("fset = " + fset)
        println ("vc = " + vc)
        popX.alloc (vc.select (fset.toArray)().toArray)            // allocate 3rd dimension
        probX.alloc (vc.select (fset.toArray)().toArray)           // allocate 3rd dimension

        frequencies (testStart, testEnd)                           // compute frequencies skipping test region

        for (i <- 0 until k) {                                     // for each class i
            val pci = popC(i).toDouble                             // population of class i
            probC(i) = pci / md                                    // probability of class i

            for (j <- 0 until nn) {                                // for each feature j in fset
                val jj    = fset(j)                                // jth feature in fset
                val me_vc = me / vc(jj).toDouble
                for (xj <- 0 until vc(jj)) {                       // for each value for feature j: xj
                    probX(i, j, xj) = (popX(i, j, xj) + me_vc) / (pci + me)
                } // for
            } // for
        } // for

        if (DEBUG) {
            println ("probC = " + probC)                           // P(C = i)
            println ("probX = " + probX)                           // P(X_j = x | C = i)
        } // if
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     * (0, ..., k-1) with the highest relative posterior probability.
     * @param z  the data vector to classify
     */
    def classify (z: VectorI): (Int, String) =
    {
        val prob = new VectorD (k)
        for (i <- 0 until k) {
            prob(i) = probC(i)                                        // P(C = i)
            for (j <- 0 until nn) prob(i) *= probX(i, j, z(fset(j)))  // P(X_j = z_j | C = i)
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()             // class with the highest relative posterior probability
        (best, cn(best))                      // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the population and probability vectors to 0
     *  and the hypermatrices to null.
     */
    def reset()
    {
        popC.set (0)
        probC.set (0)
        popX  = null
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
    def apply (xy: MatrixI, fn: Array [String], k: Int, cn: Array [String],
               fset: ListBuffer [Int], vc: VectorI = null, me: Int = 3) =
    {
        new SelNaiveBayes (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn,
                           fset, vc, me)
    } // apply

} // SelNaiveBayes object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SelNaiveBayesTest` object is used to test the `SelNaiveBayes` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > run-main scalation.analytics.SelNaiveBayesTest
 */
object SelNaiveBayesTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                 x0 x1 x2
    val x = new MatrixI ((10, 3), 1, 0, 1,              // data matrix
                                  1, 0, 1,
                                  1, 0, 1,
                                  0, 0, 1,
                                  0, 0, 0,
                                  0, 1, 0,
                                  0, 1, 0,
                                  0, 1, 1,
                                  1, 1, 0,
                                  1, 0, 0)

    val y    = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)   // classification vector: 0(No), 1(Yes))
    val fn   = Array ("Color", "Type", "Origin")        // feature/variable names
    val cn   = Array ("No", "Yes")                      // class names
    val fset = ListBuffer (0, 1, 2)

    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    val snb = new SelNaiveBayes(x, y, fn, 2, cn, fset) // create the classifier            

    // train the classifier ---------------------------------------------------
    snb.train ()

    // test samples -----------------------------------------------------------
    val z1 = VectorI (1, 0, 1)              // existing data vector to classify
    val z2 = VectorI (1, 1, 1)              // new data vector to classify
    println ("classify (" + z1 + ") = " + snb.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + snb.classify (z2) + "\n")

    // cross validate the classifier
    snb.crossValidate ()

} // SelNaiveBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SelNaiveBayesTest2` object is used to test the `SelNaiveBayes` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > run-main scalation.analytics.SelNaiveBayesTest2
 */
object SelNaiveBayesTest2 extends App
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

    val fn   = Array ("Fast", "Strong")
    val cn   = Array ("No", "Yes")
    val fset = ListBuffer (1, 0)

    println ("xy = " + xy)
    println ("fset: " + fset)
    println ("--------------------------------------------------------------")

    val snb = SelNaiveBayes (xy, fn, 2, cn, fset)     // create the classifier            

    // train the classifier ---------------------------------------------------
    snb.selectFeatures ()
    snb.train ()

    // println("crossValidate: " + bnb.crossValidate())

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                       // new data vector to classify
    println ("classify (" + z + ") = " + snb.classify (z) + "\n")

    // cross validate the classifier
    snb.crossValidate ()

} // SelNaiveBayesTest2 object

