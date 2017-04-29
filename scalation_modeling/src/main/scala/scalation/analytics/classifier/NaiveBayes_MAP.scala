
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhe Jin
 *  @version 1.3
 *  @date    Tue Oct 25 14:57:18 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see classes.engr.oregonstate.edu/eecs/spring2012/cs534/notes/Naivebayes-6.pdf
 */

package scalation.analytics.classifier

import scalation.linalgebra.{MatriI, VectoI, VectorD, VectorI}
import scalation.linalgebra.gen.HMatrix3
import scalation.relalgebra.Relation
import scalation.util.{banner, time}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesMAP` class implements an Integer-Based Naive Bayes Classifier,
 *  which is a commonly used such classifier for discrete input data.
 *  Determine the response 'y = f(x)' that maximizes the following probability:
 *  <p>
 *      y = argmax [ P(y) ∏ P(xi | y) ]
 *  <p>
 *  Given several instances, the classifier is trained using a data matrix 'x' and
 *  a classification vector 'y'.  Each data (row) vector in the matrix is classified
 *  into one of 'k' classes numbered 0, ..., k-1.  Probabilities for 'y' ('P(y)') are
 *  estimated based on frequency counts for each class (y-value) in the training-set.
 *  Relative posterior probabilities are computed by multiplying these values by
 *  estimated conditional probabilities 'P(xi | y)'.
 *  -----------------------------------------------------------------------------
 *  The classifier is naive, because it assumes feature independence and therefore
 *  simply multiplies the conditional probabilities.
 *  <p>
 *      y = argmax [ P(y) ∏ P(xi | y)
 *  <p>
 *  Probabilities are estimated using a Bayesian (Maximum a Posterior) MAP procedure.
 *  -----------------------------------------------------------------------------
 *  @param x   the integer-valued data vectors stored as rows of a matrix
 *  @param y   the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 *  @param vc  the value count (number of distinct values) for each feature
 */
class NaiveBayesMAP (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                  private var vc: VectoI = null)
      extends BayesClassifier (x, y, fn, k, cn)
{
    private val DEBUG = true                               // debug flag
    private val cor   = calcCorrelation                    // feature correlation matrix

    private val f_Y  = new VectorI (k)                     // frequency counts for class yi
    private val f_YX = new HMatrix3 [Int] (k, n)           // frequency counts for class yi & feature xj

    private var p_Y: VectorD = _                           // probabilities for class yi
    private val p_X_Y = new HMatrix3 [Double] (k, n)       // conditional probabilities for feature xj given class yi

    if (vc == null) {
        shiftToZero; vc = vc_fromData                      // set value counts from data
    } // if
    f_YX.alloc (vc().toArray)
    p_X_Y.alloc (vc().toArray)

    if (DEBUG) {
        println ("distinct value count vc = " + vc)
        println ("correlation matrix      = " + cor)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a model.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def buildModel (testStart: Int, testEnd: Int): (Array [Boolean], DAG) =
    {
        (Array.fill (n)(true), new DAG (Array.ofDim [Int] (n, 0)))
    } // buildModel

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment frequency counters based on the 'i'th row of the data matrix.
     *  @param i  the index for current data row
     */
    private def increment (i: Int)
    {
        val yi   = y(i)                                       // get the class for ith row
        f_Y(yi) += 1                                          // increment frequency for class yi
        for (j <- 0 until n) f_YX(yi, j, x(i, j)) += 1        // increment frequency for class yi and all X-features
    } // increment

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'yi' and value 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    private def frequencies (testStart: Int, testEnd: Int)
    {
        if (DEBUG) banner ("frequencies (testStart, testEnd)")
        for (i <- 0 until m if i < testStart || i >= testEnd) increment (i)

        if (DEBUG) {
            println ("f_Y  = " + f_Y)                         // #(Y = yi)
            println ("f_YX = " + f_YX)                        // #(Y = yi & X_j = x)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'yi' and value 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param itrain  indices of the instances considered train data
     */
    private def frequencies (itrain: Array [Int])
    {
        if (DEBUG) banner ("frequencies (itrain)")
        for (i <- itrain) increment (i)

        if (DEBUG) {
            println ("f_Y  = " + f_Y)                         // #(Y = yi)
            println ("f_YX = " + f_YX)                        // #(Y = yi & X_j = x)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for Y, and the
     *  conditional probabilities for X_j.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int)
    {
        frequencies (testStart, testEnd)                      // compute frequencies skipping test region
        if (DEBUG) banner ("train (testStart, testEnd)")
        train2 ()
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for Y, and the
     *  conditional probabilities for X_j.
     *  @param itrain indices of the instances considered train data
     */
    override def train (itrain: Array [Int])
    {
        frequencies (itrain)                                  // compute frequencies skipping test region
        if (DEBUG) banner ("train (itrain)")
        train2 ()
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for Y, and the
     *  conditional probabilities for X_j.
     */
    private def train2 ()
    {
        p_Y = (f_Y + 1).toDouble / (md + k)                   // probability for class yi
        for (i <- 0 until k; j <- 0 until n) {                // for each class yi & feature xj
            val vc_j = vc(j)                                  // value count for xj
            for (xj <- 0 until vc_j) {                        // for each value for feature j: xj
                p_X_Y(i, j, xj) = (f_YX(i, j, xj) + 1) / (f_Y(i) + vc_j.toDouble)
            } // for
        } // for

        if (DEBUG) {
            println ("p_Y   = " + p_Y)                        // P(Y = yi)
            println ("p_X_Y = " + p_X_Y)                      // P(X_j = x | Y = yi)
        } // if
    } // train2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        if (DEBUG) banner ("classify (z)")
        val prob = new VectorD (p_Y)
        for (i <- 0 until k) {
            for (j <- 0 until n) prob(i) *= p_X_Y(i, j, z(j))   // P(X_j = z_j | Y = i)
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()                    // class with the highest relative posterior probability
        (best, cn(best), prob(best))                 // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the population and probability vectors and
     *  hypermatrices to 0.
     */
    def reset ()
    {
        f_Y.set (0)
        f_YX.set (0)
        p_Y.set (0)
        p_X_Y.set (0)
    } // reset

} // NaiveBayesMAP class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `NaiveBayesMAP` is the companion object for the `NaiveBayesMAP` class.
 */
object NaiveBayesMAP
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NaiveBayesMAP` object, passing 'x' and 'y' together in one matrix.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names of the features
     *  @param k   the number of classes
     *  @param vc  the value count (number of distinct values) for each feature
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI = null) =
    {
        new NaiveBayesMAP (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn)
    } // apply

} // NaiveBayesMAP object

import scalation.linalgebra.MatrixI

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTestMAP` object is used to test the `NaiveBayesMAP` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > run-main scalation.analytics.classifier.NaiveBayesMAPTest
 */
object NaiveBayesMAPTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // x3: Mpg:     High (1), Low (0)
    // features:                 x0 x1 x2 x3
    val x = new MatrixI ((10, 4), 1, 0, 1, 1,               // data matrix
                                  1, 0, 1, 0,
                                  1, 0, 1, 1,
                                  0, 0, 1, 1,
                                  0, 0, 0, 1,
                                  0, 1, 0, 0,
                                  0, 1, 0, 0,
                                  0, 1, 1, 1,
                                  1, 1, 0, 0,
                                  1, 0, 0, 0)

    val y  = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)         // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin", "Mpg")       // feature/variable names
    val cn = Array ("No", "Yes")                            // class names

    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    val nb = new NaiveBayesMAP (x, y, fn, 2, cn)               // create the classifier            

    // train the classifier ---------------------------------------------------
    nb.train()

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1, 1)                           // existing data vector to classify
    val z2 = VectorI (1, 1, 1, 0)                           // new data vector to classify
    println ("classify (" + z1 + ") = " + nb.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + nb.classify (z2) + "\n")

//  nb.crossValidateRand ()                                 // cross validate the classifier

} // NaiveBayesMAPTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesMAPTest2` object is used to test the 'NaiveBayesMAP' class.
 *  Given whether a person is Fast and/or Strong, classify them as making Y = 1
 *  or not making Y = 0 the football team.
 *  > run-main scalation.analytics.classifier.NaiveBayesMAPTest2
 */
object NaiveBayesMAPTest2 extends App
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

    val fn = Array ("Fast", "Strong")                     // feature names
    val cn = Array ("No", "Yes")                          // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val nb = NaiveBayesMAP (xy, fn, 2, cn, null)          // create the classifier

    // train the classifier ---------------------------------------------------
    nb.train ()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                // new data vector to classify
    println ("classify (" + z + ") = " + nb.classify (z) + "\n")

    nb.crossValidate ()                                   // cross validate the classifier

} // NaiveBayesMAPTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesMAPTest3` object is used to test the 'NaiveBayesMAP' class.
 *  > run-main scalation.analytics.classifier.NaiveBayesMAPTest3
 */
object NaiveBayesMAPTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1")                              // class names
    val nb = NaiveBayesMAP (xy, fn, 2, cn, null)           // create the classifier
    nb.train ()
    nb.crossValidate ()

} // NaiveBayesMAPTest3 object

