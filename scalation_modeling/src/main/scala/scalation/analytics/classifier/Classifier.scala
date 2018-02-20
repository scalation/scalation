
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math.round

import scalation.linalgebra.{VectoD, VectoI, VectorI}
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Classifier` trait provides a common framework for several classifiers.
 *  A classifier is for bounded responses.  When the number of distinct responses
 *  cannot be bounded by some integer 'k', a predictor should be used.
 */
trait Classifier
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of data vectors/points in the entire dataset (training + testing),
     */
    def size: Int                                             // typically = m

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities from a training dataset of
     *  data vectors and their classifications.  The indices for the testing dataset
     *  are given and the training dataset consists of all the other instances.
     *  Must be implemented in any extending class.
     *  @param itest  the indices of the instances considered as testing data
     */
    def train (itest: IndexedSeq [Int]): Classifier

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities from a training dataset of
     *  data vectors and their classifications.  Must be implemented in any extending class.
     *  Can be used when the dataset is randomized so that the training part of a dataset
     *  corresponds to simple slices of vectors and matrices.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def train (testStart: Int, testEnd: Int): Classifier = train (testStart until testEnd)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities from a training dataset of
     *  data vectors and their classifications.  Must be implemented in any extending class.
     *  Can be used when the whole dataset is used for training.
     */
    def train (): Classifier = train (0, 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector 'z', determine which class it fits into,
     *  returning the best class, its name and its relative probability.
     *  @param z  the integer vector to classify
     */
    def classify (z: VectoI): (Int, String, Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector 'z', determine which class it fits into,
     *  returning the best class, its name and its relative probability.
     *  Override in classes that require precise real values for classification.
     *  @param z  the real vector to classify
     */
    def classify (z: VectoD): (Int, String, Double) =
    {
        val zi = new VectorI (z.dim, z.map (round (_).toInt).toArray)
        classify (zi)
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test dataset and return the fraction
     *  of correct classifications.
     *  @param itest  the indices of the instances considered test data
     */
    def test (itest: IndexedSeq [Int]): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test dataset and return the fraction
     *  of correct classifications.  Can be used when the dataset is randomized
     *  so that the testing/training part of a dataset corresponds to simple slices
     *  of vectors and matrices.
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def test (testStart: Int, testEnd: Int): Double = test (testStart until testEnd)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the accuracy of the classified results by cross-validation, returning
     *  the accuracy.  The "test data" starts at 'testStart' and ends at 'testEnd',
     *  the rest of the data is "training data'.
     *  FIX - should return a StatVector
     *  @param nx    the number of crosses and cross-validations (defaults to 10x).
     *  @param show  the show flag (show result from each iteration)
     */
    def crossValidate (nx: Int = 10, show: Boolean = false): Double =
    {
        val testSize = size / nx                                 // number of instances in test set
        var sum      = 0.0
        for (it <- 0 until nx) {
            val testStart = it * testSize                        // test set start index (inclusive)
            val testEnd   = testStart + testSize                 // test set end index (exclusive)
            train (testStart, testEnd)                           // train on opposite instances
            val acc = test (testStart, testEnd)                  // test on test set
            if (show) println (s"crossValidate: for it = $it, acc = $acc")
            sum += acc                                           // accumulate accuracy
        } // for
        sum / nx.toDouble                                        // return average accuracy
    } // crossValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the accuracy of the classified results by cross-validation, returning
     *  the accuracy.  This version of cross-validation relies on "subtracting"
     *  frequencies from the previously stored global data to achieve efficiency.
     *  FIX - are the comments correct?
     *  FIX - should return a StatVector
     *  @param nx    number of crosses and cross-validations (defaults to 10x).
     *  @param show  the show flag (show result from each iteration)
     */
    def crossValidateRand (nx: Int = 10, show: Boolean = false): Double =
    {
        val permutedVec = PermutedVecI (VectorI.range (0, size), ranStream)
        val randOrder   = permutedVec.igen                       // randomize integers 0 until size
        val itestA      = randOrder.split (nx)                   // make array of itest indices
        var sum         = 0.0
        for (it <- 0 until nx) {
            val itest = itestA(it)()                             // get array from it element
            train (itest)                                        // train on opposite instances
            val acc = test (itest)                               // test on test set
            if (show) println (s"crossValidateRand: for it = $it, acc = $acc")
            sum += acc                                           // accumulate accuracy
        } // for
        sum / nx.toDouble                                        // return average accuracy
    } // crossValidateRand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Reset the frequency counters.
     */
    def reset ()

} // Classifier trait

