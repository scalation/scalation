
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.math.round
import scala.collection.mutable.{LinkedHashMap, Map, Set}

import scalation.linalgebra.{MatriI, MatrixI, VectoD, VectorD, VectoI, VectorI}
import scalation.random.{PermutedVecI, RandomSet}
import scalation.random.RNGStream.ranStream

import Round.roundVec

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Classifier` trait provides a common framework for several classifiers.
 *  A classifier is for bounded responses.  When the number of distinct responses
 *  cannot be bounded by some integer 'k', a predictor should be used.
 */
trait Classifier
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of data vectors/points in the entire dataset (training + testing),
     */
    def size: Int                                             // typically = m

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities from a training dataset of
     *  data vectors and their classifications.  The indices for the testing dataset
     *  are given and the training dataset consists of all the other instances.
     *  Must be implemented in any extending class.
     *  @param itest  the indices of the instances considered as testing data
     */
    def train (itest: Ints): Classifier

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities from a training dataset of
     *  data vectors and their classifications.  Must be implemented in any extending class.
     *  Can be used when the dataset is randomized so that the training part of a dataset
     *  corresponds to simple slices of vectors and matrices.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def train (testStart: Int, testEnd: Int): Classifier = train (testStart until testEnd)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities from a training dataset of
     *  data vectors and their classifications.  Must be implemented in any extending class.
     *  Can be used when the whole dataset is used for training.
     */
    def train (): Classifier = train (0, 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector 'z', determine which class it fits into,
     *  returning the best class, its name and its relative probability.
     *  @param z  the integer vector to classify
     */
    def classify (z: VectoI): (Int, String, Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector 'z', determine which class it fits into,
     *  returning the best class, its name and its relative probability.
     *  @param z  the real vector to classify
     */
    def classify (z: VectoD): (Int, String, Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test dataset and return the fraction
     *  of correct classifications.
     *  @param itest  the indices of the instances considered test data
     */
    def test (itest: Ints): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test dataset and return the fraction
     *  of correct classifications.  Can be used when the dataset is randomized
     *  so that the testing/training part of a dataset corresponds to simple slices
     *  of vectors and matrices.
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def test (testStart: Int, testEnd: Int): Double = test (testStart until testEnd)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'acc', 'prec', 'recall', 'kappa'.
     *  Override to add more quality of fit measures.
     *  @see medium.com/greyatom/performance-metrics-for-classification-problems-in-machine-learning-part-i-b085d432082b
     *  @see `ConfusionMat`
     *  @param y   the actual class labels
     *  @param yp  the predicted class labels
     *  @param k   the number of class labels
     */
    def fit (y: VectoI, yp: VectoI, k: Int = 2): VectoD =
    {
        val cm  = new ConfusionMat (y, yp, k)                    // confusion matrix
        val p_r = cm.prec_recl                                   // precision and recall
        VectorD (cm.accuracy, p_r._3, p_r._4, cm.kappa)
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.  Override when necessary.
     */
    def fitLabel: Seq [String] = Seq ("acc", "prec", "recall", "kappa")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Format a double value.
     *  @param z  the double value to format
     */
    private def f_ (z: Double): String = "%.5f".format (z)

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a map of quality of fit measures (use of `LinedHashMap` makes it ordered).
     *  Override to add more quality of fit measures.
     *  @param y   the actual class labels
     *  @param yp  the predicted class labels
     *  @param k   the number of class labels
     */
    def fitMap (y: VectoI, yp: VectoI, k: Int = 2): Map [String, String] =
    {
        val lm = LinkedHashMap [String, String] ()               // empty list map
        val fl = fitLabel                                        // fit labels
        val fv = fit (y, yp, k)                                  // fit values
        for (i <- fl.indices) lm += fl(i) -> f_(fv(i))
        lm
    } // fitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the frequency counters.
     */
    def reset ()

} // Classifier trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Classifier` object provides methods for paritioning the downsampling the
 *  the dataset.
 */
object Classifier
{
    private val DEBUG = true                            // debug flag

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Partition the dataset into groups, e.g., to set up for downsampling, by
     *  returning each group's indices and frequency counts.  Instances with the
     *  same classification 'y(i)' will be found in the 'i'th group.
     *  @param y  the classification/response vector
     */
    def partition (y: VectoI): (Array [Set [Int]], VectoI) =
    {
        val k = y.max () + 1                            // number of class labels
        val group = Array.fill (k)(Set [Int] ())        // create k empty groups
        for (i <- y.range) group(y(i)) += i             // add index i into group y(i)
        val freq = VectorI (group.map (_.size))         // get the frequency for each group
        (group, freq)
    } // partition

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Downsample to reduce imbalance of classes, by returning the group indices
     *  and the probability for each group.
     *  @param y   the classification/response vector
     *  @param ns  the number of instances in downsample
     */
    def downsample (y: VectoI, ns: Int): Array [Int] =
    {
        val dsample = Set [Int] ()                      // create an empty downsample
        val (group, freq) = partition (y)               // partition into groups
        val gmax = freq.min () - 1                      // use smallest group for samples per group
        if (DEBUG) println (s"downsample: collect samples in range 0 to $gmax per group")
        val rsg   = RandomSet (gmax, gmax)              // create a random set generator
        for (ig <- group.indices) {
            val idx    = rsg.igen                       // randomly select indices in group
            val groupi = group(ig).toArray              // make corresponding array
            for (j <- idx) dsample += groupi(j)         // add selected ones to dsample
        } // for
        if (DEBUG) println (s"downsample: dsample = $dsample")
        dsample.toArray                                 // indices for y in downsample
    } // downsample

} // Classifier object

