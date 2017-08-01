
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

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
    /** Given a set of data vectors and their classifications, build a classifier.
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def train (testStart: Int, testEnd: Int)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors and their classifications, build a classifier.
     *  @param itest  the indices of the instances considered as testing data
     */
    def train (itest: IndexedSeq [Int]) {}

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors and their classifications, build a classifier.
     */
    def train () { train (0, 0) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, determine which class it belongs to,
     *  returning the best class, its name and its relative probability.
     *  @param z  the vector to classify
     */
    def classify (z: VectoI): (Int, String, Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, determine which class it belongs to,
     *  returning the best class, its name and its relative probability.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): (Int, String, Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def test (testStart: Int, testEnd: Int): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param itest  the indices of the instances considered test data
     */
    def test (itest: VectorI): Double = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Reset the frequency and probability tables.
     */
    def reset ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the accuracy of the classified results by cross-validation, returning
     *  the accuracy.  The "test data" starts at 'testStart' and ends at 'testEnd',
     *  the rest of the data is "training data'.
     *  @param nx  the number of crosses and cross-validations (defaults to 5x).
     */
    def crossValidate (nx: Int = 10): Double =
    {
        val testSize = size / nx
        var sum      = 0.0

        for (i <- 0 until nx) {
            val testStart = i * testSize
            val testEnd   = testStart + testSize
            train (testStart, testEnd)
            sum += test (testStart, testEnd)
        } // for

        val avg = sum / nx.toDouble
//      println ("Average accuracy = " + avg)
//      println ("------------------------------------------------------------")
        avg
    } // crossValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the accuracy of the classified results by cross-validation, returning
     *  the accuracy. This version of cross-validation relies on "subtracting"
     *  frequencies from the previously stored global data to achieve efficiency.
     *  @param nx  number of crosses and cross-validations (defaults to 10x).
     */
    def crossValidateRand (nx: Int = 10): Double =
    {
        var sum         = 0.0
        val permutedVec = PermutedVecI (VectorI.range(0, size), ranStream)
        val randOrder   = permutedVec.igen
        val itestA      = randOrder.split (nx)

        for (itest <- itestA) {
            train (itest())
            sum += test (itest)
        } // for

        sum / nx.toDouble
    } // crossValidateRand

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the feature set.
     */
    def size: Int
    
} // Classifier trait

