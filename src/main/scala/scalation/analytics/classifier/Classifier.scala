
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import java.util.Random

import scalation.linalgebra.{VectoD, VectoI, VectorI}
import scalation.random.RandomVecI

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Classifier` trait provides a common framework for several classifiers.
 *  A classifier is for bounded responses.  When the number of distinct responses
 *  cannot be bounded by some integer 'k', a predictor should be used.
 */
trait Classifier
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors and their classifications, build a classifier.
     *  @param testStart Beginning of test region. (inclusive)
     *  @param testEnd   End of test region. (exclusive)
     */
    def train (testStart: Int, testEnd: Int)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors and their classifications, build a classifier.
     *  @param itrain indices of the instances considered train data
     */
    def train (itrain: Array [Int]){}

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier, i.e., calculate statistics and create conditional
     *  density 'cd' functions.  Assumes that conditional densities follow the
     *  Normal (Gaussian) distribution.
     */
    def train () { train (0, 0) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, determine which class it belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectoI): Tuple2 [Int, String]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, determine which class it belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): Tuple2 [Int, String]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param testStart Beginning of test region. (inclusive)
     *  @param testEnd   End of test region. (exclusive)
     */
    def test (testStart: Int, testEnd: Int): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param itest indices of the instances considered test data
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
     *  @param nx  number of crosses and cross-validations (defaults to 5x).
     */
    def crossValidate (nx: Int = 5): Double =
    {
        val testSize = size / nx
        var sum      = 0.0

        for (i <- 0 until nx) {
            val testStart = i * testSize
            val testEnd   = testStart + testSize
            reset ()
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
     *  the accuracy.  The "test data" starts at 'testStart' and ends at 'testEnd',
     *  the rest of the data is "training data'.
     *  @param nx  number of crosses and cross-validations (defaults to 5x).
     */
    def crossValidateRand (nx: Int = 5): Double =
    {
        val testSize = size / nx
        var sum      = 0.0
        val rng = new Random ()
        val rvg = RandomVecI (testSize, size-1, stream = rng.nextInt (1000) )
        for (i <- 0 until nx) {
            val itest = rvg.igen
            reset ()
            val itrain = Array.range (0, size) diff itest()
            train (itrain)
            sum += test (itest)
        } // for

        val avg = sum / nx.toDouble
//      println ("Average accuracy = " + avg)
//      println ("------------------------------------------------------------")
        avg
    } // crossValidateRand

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Size of the feature set.
     */
    def size: Int
    
} // Classifier trait

