
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{VectoD, VectorI}

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

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier, i.e., calculate statistics and create conditional
     *  density (cd) functions.  Assumes that conditional densities follow the
     *  Normal (Gaussian) distribution.
     */
    def train () { train (0, 0) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, determine which class it belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectorI): Tuple2 [Int, String]

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
    /**  Reset the frequency and probability tables.
     */
    def reset ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the accuaracy of the classified results by crossvalidation, returning
     *  the accuaracy.  The "test data" starts at 'testStart' and ends at 'testEnd',
     *  the rest of the data is "training data'.
     *  @param nx  number of crosses and crossvalidations (defaults to 5x).
     */
    def crossValidate (nx: Int = 5): Double =
    {
        println ("------------------------------------------------------------")
        println ("cross-validation:")

        val testSize = size / nx
        var sum      = 0.0

        for (i <- 0 until nx) {
            val testStart = i * testSize
            val testEnd   = testStart + testSize
            reset ()
//          println (s"testStart = $testStart, testEnd = $testEnd, testSize = $testSize)
            train (testStart, testEnd)
            sum += test (testStart, testEnd)
        } // for

        val avg = sum / nx.toDouble
        println ("Average accuracy = " + avg)
        println ("------------------------------------------------------------")
        avg
    } // crossValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Size of the feature set.
     */
    def size: Int
    
} // Classifier trait

