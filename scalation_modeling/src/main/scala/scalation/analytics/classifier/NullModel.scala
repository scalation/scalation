
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Fri Feb 16 16:14:34 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scalation.linalgebra.{VectoD, VectorD, VectoI, VectorI}
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NullModel` class implements an Integer-Based Null Model Classifier,
 *  which is a simple classifier for discrete input data.  The classifier is trained
 *  just using a classification vector 'y'.
 *  Each data instance is classified into one of 'k' classes numbered 0, ..., k-1.
 *  @param y   the class vector, where y(i) = class for instance i
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
class NullModel (y: VectoI, k: Int, cn: Array [String])
      extends Classifier
{
    private val DEBUG = true                                 // debug flag
    private val m     = y.dim                                // number of instance
    private val nu_y  = new VectorD (k)                      // frequency counts for y-values
    private var pi_y: VectoD = null                          // probability estimates for y-values

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of data vectors/points in the entire dataset (training + testing),
     */
    def size: Int = m

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param itest  indices of the instances considered as testing data
     */
    def train (itest: IndexedSeq [Int]): NullModel =
    {
        val idx = 0 until m diff itest                       // training data set - opposite of tesing
        reset ()                                             // reset counter
        for (i <- idx) nu_y(y(i)) += 1                       // tally frequency counts
        pi_y = nu_y / idx.size.toDouble                      // probability vector for class y
        if (DEBUG) println (s" nu_y = $nu_y \n pi_y = $pi_y")
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        val best = pi_y.argmax ()                            // class with the highest  probability
        (best, cn(best), pi_y(best))                         // return the best class and its name
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a testiing dataset and return the
     *  fraction of correct classifications.
     *  @param itest  indices of the instances considered test data
     */
    def test (itest: IndexedSeq [Int]): Double =
    {
        var correct = 0
        val c = classify (VectorI (1))._1                    // decision won't change
        for (i <- itest if c == y(i)) correct += 1
        correct / itest.size.toDouble
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the frequency counters.
     */
    def reset () { nu_y.set (0) }

} // NullModel class

import scalation.linalgebra.MatrixI

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NullModelTest` object is used to test the `NullModel` class.
 *  Classify whether to play tennis(1) or not (0).
 *  > runMain scalation.analytics.classifier.NullModelTest
 */
object NullModelTest extends App
{
    import ExampleTennis._

    banner ("Tennis Example")
    val y = xy.col (4)                                                  // 4th column
    println ("y = " + y)
    println ("-" * 60)

    val nm = new NullModel (y, k, cn)                                   // create a classifier
    nm.train ()                                                         // train the classifier

    val z = VectorI (1)                                                 // new data vector to classify
    println (s"classify ($z) = nm.classify (z)")

    println (s"nm.crossValidateRand () = ${nm.crossValidateRand ()}")   // cross-validation

} // NullModelTest object

