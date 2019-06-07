
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Feb 16 16:14:34 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scalation.linalgebra.{MatriD, MatriI, VectoD, VectorD, VectoI, VectorI}
import scalation.util.banner

import Probability.{frequency, toProbability}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NullModel` class implements an Integer-Based Null Model Classifier,
 *  which is a simple classifier for discrete input data.  The classifier is trained
 *  just using a classification vector 'y'.
 *  Each data instance is classified into one of 'k' classes numbered 0, ..., k-1.
 *  @param y    the classification vector, where y(i) = class for instance i
 *  @param k    the number of classes
 *  @param cn_  the names for all classes
 */
class NullModel (y: VectoI, k: Int = 2, cn_ : Strings = null)
      extends ClassifierInt (null, y, null, k, cn_)
{
    private val DEBUG = true                                 // debug flag
    private var nu_y: VectoI = null                          // frequency counts for y-values
    private var p_y:  VectoD = null                          // probability estimates for y-values

    if (cn.length != k) flaw ("constructor", "# class names != # classes")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for y, and the
     *  conditional probabilities for X_j.
     *  @param itest  indices of the instances considered as testing data
     */
    def train (itest: Ints): NullModel =
    {
        val idx = 0 until m diff itest                       // training dataset - opposite of tesing
        nu_y = frequency (y, k, idx)                         // frequency vector for y
        p_y  = toProbability (nu_y, idx.size)                // probability vector for y
        if (DEBUG) println (s" nu_y = $nu_y \n p_y = $p_y")
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
        val best = p_y.argmax ()                            // class with the highest  probability
        (best, cn(best), p_y(best))                         // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify all of the row vectors in matrix 'xx'.
     *  @param xx  the row vectors to classify
     */
    override def classify (xx: MatriI): VectoI = VectorI.fill (xx.dim1)(p_y.argmax ())

    def classify (xx: MatriD): VectoI = classify (xx.toInt)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a testiing dataset and return the
     *  fraction of correct classifications.
     *  @param itest  indices of the instances considered test data
     */
    override def test (itest: Ints): Double =
    {
        var correct = 0
        val c = classify (VectorI (1))._1                    // decision won't change
        for (i <- itest if c == y(i)) correct += 1
        correct / itest.size.toDouble
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the frequency counters.
     */
    def reset () { /* NA */ }

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

