
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Feb 27 15:16:23 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.math.log

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.plot.Plot
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLDA` class implements a Linear Discriminant Analysis 'LDA' classifier.
 *  It places a value into a group according to its maximal discriminant function.
 *  @see en.wikipedia.org/wiki/Linear_discriminant_analysis
 *  @param x    the real-valued training/test data values stored in a vector
 *  @param y    the training/test classification vector, where y_i = class for x_i
 *  @param fn_  the name of the feature/variable
 *  @param k    the number of possible values for y (0, 1, ... k-1)
 *  @param cn_  the names for all classes
 */
class SimpleLDA (x: VectoD, y: VectoI, fn_ : Strings = Array ("x1"), k: Int = 2,
                 cn_ : Strings = null)
      extends ClassifierReal (MatrixD (Seq (x)), y, fn_, k, cn_)
{
    private val DEBUG = true                                      // debug flag
    private val xc    = for (c <- 0 until k) yield                // groups for x
        VectorD (for (i <- x.range if y(i) == c) yield x(i))      // group c

    private var py: VectorD = null                                // probability y = c
    private var mu: VectorD = null                                // group means
    private var sig2 = 0.0                                        // pooled variance
    private var term1: VectorD = null                             // first term in classify
    private var term2: VectorD = null                             // second term in classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifer by computing 'pcovar', 'ipcovar' and 'prior'
     *  that are needed to compute the discriminant functions 'f'.
     *  These are computed in the 'classify' method.
     *  @param itest  the indices of the test data - FIX - not used yet
     */
    def train (itest: Ints): SimpleLDA =
    {
        py = VectorD (xc.map (_.dim / md))                        // probability y = c
        mu = VectorD (xc.map (_.mean))                            // group means
        var sum = 0.0
        for (c <- 0 until k) sum += (xc(c) - mu(c)).normSq
        sig2  = sum / (m - k).toDouble                            // pooled variance
        term1 = mu / sig2
        term2 = mu~^2 / (2.0 * sig2) - py.map (log (_))

        if (DEBUG) {
            println (s" py \t = $py \n mu \t = $mu \n sig2 \t = $sig2")
            println (s" term1 \t = $term1 \n term2 \t = $term2")
        } // if
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify vector 'z' by computing its discriminant function 'delta' for
     *  each group and return the group index with the maximun value for 'delta'.
     */
    override def classify (z: VectoD): (Int, String, Double) =
    {
        val delta = term1 * z(0) - term2
        if (DEBUG) println ("delta = " + delta)
        val best = delta.argmax ()
        (best, cn(best), delta(best))
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset method not applicable.
     */
    def reset () { /* Not Applicable */ }

} // SimpleLDA class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLDATest` is used to test the `SimpleLDA` class.
 *  @see people.revoledu.com/kardi/tutorial/LDA/Numerical%20Example.html
 *  > runMain scalation.analytics.classifier.SimpleLDATest
 */
object SimpleLDATest extends App
{
    // features/variable: 
    // x1: curvature
    //                 x1
    val x = VectorD (2.95, 2.53, 3.57, 3.16, 2.58, 2.16, 3.27)
    val y = VectorI (   0,    0,    0,    0,    1,    1,    1)

    val k  = 2                                                 // number of classes
    val fn = Array ("curvature")                               // feature name
    val cn = Array ("pass", "fail")                            // class names
    val lda = new SimpleLDA (x, y, fn, k, cn)                  // create SimpleLDA classifier
    lda.train ()

    banner ("classify")
    val z  = VectorD (2.81)
    println (s"classify ($z) = ${lda.classify (z)}")

    banner ("test")
    val xx = new MatrixD (x.dim, 1)
    for (i <- x.range) xx(i) = VectorD (x(i))
    val yp = lda.classify (xx)
    println (lda.fitMap (y, yp))
    val cm = new ConfusionMat (y, yp)
    println (s"cm = ${cm.confusion}")

    val t = VectorD.range (0, x.dim)
    new Plot (t, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. t")
    new Plot (x, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. x")

} // SimpleLDATestObject


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLDATest2` is used to test the `SimpleLDA` class.
 *  > runMain scalation.analytics.classifier.SimpleLDATest2
 */
object SimpleLDATest2 extends App
{
    import scalation.random.Normal

    val normal1 = Normal (98.6,  1.0)                          // variate generator
    val normal2 = Normal (101.0, 1.0)                          // variate generator

    val x = new VectorD (200)                                  // temperature vector
    val y = new VectorI (x.dim)                                // actual class vector
    for (i <- 0 until 100)   { x(i) = normal1.gen; y(i) = 0 }
    for (i <- 100 until 200) { x(i) = normal2.gen; y(i) = 1 }

    val k   = 2                                                // number of classes
    val fnm = Array ("temperature")                            // feature name
    val cn  = Array ("well", "has-flu")                        // class names
    val lda  = new SimpleLDA (x, y, fnm, k, cn)                // create SimpleLDA classifier
    lda.train ()

    banner ("classify")
    val xx = new MatrixD (x.dim, 1)
    for (i <- x.range) xx(i) = VectorD (x(i))
    val yp = lda.classify (xx)
    println ("y  = " + y)
    println ("yp = " + yp)
    println (lda.fitMap (y, yp))

    val t = VectorD.range (0, x.dim)
    new Plot (t, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. t")
    new Plot (x, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. x")

} // SimpleLDATestObject2

