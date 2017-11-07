
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Jan  9 21:48:57 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scalation.linalgebra.{MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.stat.StatVector.{cov, mean}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LDA` class implements a Linear Discriminant Analysis 'LDA' classifier.
 *  @see en.wikipedia.org/wiki/Linear_discriminant_analysis
 *  @param x   the real-valued training/test data vectors stored as rows of a matrix
 *  @param y   the training/test classification vector, where y_i = class for row i of the matrix x
 *  @param fn  the names for all features/variables
 */
class LDA (x: MatrixD, y: VectoI, fn: Array [String])
      extends ClassifierReal (x, y, fn, 2, Array ("no", "yes"))
{
    private val DEBUG = true
    private val x0 = (MatrixD (for (i <- 0 until x.dim1 if y(i) == 0) yield x(i))).t
    private val x1 = (MatrixD (for (i <- 0 until x.dim1 if y(i) == 1) yield x(i))).t

    if (DEBUG) {
        println ("x0 = " + x0)
        println ("x1 = " + x1)
    } // if

    private val mu0  = mean (x0)
    private val mu1  = mean (x1)
    private val sig  = cov (x)
    private val sig0 = cov (x0)
    private val sig1 = cov (x1)

    private var c = 0.5
    private var w: VectoD = null

    if (DEBUG) {
        println ("mu0  = " + mu0)
        println ("mu1  = " + mu1)
        println ("sig0 = " + sig0)
        println ("sig1 = " + sig1)
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     */
    def setCutoff (thres: Double)
    {
        c = 0.5 * (thres - (mu0 dot sig0.inverse * mu0) + (mu1 dot sig1.inverse * mu1))
    } // setCutoff

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     */
    def train (testStart: Int, testEnd: Int)
    {
        w = sig.inverse * (mu0 - mu1)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     */
    def reset (): Unit = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     */
    def classify (z: VectoD): (Int, String, Double) =
    {
        if ((w dot z) > c) (1, "yes", 1.0) else (0, "no", 0.0)
    } // classify

} // LDA class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LDATest` is used to test the `LDA` class.
 *  @see people.revoledu.com/kardi/tutorial/LDA/Numerical%20Example.html
 *  > run-main scalation.analytics.classifier.LDATest
 */
object LDATest extends App
{
    // features/variable: 
    // x0: curvature
    // x0: diameter
    //                         x0    x1
    val x = new MatrixD ((7, 2), 2.95, 6.63,
                                 2.53, 7.79,
                                 3.57, 5.65,
                                 3.16, 5.47,
                                 2.58, 4.46,
                                 2.16, 6.22,
                                 3.27, 3.52)
    val y = VectorI (0, 0, 0, 0, 1, 1, 1)

    val cl = new LDA (x, y, Array ("curvature", "diameter"))
    cl.train ()
    val z  = VectorD (2.81, 5.46)

    println (s"classify ($z) = ${cl.classify (z)}")

} // LDATestObject

