
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Jan 14 19:37:20 EST 2012
 *  @see     LICENSE (MIT style license file).
 *  @see http://www.svms.org/tutorials/
 *  @see http://www.svms.org/tutorials/Burges1998.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import scala.math.signum

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.maxima.ConjGradient
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class implements linear support vector machines (SVM).  Each vector xi
 *  is stored as a row in the x matrix.
 *  @param x  the matrix consisting of vectors
 *  @param y  the vector of outcomes (e.g., positive(1), negative(-1))
 */
class SupportVectorMachine (x: MatrixD, y: VectorD)
      extends Classifier with Error
{
    if (x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val w = new VectorD (x.dim2)     // normal vector to the hyperplane
    private var b = 0.0                       // intercept

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Lagrangian Dual function, i.e., the objective function, to be
     *  maximized.
     *  @param a  the vector of Lagrange multipliers
     */
    def l_D (a: VectorD): Double =
    {
        var sum = 0.0
        for (i <- 0 until x.dim1; j <- 0 until x.dim2) {
            sum += a(i) * a(j) * y(i) * y(j) * (x(i) dot x(j))
        } // for
        a.sum - .5 * sum
    } // l_D

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Equality constraint to be satisfied, dot product of a and y == 0.0
     *  @param a  the vector of Lagrange multipliers
     */
    def g (a: VectorD): Double = a dot y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the normal vector w of the separating hyperplane w dot x + b = 0.0
     *  Use a quadratic programming solver to find the optimal values for the
     *  the Lagrange multipliers a.  Use these to compute w.
     */
    def find_w ()
    {
        val a0 = new VectorD (y.dim)
        val solver = new ConjGradient (l_D, g, false)      // may try other optimizers
        val a = solver.solve (a0)
        for (i <- 0 until y.dim) w += x(i) * (a(i) * y(i))
    } // find_w

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the intercept b of the separating hyperplane w dot x + b = 0.0
     *  Use ...
     */
    def find_b ()
    {
        b = 0.0  // FIX: to be implemented
    } // find_b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From the positive and negative cases (vectors), find an optimal separating
     *  hyperplane w dot x + b = 0.0
     */
    def train ()
    {
        find_w ()      // find normal vector w
        find_b ()      // find intercept scalar b
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (normal vector w, intercept b)
     */
    def fit: Tuple2 [VectorD, Double] = (w, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, determine which class it belongs
     *  to by determining whether the function is positive (1), zero (0) or
     *  negative (-1), i.e., compute the sign function of (w dot z + b).
     */
    def classify (z: VectorD): Int = (signum (w dot z + b)).toInt

} // SupportVectorMachine class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SupportVectorMachine class.
 */
object SupportVectorMachineTest extends App
{
    val x = new MatrixD ((2, 2), 1.0, 1.0,
                                 2.0, 2.0)
    val y = VectorD (1.0, -1.0)

    println ("x = " + x)
    println ("y = " + y)

    val svm = new SupportVectorMachine (x, y)
    svm.train ()
    println ("fit = " + svm.fit)

    val z  = VectorD (3.0, 3.0)               // classify y for one point
    val yp = svm.classify (z)
    println ("classify (" + z + ") = " + yp)

} // SupportVectorMachineTest object

