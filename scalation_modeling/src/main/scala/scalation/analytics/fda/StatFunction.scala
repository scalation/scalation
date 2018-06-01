
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Oct 16 15:30:50 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see Functional Data Analysis, Second Edition, Section 2.3
 *  @see http://link.springer.com/book/10.1007%2Fb98888
 */

package scalation.analytics.fda

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.Functions
import scalation.stat.vectorD2StatVector

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatFunction` companion object extends statistics vector operations to matrices.
 *  The `StatFunction` object/class is the functional analog to the `StatVector` object/class.
 */
object StatFunction
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean vector containing the cross-sectional means over the time points.
     *  @param xa  the array of functions
     *  @param t   the vector of time points
     */
    def mean (xa: Functions, t: VectorD): VectorD =
    {
        val sf = new StatFunction (xa)
        VectorD (for (i <- t.indices) yield sf(t(i)).mean)
    } // mean

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the array of functions 'xa' with time points 't' into a matrix.
     *  @param xa  the array of functions
     *  @param t   the vector of time points
     */
    def toMatrix (xa: Functions, t: VectorD): MatrixD =
    {
        val sf = new StatFunction (xa)
        MatrixD (for (i <- t.indices) yield sf(t(i)))
    } // toMatrix

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sample covariance matrix for the functions over the time points.
     *  @param xa  the array of functions
     *  @param t   the vector of time points
     */
    def cov (xa: Functions, t: VectorD): MatrixD =
    {
        val sf = new StatFunction (xa)
        MatrixD ((t.dim, t.dim),
                (for (i <- t.indices; j <- t.indices) yield sf(t(i)).cov (sf(t(j)))) :_*)
    } // cov

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the population covariance matrix for the functions over the time points.
     *  @param xa  the array of functions
     *  @param t   the vector of time points
     */
    def pcov (xa: Functions, t: VectorD): MatrixD =
    {
        val sf = new StatFunction (xa)
        MatrixD ((t.dim, t.dim),
                (for (i <- t.indices; j <- t.indices) yield sf(t(i)).pcov (sf(t(j)))) :_*)
    } // pcov

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the correlation matrix for the functions over the time points.
     *  Note:  sample vs. population results in essentailly the same values.
     *  @param xa  the array of functions
     *  @param t   the vector of time points
     */
    def corr (xa: Functions, t: VectorD): MatrixD =
    {
        val sf = new StatFunction (xa)
        MatrixD ((t.dim, t.dim),
                (for (i <- t.indices; j <- t.indices) yield sf(t(i)).corr (sf(t(j)))) :_*)
    } // corr

} // StatFunction


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatFunction` class takes an array of functions 'xa' and provides an
 *  'apply' method for evaluating these functions at time 't' to produce a
 *  vector from which statistics can be computed.  Each function represents
 *  an experimental replication.
 *  @param xa  the array of functions
 */
class StatFunction (xa: Functions)
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate each function in 'xa' at times 't', and then produce the vector
     *  of values.
     *  @param t  the time at which to evaluate the functions
     */
    def apply (t: Double): VectorD =  VectorD (xa.map (_(t)))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate each function in 'xa' at times 't1' and 't2', and then produce
     *  two vectors of values.
     *  @param t  the time at which to evaluate the functions
     */
    def apply (t1: Double, t2: Double): Array [VectorD] =
    {
        Array (VectorD (xa.map (_(t1))), VectorD (xa.map (_(t2))))
    } // apply

} // StatFunction class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatFunctionTest` object is used to test the `StatFunction` class.
 *  > runMain scalation.analytics.fda.StatFunctionTest
 */
object StatFunctionTest extends App
{
    val x0 = (t: Double) => 2 * t                    // first function
    val x1 = (t: Double) => t * t                    // second function
    val xa = Array (x0, x1)                          // array of functions
    val sf = new StatFunction (xa)                   // stat function

    val t1  = 3.0                                    // first time point
    val sv1 = sf (t1)                                // first stat vector - sf evaluated at t1
    val t2  = 5.0                                    // second time point
    val sv2 = sf (t2)                                // second stat vector - sf evaluated at t2

    println ("sv1.mean     = " + sv1.mean)           // mean at t1
    println ("sv1.variance = " + sv1.variance)       // variance at t1
    println ("sv1.stddev   = " + sv1.stddev)         // standard deviation at t1
    println ("sv2.mean     = " + sv2.mean)           // mean at t2
    println ("sv2.variance = " + sv2.variance)       // variance at t2
    println ("sv2.stddev   = " + sv2.stddev)         // standard deviation at t2

    println ("sv1.covariance  = "+ sv1.cov (sv2))    // covariance at t1 vs. t2
    println ("sv1.correlation = "+ sv1.corr (sv2))   // correlation at t1 vs. t2

} // StatFunctionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatFunctionTest2` object is used to test the `StatFunction` object.
 *  > runMain scalation.analytics.fda.StatFunctionTest2
 */
object StatFunctionTest2 extends App
{
    import scalation.plot.PlotM

    val x0 = (t: Double) => 2 * t                    // first function
    val x1 = (t: Double) => t * t                    // second function
    val xa = Array (x0, x1)                          // array of functions

    val t  = VectorD.range (0, 49) / 50.0            // time points
    val f0 = t.map (xa(0)(_))                        // points for function 1
    val f1 = t.map (xa(1)(_))                        // points for function 2
    val means = StatFunction.mean (xa, t)            // points for mean function

    val mat = new MatrixD (3, t.dim)
    mat(0)  = f0
    mat(1)  = f1
    mat(2)  = means
    println ("t   = " + t)
    println ("mat = " + mat)
    new PlotM (t, mat, Array ("means vs. time"))

    val t2 = t.slice (0, 10)

    println ("-" * 60)
    println ("cov matrix  = " + StatFunction.cov (xa, t2))       // sample covariance matrix
    println ("-" * 60)
    println ("pcov matrix = " + StatFunction.pcov (xa, t2))      // population covariance matrix
    println ("-" * 60)
    println ("corr matrix = " + StatFunction.corr (xa, t2))      // correlation matrix
    println ("-" * 60)

} // StatFunctionTest2 object

