
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 1.6
 *  @date    Sun Nov  5 12:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.calculus

import scalation.calculus.BasisFunction.dot
import scalation.calculus.DBasisFunction.penalty
import scalation.linalgebra.{MatrixD, VectoD, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DBasisFunction` object provides utility functions related to
 *  derivatives of basis functions.
 */
object DBasisFunction
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Computes the roughness penalty matrix defined in Ramsay et al.
     *  (Section 5.8), which is composed of the integrals of products of
     *  the second derivative of basis functions.
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param t  the time parameter
     */
    def penalty (dbf: DBasisFunction, m: Int) (t: VectoD): MatrixD =
    {
        val d2Φ = dbf.dnabf (2)(m)(t)
        d2Φ.t * d2Φ
    } // penalty

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot the B-spline basis functions and their first and second derivatives.
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param t  the time parameter
     */
    def plot (dbf: DBasisFunction, m: Int, t: VectoD)
    {
        import scalation.plot.PlotM
        val ns  = dbf.size(m)
        val n   = t.dim
        val d0y = new MatrixD (ns, n)
        val d1y = new MatrixD (ns, n)
        val d2y = new MatrixD (ns, n)
        for (i <- 0 until n; j <- dbf.range(m)) {
            d0y(j, i) =   dbf.bf (m)(j)(t(i))
            d1y(j, i) = dbf.d1bf (m)(j)(t(i))
            d2y(j, i) = dbf.d2bf (m)(j)(t(i))
        } // for
        new PlotM (t, d0y, null, "Basis Function order " + m, lines = true)
        new PlotM (t, d1y, null, "First Derivative of Basis Function order " + m, lines = true)
        new PlotM (t, d2y, null, "Second Derivative of Basis Function order " + m, lines = true)
    } // plot

} // DBasisFunction object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DBasisFunction` trait provides a common framework for the derivatives of
 *  various Basis Functions.
 */
trait DBasisFunction extends BasisFunction
{
    protected var Σ: MatrixD = null

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of 1st derivative of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the 1st derivative basis function by calling d1bf(m)(j) only.
     *  Ex: val x = d1bf(m)(j)(t) retrieves the 1st derivative value of the j-th basis function at 't'.
     *      val f = d1bf(m)(j)    retrieves the 1st derivative of the j-th basis function.
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def d1bf (m: Int)(j: Int)(t: Double): Double = dnbf (1)(m)(j)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of 2nd derivative of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the 2nd derivative basis function by calling d2bf(m)(j) only.
     *  Ex: val x = d2bf(m)(j)(t) retrieves the 2nd derivative value of the j-th basis function at 't'.
     *      val f = d2bf(m)(j)    retrieves the 2nd derivative of the j-th basis function.
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def d2bf (m: Int)(j: Int)(t: Double): Double = dnbf (2)(m)(j)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of nth derivative of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the nth derivative basis function by calling dnbf(n)(m)(j) only.
     *  Ex: val x = dnbf(n)(m)(j)(t) retrieves the nth derivative value of the j-th basis function at 't'.
     *      val f = dnbf(n)(m)(j)    retrieves the nth derivative of the j-th basis function.
     *  @param n  the order of the derivative
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def dnbf (n: Int)(m: Int)(j: Int)(t: Double): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of nth derivative of the m-th order basis functions (all) at time 't'.
     *  Or alternatively, obtain the basis function by calling dnabf(m)(j) only.
     *  Ex: val x = dnabf(n)(m)(t) retrieves the nth derivative value of the value of all the basis functions at 't'.
     *      val f = dnabf(n)(m)    retrieves the nth derivative value of all the basis functions.
     *  @param n  the order of the derivative
     *  @param m  the order of all the basis function
     *  @param t  the time parameter
     */
    def dnabf_ (n: Int)(m: Int)(t: Double): VectorD =
    {
        val dΦt = new VectorD (size (m))
        for (j <- range (m)) dΦt (j) = dnbf (n)(m)(j)(t)
        dΦt
    } // dnabf_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of nth derivative of the m-th order basis functions (all) at time 't'.
     *  Or alternatively, obtain the basis function by calling dnabf(m)(j) only.
     *  Ex: val x = dnabf(n)(m)(t) retrieves the nth derivative value of the value of all the basis functions at 't'.
     *      val f = dnabf(n)(m)    retrieves the nth derivative value of all the basis functions.
     *  @param n  the order of the derivative
     *  @param m  the order of all the basis function
     *  @param t  the time parameter
     */
    def dnabf (n: Int)(m: Int)(t: VectoD) = MatrixD (for (j <- t.indices) yield dnabf_ (n)(m)(t(j)), false)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot/inner product of nth derivative of 'this' basis function and
     *  that of basis function 'g'.
     *  @param n  the order of the derivative
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param g  the other function
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def dot_ (n: Int)(m: Int)(i: Int, j: Int)(g: DBasisFunction, a: Double, b: Double) =
    {
        dot (dnbf (n)(m)(i), g.dnbf (n)(m)(j), a, b)
    } // dot_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieves the cached design matrices and penalty matrices
     *  @param m  the order of all the basis function
     *  @param t  the time parameter
     */
    override def getCache (m: Int, t: VectoD): Array [MatrixD] =
    {
        if (needCompute) {
            Φ = abf (m)(t)
            Φt = Φ.t
            ΦtΦ = Φt * Φ
            Σ = penalty (this, m)(t)
            needCompute = false
        } // if
        Array (Φ, Φt, ΦtΦ, Σ)
    } // getCache

} // DBasisFunction trait

