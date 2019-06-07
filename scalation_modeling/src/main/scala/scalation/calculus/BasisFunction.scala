
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 1.6
 *  @date    Sun Nov  5 12:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.calculus

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.math.FunctionS2S

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BasisFunction` object provides utility functions related to
 *  basis functions.
 */
object BasisFunction
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot/inner product of functions 'f' and 'g'.
     *  @param f  the 1st function
     *  @param g  the other function
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def dot (f: FunctionS2S, g: FunctionS2S, a: Double = 0.0, b: Double = 1.0) = f.dot (g, a, b)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot the basis functions.
     *  @param bf  a basis function
     *  @param m   the order of the basis function
     *  @param t   the time parameter
     */
    def plot (bf: BasisFunction, m: Int, t: VectorD)
    {
        import scalation.plot.PlotM
        val ns  = bf.size(m)
        val n   = t.dim
        val y   = new MatrixD (ns, n)
        for (i <- 0 until n; j <- bf.range(m) if m > 0) y(j, i) = bf (m)(j)(t(i))
        if (m > 0) new PlotM (t, y, null, "Basis Function order " + m, lines = true)
    } // plot

} // BasisFunctions object

import BasisFunction.dot

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BasisFunction` trait provides a common framework for various Basis Functions.
 *  They are a set of functions forming a basis whose orthogonal components form a
 *  function space. Two functions 'f' and 'g' are orthogonal if their inner product is 0,
 *  meaning <f, g> = 0.
 */
trait BasisFunction
{
    protected var Φ:   MatrixD = null       // matrix of all basis functions, time points x functions
    protected var Φt:  MatrixD = null       // transpose of Φ
    protected var ΦtΦ: MatrixD = null       // Φ.t * Φ
    protected var needCompute  = true       // flag for computing Φ, which only needs be computed once

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the 1st order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the basis function by calling bf1(j) only.
     *  Ex: val x = bf1(j)(t) retrieves the value of the j-th basis function at 't'.
     *      val f = bf1(j)    retrieves the j-th basis function.
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def bf1 (j: Int)(t: Double): Double = bf (1)(j)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the 2nd order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the basis function by calling bf2(j) only.
     *  Ex: val x = bf2(j)(t) retrieves the value of the j-th basis function at 't'.
     *      val f = bf2(j)    retrieves the j-th basis function.
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def bf2 (j: Int)(t: Double): Double = bf (2)(j)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the 3rd order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the basis function by calling bf3(j) only.
     *  Ex: val x = bf3(j)(t) retrieves the value of the j-th basis function at 't'.
     *      val f = bf3(j)    retrieves the j-th basis function.
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def bf3 (j: Int)(t: Double): Double = bf (3)(j)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the 4th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the basis function by calling bf4(j) only.
     *  Ex: val x = bf4(j)(t) retrieves the value of the j-th basis function at 't'.
     *      val f = bf4(j)    retrieves the j-th basis function.
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def bf4 (j: Int)(t: Double): Double = bf (4)(j)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the basis function by calling bf(m)(j) only.
     *  Ex: val x = bf(m)(j)(t) retrieves the value of the j-th basis function at 't'.
     *      val f = bf(m)(j)    retrieves the j-th basis function.
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def bf (m: Int)(j: Int)(t: Double): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the basis function by calling bf(m)(j) only.
     *  Ex: val x = bf(m)(j)(t) retrieves the value of the j-th basis function at 't'.
     *      val f = bf(m)(j)    retrieves the j-th basis function.
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def apply (m: Int)(j: Int)(t: Double): Double = bf (m)(j)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the m-th order basis functions (all) at time 't'.
     *  Or alternatively, obtain the basis function by calling bf(m)(j) only.
     *  Ex: val x = bf(m)(t) retrieves the value of all the basis functions at 't'.
     *      val f = bf(m)    retrieves all the basis functions.
     *  @param m  the order of all the basis function
     *  @param t  the time parameter
     */
    def abf_ (m: Int)(t: Double): VectorD =
    {
        val Φ_t  = new VectorD (size (m))           // Φ_t represents the t-th row of the Φ matrix
        for (j <- Φ_t.range) Φ_t(j) = bf (m)(j)(t)
        Φ_t
    } // abf_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the m-th order basis functions (all) at time 't'.
     *  Or alternatively, obtain the basis function by calling bf(m)(j) only.
     *  Ex: val x = bf(m)(t) retrieves the value of all the basis functions at 't'.
     *      val f = bf(m)    retrieves all the basis functions.
     *  @param m  the order of all the basis function
     *  @param t  the time parameter
     */
    def abf (m: Int)(t: VectoD): MatrixD =
    {
        if (needCompute || Φ == null) Φ = MatrixD (for (j <- t.indices) yield abf_ (m)(t(j)), false)
        Φ
    } // abf

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot/inner product of 'this' basis function object and basis function 'g'.
     *  @param m  the order of the basis function
     *  @param i  indicates which basis function of 'this'
     *  @param j  indicates which basis function of 'g'
     *  @param g  the other function
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def dot_ (m: Int)(i: Int, j: Int)(g: BasisFunction, a: Double = 0.0, b: Double = 1.0) =
    {
        dot (bf(m)(i), g.bf(m)(j), a, b)
    } // dot_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The number of basis functions for a specified order.
     *  @param m  the order of the basis function
     */
    def size (m: Int): Int

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The number of basis functions for a specified order.
     *  @param m  the order of the basis function
     */
    def count (m: Int): Int = size (m)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The range of basis functions for a specified order.
     *  @param m  the order of the basis function
     */
    def range (m: Int): Range

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieves the order of the Basis function
     */
    def getOrder: Int

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recompute cached matrices
     */
    def recomputeCache = needCompute = true

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieves the cached design matrices
     */
    def getCache (m: Int, t: VectoD): Array [MatrixD] =
    {
        if (needCompute) {
            Φ   = abf (m)(t)
            Φt  = Φ.t
            ΦtΦ = Φt * Φ
            needCompute = false
        } // if
        Array (Φ, Φt, ΦtΦ)
    } // getCache

} // BasisFunction trait

