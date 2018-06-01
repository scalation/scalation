
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell, Hao Peng, Dong-Yu Yu
 *  @version 1.4
 *  @date    Thu Sep 22 21:45:58 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see en.wikipedia.org/wiki/B-spline
 *  @see cran.r-project.org/web/packages/crs/vignettes/spline_primer.pdf
 *  @see http://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node17.html
 *  @see open.uct.ac.za/bitstream/item/16664/thesis_sci_2015_essomba_rene_franck.pdf?sequence=1
 */

package scalation.calculus

import java.io.FileOutputStream

import scalation.calculus.DBasisFunction._
import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.math.ExtremeD.TOL
import scalation.math.double_exp

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DB_Spline` class provides B-Spline basis functions with derivatives for
 *  various orders 'm', where the order is one more than the degree.  A spline
 *  function is a piecewise polynomial function where the pieces are stitched
 *  together at knots with the goal of maintaining continuity and differentability.
 *  B-Spline basis functions form a popular form of basis functions used in
 *  Functional Data Analysis.
 *  @see http://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node17.html
 *-----------------------------------------------------------------------------
 *  @param ττ    the time-points of the original knots in the time dimension
 *  @param mMax  the maximum order, allowing splines orders from 1 to mMax
 *  @param clamp whether or not to clamp the ends of the knot vector using `B_Spline.clamp`
 */
class DB_Spline (ττ: VectoD, mMax: Int = 4, clamp: Boolean = true)
        extends B_Spline (ττ, mMax, clamp) with DBasisFunction
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** First derivatives of order 'm' B-Spline basis functions (general
     *  recurrence).
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def d1bb (m:Int) (j: Int)(t: Double): Double =
    {
        if (mMax < m) flaw ("d1bb", s"mMax = $mMax can't be less than m = $m")
        if (m == 1) return 0.0
        val jm = j + m
        val n1 = t  - τ(j)
        val n2 = τ(jm) - t
        val d1 = τ(jm-1) - τ(j) // + TOL
        val d2 = τ(jm) - τ(j+1) // + TOL
        val a  = if (d1 =~ 0) 0 else ( bb (m-1)(j)(t)   + n1 * d1bb (m-1)(j)(t))   / d1
        val b  = if (d2 =~ 0) 0 else (-bb (m-1)(j+1)(t) + n2 * d1bb (m-1)(j+1)(t)) / d2
        a + b
    } // d1bb

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adjusted derivatives of order 'm' B-Spline basis functions (general
     *  recurrence). These are adjusted so that the first "usable" spline is at
     *  `j = 0`. The valid range of usable splines is defined in `range`.
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def d1bfr (m: Int)(j: Int)(t: Double): Double = d1bb (m)(j+mMax-m)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** First derivatives of order 'm' B-Spline basis functions (dynamic programming
     *  apporach)
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    override def d1bf (m:Int) (j: Int)(t: Double): Double =
    {
        if (m <= 1) return 0.0

        val b1 = bf (m-1)(j)(t)
        val b2 = bf (m-1)(j+1)(t)
        val f1 = if (b1 =~ 0) 0 else (m-1) / (τ(j+m-1) - τ(j)) * b1
        val f2 = if (b2 =~ 0) 0 else (m-1) / (τ(j+m) - τ(j+1)) * b2
        f1 - f2
    } // d1bff

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Second derivatives of order 'm' B-Spline basis functions (general
     *  recurrence).
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def d2bb (m:Int)(j: Int)(t: Double): Double =
    {
        if (mMax < m) flaw ("d2bb", s"mMax = $mMax can't be less than m = $m")
        if (m == 2) return 0.0
        val jm = j + m
        val n1 = t  - τ(j)
        val n2 = τ(jm) - t
        val d1 = τ(jm-1) - τ(j) //+ TOL
        val d2 = τ(jm) - τ(j+1) //+ TOL
        val a  = if (d1 =~ 0) 0 else ( 2.0 * d1bb (m-1)(j)(t)   + n1 * d2bb (m-1)(j)(t))   / d1
        val b  = if (d2 =~ 0) 0 else (-2.0 * d1bb (m-1)(j+1)(t) + n2 * d2bb (m-1)(j+1)(t)) / d2
        a + b
    } // d2bb

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adjusted second derivatives of order 'm' B-Spline basis functions
     *  (general recurrence). These are adjusted so that the first "usable"
     *  spline is at `j = 0`. The valid range of usable splines is defined in
     *  `range`.
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def d2bfr (m: Int)(j: Int)(t: Double): Double = d2bb (m)(j+mMax-m)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of 2nd derivative of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the 2nd derivative basis function by calling d2bf(m)(j) only.
     *  Ex: val x = d2bf(m)(j)(t) retrieves the 2nd derivative value of the j-th basis function at 't'.
     *      val f = d2bf(m)(j)    retrieves the 2nd derivative of the j-th basis function.
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    override def d2bf (m:Int) (j: Int)(t: Double): Double =
    {
        if (m <= 2) return 0.0

        val b1 = d1bf (m-1)(j)(t)
        val b2 = d1bf (m-1)(j+1)(t)
        val f1 = if (b1 =~ 0) 0 else (m-1) / (τ(j+m-1) - τ(j)) * b1
        val f2 = if (b2 =~ 0) 0 else (m-1) / (τ(j+m) - τ(j+1)) * b2
        f1 - f2
    } // d2bff

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** N-th derivatives of order 'm' B-Spline basis functions (general recurrence).
     *  @param n  the n-th derivative to be computed
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def dnbb (n: Int)(m: Int)(j: Int)(t: Double): Double =
    {
        if      (n == 2) d2bb(m)(j)(t)
        else if (n == 1) d1bb(m)(j)(t)
        else if (n == 0) bb  (m)(j)(t)
        else {
            val jm = j + m
            val d1 = τ(jm-1) - τ(j)
            val d2 = τ(jm) - τ(j+1)
            val a  = if (d1 =~ 0) 0 else ((m-1) / d1) * dnbb (n-1)(m-1)(j)(t)
            val b  = if (d2 =~ 0) 0 else ((m-1) / d2) * dnbb (n-1)(m-1)(j+1)(t)
            a - b
        } // if
    } // dnbb

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adjusted n-th derivative of order 'm' B-Spline basis functions
     *  (general recurrence). These are adjusted so that the first "usable" spline
     *  is at `j = 0`. The valid range of usable splines is defined in `range`.
     *  @param n  the n-th derivative to be computed
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def dnbfr (n: Int)(m: Int)(j: Int)(t: Double): Double = dnbb (n)(m)(j+mMax-m)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adjusted n-th derivative of order 'm' B-Spline basis functions
     *  (general recurrence). These are adjusted so that the first "usable" spline
     *  is at `j = 0`. The valid range of usable splines is defined in `range`.
     *  @param n  the n-th derivative to be computed
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def dnbf (n: Int)(m: Int)(j: Int)(t: Double): Double =
    {
        if (m <= n) return 0.0

        if      (n == 2) return d2bf(m)(j)(t)
        else if (n == 1) return d1bf(m)(j)(t)
        else if (n == 0) return   bf(m)(j)(t)

        val b = abf_ (m-n)(t)
        for (d <- 1 to n) {
            val mm = m - n + d      // effective order
            for(i <- j to j+n-d) {
                val b1 = b(i)
                val b2 = b(i+1)
                val f1 = if (b1 =~ 0) 0 else (mm-1) / (τ(i+mm-1) - τ(i)) * b1
                val f2 = if (b2 =~ 0) 0 else (mm-1) / (τ(i+mm) - τ(i+1)) * b2
                b(i) = f1 - f2
            } // for
        } // for
        b(j)
    } // dnbff

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of nth derivative of the m-th order basis functions (all) at time 't'.
     *  Or alternatively, obtain the basis function by calling dnabf(m)(j) only.
     *  Ex: val x = dnabf(m)(t) retrieves the nth derivative value of the value of all the basis functions at 't'.
     *      val f = dnabf(m)    retrieves the nth derivative value of all the basis functions.
     *  @param n  the order of the derivative
     *  @param m  the order of all the basis function
     *  @param t  the time parameter
     */
    override def dnabf_ (n: Int)(m: Int)(t: Double): VectorD =
    {
        if (m <= n) return new VectorD (τ.dim-m)

        val b = abf_ (m-n)(t)       // base case
        for (d <- 1 to n) {
            val mm = m - n + d      // effective order
            val nf = τ.dim - mm     // number of basis funftions for order mm
                for (i <- 0 until nf) {
                    val b1 = b(i)
                    val b2 = b(i+1)
                    val f1 = if (b1 == 0) 0 else (mm-1) / (τ(i+mm-1) - τ(i)) * b1
                    val f2 = if (b2 == 0) 0 else (mm-1) / (τ(i+mm) - τ(i+1)) * b2
                    b(i) = f1 - f2
                } // for
        } // for
        new VectorD (τ.dim-m, b().array)
    } // dnabff_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of nth derivative of the m-th order basis functions (all) at time 't'.
     *  Or alternatively, obtain the basis function by calling dnabf(m)(j) only.
     *  Ex: val x = dnabfr(m)(t) retrieves the nth derivative value of the value of all the basis functions at 't'.
     *      val f = dnabfr(m)    retrieves the nth derivative value of all the basis functions.
     *  Note that this is the recursive approach as opposed to the dynamic programming approach.
     *  @param n  the order of the derivative
     *  @param m  the order of all the basis function
     *  @param t  the time parameter
     */
    def dnabfr_ (n: Int)(m: Int)(t: Double): VectorD =
    {
        val dΦt = new VectorD (size (m))
        for (j <- range (m)) dΦt (j) = dnbfr (n)(m)(j)(t)
        dΦt
    } // dnabf_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of nth derivative of the m-th order basis functions (all) at time 't'.
     *  Or alternatively, obtain the basis function by calling dnabf(m)(j) only.
     *  Ex: val x = dnabfr(n)(m)(t) retrieves the nth derivative value of the value of all the basis functions at 't'.
     *      val f = dnabfr(n)(m)    retrieves the nth derivative value of all the basis functions.
     *  Note that this is the recursive approach as opposed to the dynamic programming approach.
     *  @param n  the order of the derivative
     *  @param m  the order of all the basis function
     *  @param t  the time parameter
     */
    def dnabfr (n: Int)(m: Int)(t: VectorD) = MatrixD (for (j <- t.indices) yield dnabfr_ (n)(m)(t(j)), false)

} // DB_Spine class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DB_SplineTest` object is used to test the `DB_Spline` class. Here, we
 *  compute the penalty matrix for a ridge regression.
 * > runMain scalation.caculus.DB_SplineTest
 */
object DB_SplineTest extends App
{
    val mM  = 4                                         // maximum order to test
    val n   = 100                                       // number of time points
    val τ   = VectorD (0.0, 0.5 * (n-1), (n-1)) / n     // knot vector (unaugmented)
    val dbs = new DB_Spline (τ, mM)                     // DB-Spline generator
    val t   = VectorD.range (0, n) / n                  // time-points
    val ns  = dbs.size ()                               // number of splines
    val Σ   = penalty (dbs, mM)(t)                      // penalty matrix
    val λ   = 0.01                                      // regularization parameter

    println (s" λ = $λ")
    println (s" Σ = $Σ")
    println (s"λΣ = ${Σ * λ}")

} // DB_SplineTest object

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DB_SplineTest2` object is used to test the `DB_Spline` class. Here, we
 *  smooth functional data manually with a roughness penalty.
 *  @see Ramsay et al. page 86
 * > runMain scalation.caculus.DB_SplineTest2
 */
object DB_SplineTest2 extends App
{
    import scalation.random.Normal
    val normal = Normal ()
    val mM = 4                                         // maximum order to test
    val n  = 100                                       // number of time points
    val G  = 10                                        // gap
    val t  = VectorD.range (0, n) / n                  // time-points
    val τ  = VectorD.range (0, n/G) / ((n-1)/G) * t(t.dim-1)       // knot vector (unaugmented)
    val dbs = new DB_Spline (τ, mM)                    // DB-Spline generator

    val ns = dbs.range ().size                         // number of splines
    val Σ  = penalty (dbs, mM)(t)                      // penalty matrix
    val λ  = 1E-1                                      // regularization parameter

    val y = new VectorD (n)
    val w = new VectorD (n)
    for (i <- 0 until n) { w(i) = t(i) * t(i); y(i) = w(i) + normal.gen }

    val Φ = dbs.abf (mM)(t)                            // basis matrix
    val I = MatrixD.eye(ns)                            // identity matrix
    val W = MatrixD.eye(n)                             // weight matrix
    val c = ((Φ.t * W * Φ) + (Σ * λ)).inverse * Φ.t * W * y // Penalized Ridge
//  val c = ((Φ.t * Φ) + (I * λ)).inverse * Φ.t * y    // Simple Ridge
//  val c = (Φ.t * Φ).inverse * Φ.t * y                // OLS

    val z = new VectorD (n)                            // smoothed values
    for (i <- 0 until n) z(i) = {
        var sum = 0.0
        for (j <- dbs.range()) sum += c(j) * dbs (mM) (j)(t(i))
        sum
    } // for

    println (s"  y = $y")
    println (s"  λ = $λ")
    println (s"  Σ = $Σ")
    println (s" λΣ = ${Σ * λ}")
    println (s" λI = ${I * λ}")
    println (s"  Φ = $Φ")
    println (s"Φ'Φ = ${Φ.t * Φ}")
    println (s"  c = $c")
    println (s"  z = $z")

    import scalation.plot.Plot
    new Plot (t, y, z)
    new Plot (t, y, w)

} // DB_SplineTest2 object

