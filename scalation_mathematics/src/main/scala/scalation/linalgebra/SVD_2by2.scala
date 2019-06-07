
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon May 19 15:52:24 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.netlib.org/lapack/lawnspdf/lawn03.pdf
 *  @see www.netlib.org/lapack/lawns/lawn11.ps
 *  @see fortranwiki.org/fortran/show/svd
 *  @see www.math.pitt.edu/~sussmanm//2071Spring08/lab09/index.html
 */

package scalation.linalgebra

import scala.math.{abs, max, min, sqrt}

import scalation.math.{double_exp, sign}
import scalation.math.ExtremeD.EPSILON

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD_2by2` is used to solve Singular Value Decomposition for
 *  bidiagonal 2-by-2 matrices.
 *  <p>
 *      [ f g ]
 *      [ 0 h ]
 *  <p>
 *  @see fortranwiki.org/fortran/show/svd
 *
 *  @param f  the first diagonal element
 *  @param g  the super-diagonal element
 *  @param h  the second diagonal element
 */
class SVD_2by2 (f: Double, g: Double, h: Double)
      extends SVDecomp
{
    private var ssMin = 0.0           // smallest singular values
    private var ssMax = 0.0           // largest singular value
    private var left  = (0.0, 0.0)    // left singular vector
    private var right = (0.0, 0.0)    // right singular vector
    private var lt    = (0.0, 0.0)    // temp left singular vector
    private var rt    = (0.0, 0.0)    // temp right singular vector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' forming a diagonal matrix consisting of singular
     *  values and return the singular values in a vector.
     */
    override def factor123 (): FactorType =
    {
        (null, null, null)                                  // FIX 
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the two singular values (smallest first) for the bidiagonal 2-by-2
     *  matrix form from the elements f, g and h. 
     *
     *  @see LAPACK SUBROUTINE DLAS2 (F, G, H, SSMIN, SSMAX)
     */
    def deflate (): VectorD =
    {
        val fa   = abs (f)                // absolute value of f
        val ga   = abs (g)                // absolute value of g
        val ha   = abs (h)                // absolute value of h
        val fhmn = min (fa, ha)           // minimum of fa and ha
        val fhmx = max (fa, ha)           // maximum of fa and ha

        var as = 0.0
        var at = 0.0
        var au = 0.0
        var c  = 0.0

        if (fhmn =~ 0.0) {
            return VectorD (0.0,
                            if (fhmx =~ 0.0) ga
                            else max (fhmx, ga) * sqrt (1.0 + (min (fhmx, ga) / max (fhmx, ga))~^2))
        } // if

        if (ga < fhmx) {
            as = 1.0 + fhmn / fhmx
            at = (fhmx - fhmn) / fhmx
            au = (ga / fhmx)~^2
            c = 2.0 / ( sqrt (as * as + au) + sqrt (at * at + au))
            return VectorD (fhmn * c, fhmx / c)
        } // if

        au = fhmx / ga
        if (au =~ 0.0) {
            return VectorD ((fhmn * fhmx ) / ga, ga)
        } // if

        as = 1.0 + fhmn / fhmx
        at = (fhmx - fhmn ) / fhmx
        c = 1.0 / (sqrt (1.0 + (as * au)~^2) + sqrt (1.0 + (at * au)~^2))
        VectorD ((fhmn * c) * au * 2.0, ga / (c + c))
    } // deflate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the two singular values (smallest first) for the bidiagonal 2-by-2
     *  matrix form from the elements f, g and h.  Also, return the singular
     *  vectors.
     *
     *  @see LAPACK SUBROUTINE DLASV2 (F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL)
     */
    def deflateV (): Tuple6 [Double, Double, Double, Double, Double, Double] =
    {
        var ft  = f
        var fa  = abs (f)
        var ht  = h
        var ha  = abs (h)
        var gt  = g
        var ga  = abs (gt)
        var tmp = 0.0

//      pmax points to the maximum absolute element of matrix
//          pmax = 1 if f largest in absolute values
//          pmax = 2 if g largest in absolute values
//          pmax = 3 if h largest in absolute values

        var pmax = 1
        val swap = ha > fa 
        if (swap) {
            pmax = 3
            tmp = ft; ft = ht; ht = tmp                // swap ft and ht
            tmp = fa; fa = ha; ha = tmp                // swap fa and ha, now fa >= ha
        } // if

        if (ga =~ 0.0) return (ha, fa, 0.0, 1.0, 0.0, 1.0)   // it's already a diagonal matrix

        var gaSmal = true

        if (ga > fa) {
            pmax = 2
            if (fa / ga < EPSILON) {                   // case of very large ga
                gaSmal = false
                ssMax  = ga
                ssMin  = if (ha > 1.0) fa / (ga / ha) else (fa / ga) * ha
                lt     = (1.0, ht / gt)
                rt     = (1.0, ft / gt)
            } // if
        } // if

        if (gaSmal) {                                  // normal case
            val d = fa - ha
            var l = if (d == fa) 1.0 else d / fa       // copes with infinite f or h (note: 0 <= L <= 1)
            val m = gt / ft                            // note: abs (m) <= 1/macheps
            var t = 2.0 - l                            // note: t >= 1
            val mm = m * m
            val tt = t * t
            val s = sqrt (tt + mm)                     // note: 1 <= s <= 1 + 1/macheps
            val r = if (l =~ 0.0) abs (m)
                    else          sqrt (l * l + mm )   // note: 0 <= r <= 1 + 1/macheps
            val a = 0.5 * (s+r)                        // note: 1 <= a <= 1 + abs (m)

            ssMin = ha / a                             // initial values for signular values
            ssMax = fa * a

            if (mm =~ 0.0) {                           // note: m is very tiny
                t = if (l =~ 0.0) sign (2.0, ft) * sign (1.0, gt)
                    else          gt / sign (d, ft) + m / t
            } else {
                 t = (m / (s + t) + m / (r + l)) * (1.0 + a)
            } // if
            l  = sqrt (t*t + 4.0)

            rt = (2.0 / l, t / l)                      // initial values for signular vectors
            lt = ((rt._1 + rt._2 * m) / a, (ht / ft) * rt._2 / a)
        } // if

        if (swap) {
            left  = (rt._2, rt._1)
            right = (lt._2, lt._1)
        } else {
            left  = lt
            right = rt
        } // if

        val sigv = correctSigns (pmax)                 // correct signs for singular values
        (sigv._1, sigv._2, right._1, right._2, left._1, left._2)
    } // deflateV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Correct signs of singular values 'ssMin' and 'ssMax'.
     *  @param pmax
     */
    private def correctSigns (pmax: Int): Tuple2 [Double, Double] =
    {
        val tsign = pmax match {
             case 1 => sign (1.0, right._1) * sign (1.0, left._1) * sign (1.0, f)
             case 2 => sign (1.0, right._2) * sign (1.0, left._1) * sign (1.0, g)
             case 3 => sign (1.0, right._2) * sign (1.0, left._2) * sign (1.0, h)
        } // match
        (sign (ssMin, tsign * sign (1.0, f) * sign (1.0, h)), sign (ssMax, tsign))
    } // correctSigns

} // SVD_2by2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD_2by2Test` is used to test the `SVD_2by2` class.
 */
object SVD_2by2Test extends App
{
    import Eigen_2by2.eigenvalues

    val a = new MatrixD ((2, 2), 1.0, 1.0,
                                 0.0, 2.0)

    val svd = new SVD_2by2 (a(0, 0), a(0, 1), a(1, 1))

    println ("----------------------------------------")
    println ("Test SVD_2by2")
    println ("----------------------------------------")
    println ("a = " + a)
    println ("----------------------------------------")
    println ("singular values  = " + svd.deflate ())
    println ("----------------------------------------")
    println ("singular val/vec = " + svd.deflateV ())

    println ("----------------------------------------")
    println ("Compare to Eigen_2by2")
    println ("----------------------------------------")
    println ("root of eigenvalues = " + eigenvalues (a.t * a).map (sqrt _))

} // SVD_2by2Test object

