
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat Apr 29 13:22:47 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see introcs.cs.princeton.edu/java/92symbolic/Polynomial.java.html
 */
package scalation.calculus

import scala.math.abs

import scalation.linalgebra.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Poly` class provides simple operations on polynomials.
 *  <p>
 *      Poly (2, 3) => 3 x + 2
 *  <p>
 *  Note, reverse order of coefficients, i.e., coefficients for smallest terms first.
 *  @param c  the coefficients of the polynomial
 */
case class Poly (c: VectorD)
{
    private val DEBUG = true                         // debug flag
    val deg           = c.size - 1                   // degree of the polynomial

    if (DEBUG) println (s"Poly ($c) has degree $deg")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply/evaluate the polynomial at 'x'.
     *  @param x  the value of the variable
     */
    def apply (x: Double): Double =
    {
        var sum = 0.0
        for (i <- deg to 0 by -1) sum = x * sum + c(i) 
        sum
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' polynomial and the 'q' polynomial.
     *  @param q  the other polynomial
     */
    def + (q: Poly): Poly =
    {
        if (deg < q.deg) Poly (c + q.c ++ q.c.slice (deg+1, q.deg+1))
        else             Poly (q.c + c ++ c.slice (q.deg+1, deg+1))
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract the 'q' polynomial from 'this' polynomial.
     *  @param q  the other polynomial
     */
    def - (q: Poly): Poly =
    {
        if (deg < q.deg) Poly (c - q.c ++ q.c.slice (deg+1, q.deg+1))
        else             Poly (-q.c + c ++ c.slice (q.deg+1, deg+1))
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' polynomial and the 'q' polynomial.
     *  @param q  the other polynomial
     */
    def * (q: Poly): Poly =
    {
        val cc = new VectorD (deg + q.deg + 1)
        for (i <- 0 to deg; j <- 0 to q.deg) cc(i+j) += c(i) * q.c(j)
        Poly (cc)
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the derivative of 'this' polynomial, returning the result as a polynomial.
     */
    def derv: Poly =
    {
        Poly ((for (i <- 1 to deg) yield i * c(i)) :_*)
    } // derv

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the polynomial to a string.
     */
    override def toString: String =
    {
        val sb = new StringBuilder ()
        for (i <- deg to 0 by -1) sb.append (if (i >= 2)      s"${c(i)} x^$i + "
                                             else if (i == 1) s"${c(i)} x + "
                                             else             s"${c(i)}")
        sb.toString
    } // toString

} // Poly class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Poly` companion object provides factory methods for the 'Poly' class.
 */
object Poly
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a polynomial from repeated doubles.
     *  @param c  the coefficients as a repeated double
     */
    def apply (c: Double*): Poly = Poly (VectorD (c))

} // Poly object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PolyTest` object is used to test the `Poly` class.
 *  > run-main scalation.calculus.PolyTest
 */
object PolyTest extends App
{
    import scalation.plot.Plot

    val pl  = Poly (4.0, 3.0, 2.0)                  // example polynomial: 2 x^2 + 3 x + 4
    val dpl = pl.derv                               // its derivative
    val spl = pl + dpl                              // sum of polynomials and its dervivate
    val mpl = pl - dpl                              // difference of polynomial and its dervivate
    val tpl = pl * dpl                              // product of polynomial and its dervivate

    println (s"pl      = $pl")
    println (s"dpl     = $dpl")
    println (s"spl     = $spl")
    println (s"mpl     = $mpl")
    println (s"tpl     = $tpl")
    println (s"pl (2)  = ${pl (2)}")
    println (s"dpl (2) = ${dpl (2)}")
    println (s"spl (2) = ${spl (2)}")
    println (s"mpl (2) = ${mpl (2)}")
    println (s"tpl (2) = ${tpl (2)}")

    val x = VectorD.range (0, 20) / 5.0
    val y = x.map (pl (_))
    val z = x.map (dpl (_))

    new Plot (x, y, z)
    
} // PolyTest object

