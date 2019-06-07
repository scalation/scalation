
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat Apr 29 13:22:47 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see introcs.cs.princeton.edu/java/92symbolic/Polynomial.java.html
 */
package scalation.calculus

import scala.math.abs

import scalation.linalgebra.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Poly` class provides operations on univariate polynomials.
 *  <p>
 *      Poly (2, 3) => 3 x + 2
 *  <p>
 *  Note, reverse order of coefficients, i.e., coefficients for smallest terms first.
 *  @see `MPoly' for multivariate polynomials.
 *  @param c  the coefficients of the polynomial
 *  @param x  the variable/indeterminate of the polynomial
 */
case class Poly (c: VectorD, x: String = "x")
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
    def derivative: Poly = Poly ((for (i <- 1 to deg) yield i * c(i)) :_*)

    def Ⅾ : Poly = Poly ((for (i <- 1 to deg) yield i * c(i)) :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Integrate 'this' polynomial, returning the result as a polynomial.
     *  Note, the arbitrary constant 'c' for the indefinite integral is set to 1.
     */
    def integrate: Poly = Poly (1.0 +: (for (i <- 0 to deg) yield c(i) / (i+1.0)) :_*)

    def ∫ : Poly = Poly (1.0 +: (for (i <- 0 to deg) yield c(i) / (i+1.0)) :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Integrate 'this' polynomial on the interval 'on', returning its value as
     *  a double.
     *  @param on  the interval of integration
     */
    def integrate (on: Interval): Double =
    {
        val pl = Poly (1.0 +: (for (i <- 0 to deg) yield c(i) / (i+1.0)) :_*)
        pl (on._2) - pl (on._1)
    } // integrate

    def ∫ (on: Interval): Double =
    {
        val pl = Poly (1.0 +: (for (i <- 0 to deg) yield c(i) / (i+1.0)) :_*)
        pl (on._2) - pl (on._1)
    } // ∫

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Trim away trailing zero coefficients (i.e., those on highest order terms),
     *  returning the resulting polynomial of possibly lower degree.
     */
    def trim: Poly = 
    {
        var i = deg; while (c(i) == 0.0) i -= 1          // skip trailing zeros
        Poly (c.slice (0, i+1))                          // keep the rest
    } // trim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse a readable/LaTeX-compatible string to create a polynomial, using
     *  a PEG parser.
     *  @see https://github.com/sirthias/parboiled2
     *  @param str  the string to parse, e.g., "2.0 x^3 + 3.0 x^2 + 4.0 x + 5.0"
     */
    def parse (str: String): Poly =
    {
        null
    } // parse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parse a compilable Scala expression string to create a polynomial, using
     *  a PEG parser.
     *  @see https://github.com/sirthias/parboiled2
     *  @param str  the string to parse, e.g., "2.0*x~^3 + 3.0*x~^2 + 4.0*x + 5.0"
     */
    def parse2 (str: String): Poly =
    {
        null
    } // parse2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the polynomial to a readable/LaTeX-compatible string.
     */
    override def toString: String =
    {
        val sb = new StringBuilder ()
        for (i <- deg to 0 by -1) sb.append (if (i >= 2)      s"${c(i)} $x^$i + "
                                             else if (i == 1) s"${c(i)} $x + "
                                             else             s"${c(i)}")
        sb.toString
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the polynomial to an compilable Scala expression string.
     */
    def toString2: String =
    {
        val sb = new StringBuilder ()
        for (i <- deg to 0 by -1) sb.append (if (i >= 2)      s"${c(i)}*$x~^$i + "
                                             else if (i == 1) s"${c(i)}*$x + "
                                             else             s"${c(i)}")
        sb.toString
    } // toString2

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a polynomial from repeated doubles.
     *  @param x  the variable/indeterminate of the polynomial
     *  @param c  the coefficients as a repeated double
     */
    def apply (x: String, c: Double*): Poly = Poly (VectorD (c), x)

} // Poly object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PolyTest` object is used to test the `Poly` class.
 *  > runMain scalation.calculus.PolyTest
 */
object PolyTest extends App
{
    import scalation.plot.Plot

    val pl  = Poly (5.0, 4.0, 3.0, 2.0)             // example polynomial: 2 x^3 + 3 x^2 + 4 x + 5
    val dpl = pl.derivative                         // its derivative
    val ipl = pl.integrate                          // one of its indefinite integrals
    val jpl = pl.integrate ((0.0, 2.0))             // one of its definite integrals
    val spl = pl + dpl                              // sum of polynomials and its dervivate
    val mpl = pl - dpl                              // difference of polynomial and its dervivate
    val tpl = pl * dpl                              // product of polynomial and its dervivate
    val zpl = Poly (4.0, 0.0, 3.0, 0.0, 0.0)        // polynomial with trailing zero (e.g., 0 x^2)

    println (s"pl      = $pl")
    println (s"pl.2    = ${pl.toString2}")
    println (s"dpl     = $dpl")
    println (s"ipl     = $ipl")
    println (s"spl     = $spl")
    println (s"mpl     = $mpl")
    println (s"tpl     = $tpl")
    println (s"zpl     = $zpl")
    println (s"t(zpl)  = ${zpl.trim}")
    println (s"pl (2)  = ${pl (2)}")
    println (s"dpl (2) = ${dpl (2)}")
    println (s"ipl (2) = ${ipl (2)}")
    println (s"jpl     = $jpl")
    println (s"spl (2) = ${spl (2)}")
    println (s"mpl (2) = ${mpl (2)}")
    println (s"tpl (2) = ${tpl (2)}")

    val x = VectorD.range (0, 20) / 5.0
    val y = x.map (pl (_))
    val z = x.map (dpl (_))

    new Plot (x, y, z)
    
} // PolyTest object

