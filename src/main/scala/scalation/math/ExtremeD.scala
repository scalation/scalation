
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sat May 24 14:48:00 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import math.abs

import DoubleWithExp._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExtremeD` object contains constants representing extreme values for
 *  Double (IEEE 754 double precision floating point numbers).
 *  @see en.wikipedia.org/wiki/Double-precision_floating-point_format
 */
object ExtremeD
{
    /** Smallest double such that 1.0 + EPSILON != 1.0, slightly above 2^-53.
     *  Also, known as the "machine epsilon".
     *  @see https://issues.scala-lang.org/browse/SI-3791
     */
    val EPSILON = 1.1102230246251568E-16              // 1 + EPSILON okay
    val _2_53 = 2 ~^ -53                              // 1 + 2^-53 underflows

    /** Largest exponent a finite double variable may have. 
     */
    val MAX_EXPONENT = java.lang.Double.MAX_EXPONENT

    /** Largest positive finite value of type double, 2^1023
     */
    val MAX_VALUE = java.lang.Double.MAX_VALUE

    /** Smallest exponent a normalized double variable may have.
     */
    val MIN_EXPONENT = java.lang.Double.MIN_EXPONENT

    /** Smallest positive normal value of type double, 2^-1022 (retains full precision).
     *  Also, the smallest double such that 1.0 / SAFE_MIN does not overflow.
     */
    val MIN_NORMAL = java.lang.Double.MIN_NORMAL

    /** Smallest positive nonzero value of type double, 2^-1074 (minimal precision).
     */
    val MIN_VALUE = java.lang.Double.MIN_VALUE

    /** Default tolerance should be much larger than the "machine epsilon".
     *  Application dependent => redefine as needed per application.
     */
    val TOL = 100.0 * EPSILON

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return true if x == y approximately, i.e., the difference is less than TOL.
     *  @param x  the first value to compare
     *  @param y  the second value to compare
     */
    def approx (x: Double, y: Double): Boolean =  abs (x - y) < TOL

} // ExtremeD


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExtremeDTest` object is used to test the `ExtremeD` class.
 */
object ExtremeDTest extends App
{
    import ExtremeD._

    println ("MAX_EXPONENT  = " + MAX_EXPONENT)
    println ("MAX_VALUE     = " + MAX_VALUE)
    println ("MIN_EXPONENT  = " + MIN_EXPONENT)
    println ("MIN_NORMAL    = " + MIN_NORMAL)
    println ("MIN_VALUE     = " + MIN_VALUE)
    println ("TOL           = " + TOL)

    println ("EPSILON       = " + EPSILON)
    println ("1 + EPSILON   = " + (1.0 + EPSILON))      // still okay
    println ("_2_53         = " + _2_53)
    println ("1 + _2_53     = " + (1.0 + _2_53))        // underflow

    println ("approx (5.0, 5.0) = " + approx (5.0, 5.0))
    println ("approx (5.0, 5.1) = " + approx (5.0, 5.1))

} // ExtremeDTest object

