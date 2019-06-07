
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miler
 *  @version 1.6
 *  @date    Thu Jan 31 15:08:27 EST 2019
 *  @see     LICENSE (MIT style license file).
 *-----------------------------------------------------------------------------
 *  Convention:
 *      'x'    - data/input matrix (may have an intercept column)
 *      'y'    - response/output vector
 *      'ox'   - data matrix with an intercept column of ones prepended
 *      'xy'   - combined data matrix
 *      'oxy'  - combined data matrix with an intercept column of ones prepended
 *      't'    - index for the data points (instances 0, 1, ... m-1)
 *
 *  Note: for studies where only models with intercepts are considered,
 *  ox and oxy are not needed (the ones column will already be in x).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleBPressure` object stores the Blood Pressure dataset in a matrix.
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 */
object ExampleBPressure
{
    /** the variable/feature names
     */
    val fname = Array ("age", "weight", "dur", "stress")

    /** the variable/feature names including intercept ("one")
     */
    val ofname = Array ("one") ++ fname

    /** Blood Pressure (BP) data/input matrix
     *  20 data points:       t    x_1     x_2    x_3      x_4
     *                             Age  Weight    Dur   Stress
     */
    val x = new MatrixD ((20, 4), 47.0,   85.4,   5.1,    33.0,     // 1
                                  49.0,   94.2,   3.8,    14.0,     // 2
                                  49.0,   95.3,   8.2,    10.0,     // 3
                                  50.0,   94.7,   5.8,    99.0,     // 4
                                  51.0,   89.4,   7.0,    95.0,     // 5
                                  48.0,   99.5,   9.3,    10.0,     // 6
                                  49.0,   99.8,   2.5,    42.0,     // 7
                                  47.0,   90.9,   6.2,     8.0,     // 8
                                  49.0,   89.2,   7.1,    62.0,     // 9
                                  48.0,   92.7,   5.6,    35.0,     // 10

                                  47.0,   94.4,   5.3,    90.0,     // 11
                                  49.0,   94.1,   5.6,    21.0,     // 12
                                  50.0,   91.6,  10.2,    47.0,     // 13
                                  45.0,   87.1,   5.6,    80.0,     // 14
                                  52.0,  101.3,  10.0,    98.0,     // 15
                                  46.0,   94.5,   7.4,    95.0,     // 16
                                  46.0,   87.0,   3.6,    18.0,     // 17
                                  46.0,   94.5,   4.3,    12.0,     // 18
                                  48.0,   90.5,   9.0,    99.0,     // 19
                                  56.0,   95.7,   7.0,    99.0)     // 20

    /** Blood Pressure (BP) response/output vector
     */
    val y = VectorD (105.0, 115.0, 116.0, 117.0, 112.0, 121.0, 121.0, 110.0, 110.0, 114.0,
                     114.0, 115.0, 114.0, 106.0, 125.0, 114.0, 106.0, 113.0, 110.0, 122.0)

    /** the first two columns (0 and 1) of data matrix 'x'
     */
    val x01 = x.sliceCol (0, 2)

    /** the combined data matrix of data matrix 'x' with the response column 'y' appended
     */
    val xy = x :^+ y

    /** the data matrix 'x' with a column of all ones prepended for intercept models
     */
    val ox = VectorD.one (x.dim1) +^: x

    /** the data combined matrix with a column of all ones prepended for intercept models
     */
    val oxy = ox :^+ y

    /** index for the data points (instances)
     */
    val t = VectorD.range (0, y.dim)

} // ExampleBPressure object

import ExampleBPressure._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleBPressureTest` object test the `ExampleBPressure` by printing out
 *  of the vectors and matrices.
 *  > runMain scalation.analytics.ExampleBPressureTest
 */
object ExampleBPressureTest extends App
{
    println (s"fname  = ${fname.deep}")
    println (s"x      = $x")
    println (s"y      = $y")
    println (s"t      = $t")
    println (s"xy     = $xy")
    println (s"ofname = ${ofname.deep}")
    println (s"ox     = $ox")
    println (s"oxy    = $oxy")

    new Plot (t, y, null, "y vs. t")

} // ExampleBPressureTest object

