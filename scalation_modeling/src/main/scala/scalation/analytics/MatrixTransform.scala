
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat Jan 31 20:59:02 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.sqrt

import scalation.linalgebra.{FunctionV_2V, MatriD, MatrixD, VectoD, VectorD}
import scalation.math.double_exp
import scalation.minima.G_SECTION
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixTransform` object is used to transform the columns of a data matrix 'x'.
 *  Such pre-processing of the data is required by some modeling techniques.
 */
object MatrixTransform
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'j' to all ones.
     *  @param x  the given matrix
     */
    def setCol2One (x: MatriD, j: Int = 0)
    {
        x.setCol (0, VectorD.one (x.dim1))
    } // setCol2One

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum value for each column in the matrix.
     *  @param x  the given matrix
     */
    def max (x: MatriD): VectorD = VectorD (for (j <- x.range2) yield x.col(j).max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum value for each column in the matrix.
     *  @param x  the given matrix
     */
    def min (x: MatriD): VectorD = VectorD (for (j <- x.range2) yield x.col(j).min ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the standard deviation for the given vector.
     *  @param x  the given vector
     */
    def stddev (x: VectoD): Double = sqrt (x.variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the standard deviation for each column in the matrix.
     *  @param x  the given matrix
     */
    def stddev (x: MatriD): VectorD = VectorD (for (j <- x.range2) yield sqrt (x.col(j).variance))

// Vector Transformations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the extreme values (min, max) for vector 'x'.
     *  @param x  the vector whose extreme values are sought
     */
    def extreme (x: VectoD): PairD = (x.min (), x.max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean and standard deviation stats for vector 'x'
     *  @param x  the vector whose stats are sought
     */
    def mean_stddev (x: VectoD): PairD = (x.mean, sqrt (x.variance))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale vector 'x' to the range 'lb' to 'ub': 'x -> x_s'.
     *  @param x         the vector to scale
     *  @param extremes  the (minimum value, maximum value) in vector x
     *  @param bounds    the desired (lower, upper) bounds
     */
    def scaleV (extremes: PairD, bounds: PairD) (x: VectoD): VectoD =
    {
        val (min_x, max_x) = extremes
        val (lb, ub) = bounds
        val scale = (ub - lb) / (max_x - min_x)
        (x - min_x) * scale + lb                                   // shift and scale
    } // scaleV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unscale vector 'x_s' from the range 'lb' to 'ub' to original range: 'x_s -> x'.
     *  @param x_s       the vector to unscale
     *  @param extremes  the (minimum value, maximum value) in original vector x
     *  @param bounds    the scaled (lower, upper) bounds
     */
    def unscaleV (extremes: PairD, bounds: PairD) (x_s: VectoD): VectoD =
    {
        val (min_x, max_x) = extremes
        val (lb, ub) = bounds
        val scale = (ub - lb) / (max_x - min_x)
        (x_s - lb) / scale + min_x                                 // shift and scale
    } // unscaleV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize the vector 'x' to zero mean and unit standard deviation,
     *  by subtracting the mean and dividing by the standard deviation.
     *  @param x       the vector to normalize
     *  @param mu_sig  the column vector's mean and standard deviation
     */
    def normalizeV (mu_sig: PairD) (x: VectoD): VectoD =
    {
        val (mu_x, sig_x) = mu_sig
        (x - mu_x) / sig_x
    } // normalizeV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Denormalize the vector 'x_n' from zero mean and unit standard deviation,
     *  by multiplying by the standard deviation and adding the mean.
     *  @param x_n     the vector to denormalize
     *  @param mu_sig  the column vector's mean and standard deviation
     */
    def denormalizeV (mu_sig: PairD) (x_n: VectoD): VectoD =
    {
        val (mu_x, sig_x) = mu_sig
        x_n * sig_x + mu_x
    } // denormalizeV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the vector by taking the 'G_SECTION' power (weaker than a square root).
     *  @param x  the vector to make golden
     */
    def golden (x: VectoD): VectoD = 
    {
        VectorD (for (i <- x.range) yield if (x(i) < 0.0) -(-x(i) ~^ G_SECTION) 
                                          else            x(i) ~^ G_SECTION)
    } // golden

// Matrix Transformations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the extreme values (min, max) for matrix 'x', for each column.
     *  @param x  the matrix whose extreme values are sought
     */
    def extreme (x: MatriD): PairV = (min (x), max (x))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Center matrix 'x' to zero mean, column-wise, by subtracting the mean.
     *  @param x     the matrix to center
     *  @param mu_x  the vector of column means of matrix x
     */
    def center (x: MatriD, mu_x: VectoD): MatriD =
    {
        val x_c = new MatrixD (x.dim1, x.dim2)
        for (j <- x.range2) x_c.setCol (j, x.col(j) - mu_x(j))     // subtract column means
        x_c
    } // center

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Uncenter matrix 'x_c' from zero mean, column-wise, by adding the mean.
     *  @param x_c   the matrix to uncenter
     *  @param mu_x  the vector of column means of matrix x_c
     */
    def uncenter (x_c: MatriD, mu_x: VectoD): MatriD =
    {
        val x = new MatrixD (x_c.dim1, x_c.dim2)
        for (j <- x.range2) x.setCol (j, x_c.col(j) + mu_x(j))     // add column means
        x
    } // uncenter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale matrix 'x' to the range 'lb to 'ub', column-wise: 'x -> x_s'.
     *  @param x       the matrix to scale
     *  @param min_x   the vector of column minima of matrix x
     *  @param max_x   the vector of column maxima of matrix x
     *  @param bounds  the desired (lower, upper) bounds
     */
    def scale (x: MatriD, extremes: PairV, bounds: PairD): MatriD =
    {
        val (lb, ub) = bounds
        val (min_x, max_x) = extremes
        val x_s = new MatrixD (x.dim1, x.dim2)
        for (j <- x.range2) {
            val scale = (ub - lb) / (max_x(j) - min_x(j))
            x_s.setCol (j, (x.col(j) - min_x(j)) * scale + lb)     // shift and scale
        } // for
        x_s
    } // scale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unscale matrix 'x_s' from the range 'lb' to 'ub', column-wise: 'x_s -> x'.
     *  @param x_s     the matrix to unscale
     *  @param min_x   the vector of column minima of original matrix x
     *  @param max_x   the vector of column maxima of original matrix x
     *  @param bounds  the scaled (lower, upper) bounds
     */
    def unscale (x_s: MatriD, extremes: PairV, bounds: PairD): MatriD =
    {
        val (lb, ub) = bounds
        val (min_x, max_x) = extremes
        val x = new MatrixD (x_s.dim1, x_s.dim2)
        for (j <- x.range2) {
            val scale = (ub - lb) / (max_x(j) - min_x(j))
            x.setCol (j, (x_s.col(j) - lb) / scale + min_x(j))     // scale and shift
        } // for
        x
    } // unscale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize the matrix 'x' to zero mean and unit standard deviation,
     *  column-wise, by subtracting the mean and dividing by the standard deviation.
     *  @param x      the matrix to normalize
     *  @param mu_x   the vector of column means of matrix x
     *  @param sig_x  the vector of column standard deviations of matrix x
     */
    def normalize (x: MatriD, mu_sig: PairV): MatriD =
    {
        val (mu_x, sig_x) = mu_sig
        val x_n = new MatrixD (x.dim1, x.dim2)
        for (j <- x.range2) x_n.setCol (j, (x.col(j) - mu_x(j)) / sig_x(j))
        x_n
    } // normalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Denormalize the matrix 'x_n' from zero mean and unit standard deviation,
     *  column-wise, by multiplying by the standard deviation and adding the mean.
     *  @param x_n    the matrix to denormalize
     *  @param mu_x   the vector of column means of matrix x_n
     *  @param sig_x  the vector of column standard deviations of matrix x_n
     */
    def denormalize (x_n: MatriD, mu_sig: PairV): MatriD =
    {
        val (mu_x, sig_x) = mu_sig
        val x = new MatrixD (x_n.dim1, x_n.dim2)
        for (j <- x.range2) x.setCol (j, x_n.col(j) * sig_x(j) + mu_x(j))
        x
    } // denormalize

} // MatrixTransform object

import MatrixTransform._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixTransformTest` is used to test the `MatrixTransform` object.
 *  It tests centering, scaling and normalization.
 *  > runMain scalation.analytics.MatrixTransformTest
 */
object MatrixTransformTest extends App
{
    val x = new MatrixD ((3, 3), 1, 2, 3,
                                 4, 5, 6,
                                 7, 8, 9)

    val (min_x, max_x) = (min (x), max (x))
    val (mu_x, sig_x)  = (x.mean, stddev (x))

    println (s"(min_x, max_x) = ($min_x, $max_x)")
    println (s"(mu_x, sig_x) = ($mu_x, $sig_x)")

    val x_c  = center (x, mu_x)
    val x_s  = scale (x, (min_x, max_x), (0, 1))     // e.g., used for sigmoid activation function
    val x_s2 = scale (x, (min_x, max_x), (-1, 1))    // e.g., used by tanh activation function
    val x_n  = normalize (x, (mu_x, sig_x))          // e.g., used for unbounded activation function

    println ("x    = " + x)
    banner ("Center at 0")
    println ("x_c  = " + x_c)
    banner ("Scale to (0, 1)")
    println ("x_s  = " + x_s)
    banner ("Scale to (-1, 1)")
    println ("x_s2 = " + x_s2)
    banner ("Normalize to (mu = 0, sig = 1)")
    println ("x_n  = " + x_n)

    assert (uncenter (x_c, mu_x) == x, "uncenter")
    assert (unscale (x_s,  (min_x, max_x), (0, 1))  == x, "unscale")
    assert (unscale (x_s2, (min_x, max_x), (-1, 1)) == x, "unscale")
    assert (denormalize (x_n, (mu_x, sig_x)) == x, "denormalize")

} // MatrixTransformTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixTransformTest2` is used to test the `MatrixTransform` object.
 *  It tests usage of `TransformV` functional variables.
 *  > runMain scalation.analytics.MatrixTransformTest2
 */
object MatrixTransformTest2 extends App
{
    val x = VectorD (1, 2, 3, 4, 5, 6)

    val extremes = extreme (x)
    val bounds   = (0.0, 1.0)
    val mu_sig   = mean_stddev (x)

    println (s"extremes = $extremes")
    println (s"mu_sig   = $mu_sig")

    var tran:  FunctionV_2V = scaleV (extremes, bounds)      // tranform - partially applied function
    var itran: FunctionV_2V = unscaleV (extremes, bounds)    // inverse transform

    val x_s = tran (x)
    val x_u = itran (x_s)

    banner ("Test scaling")
    println (s"x_s = $x_s")
    println (s"x_u = $x_u")

    assert (x_u == x, "unscale")

    tran  = normalizeV (mu_sig)                              // tranform - partially applied function
    itran = denormalizeV (mu_sig)                            // inverse transform

    val x_n = tran (x)
    val x_d = itran (x_n)

    banner ("Test normalization")
    println (s"x_n = $x_n")
    println (s"x_d = $x_d")

    assert (x_d == x, "denormalize")

} // MatrixTransformTest2 object

