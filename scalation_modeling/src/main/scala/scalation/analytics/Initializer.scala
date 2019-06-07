
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.6
 *  @date    Sun Jan 27 15:34:08 EST 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.sqrt

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math.double_exp
import scalation.random.{Normal, RandomMatD, RandomVecD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Initializer` object provides functions to initialize the parameters/weights
 *  of Neural Networks.  Supports Uniform, Normal and Nguyen & Widrow methods
 */
object Initializer
{

// Technique 1: Uniform distribution

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return value limit based on number of rows.
     *  @param rows  the number of rows
     */
    private def limitF (rows: Int): Double = 1.0 / sqrt (rows)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random weight/parameter matrix with elements values in (0, limit).
     *  @param rows    the number of rows
     *  @param limit   the maximum value for any weight
     *  @param stream  the random number stream to use
     */
    def weightVec (rows: Int, stream: Int = 0, limit: Double = -1.0): VectoD =
    {
        val lim = if (limit <= 0.0) limitF (rows) else limit
        val rvg = new RandomVecD (rows, lim, 0.0, stream = stream)       // change stream for different random numbers
        rvg.gen
    } // weightVec

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random weight/parameter matrix with elements values in (0, limit).
     *  @param rows    the number of rows
     *  @param cols    the number of columns
     *  @param limit   the maximum value for any weight
     *  @param stream  the random number stream to use
     */
    def weightMat (rows: Int, cols: Int, stream: Int = 0, limit: Double = -1.0): MatriD =
    {
        val lim = if (limit <= 0.0) limitF (rows) else limit
        val rmg = new RandomMatD (rows, cols, lim, stream = stream)      // change stream for different random numbers
        rmg.gen
    } // weightMat

// Technique 2: Standard Normal distribution

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random weight/parameter matrix with elements values from the
     *  Standard Normal distribution.
     *  @param rows    the number of rows
     *  @param stream  the random number stream to use
     */
    def weightVec2 (rows: Int, stream: Int = 0): VectoD =
    {
        val normal = new Normal (stream = stream)
        val vec    = new VectorD (rows)
        for (i <- vec.range) vec(i) = normal.gen
        vec
    } // weightVec2
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random weight/parameter matrix with elements values from the
     *  Standard Normal distribution.
     *  @param rows    the number of rows
     *  @param cols    the number of columns
     *  @param stream  the random number stream to use
     */
    def weightMat2 (rows: Int, cols: Int, stream: Int = 0): MatriD =
    {
        val normal = new Normal (stream = stream)
        val denom  = sqrt (rows)
        val mat    = new MatrixD (rows, cols)
        for (i <- mat.range1; j <- mat.range2) mat(i, j) = normal.gen / denom
        mat
    } // weightMat2

// Technique 3: Nguyen & Widrow Method

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random weight/parameter matrix with elements values from the
     *  Nguyen & Widrow Method.
     *  @see ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6945481
     *  @param rows    the number of rows
     *  @param stream  the random number stream to use
     */
    def weightVec3 (rows: Int, stream: Int = 0): VectoD =
    {
        val beta = 0.7~^(1.0/rows.toDouble)
        val rvg  = new RandomVecD (rows, 1.0, -1.0, stream = stream)       // change stream for different random numbers
        val wb   = rvg.gen
        wb * (beta / wb.norm)
    } // weightVec2
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random weight/parameter matrix with elements values from the
     *  Nguyen & Widrow method.
     *  @see ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6945481
     *  @param rows    the number of rows
     *  @param cols    the number of columns
     *  @param stream  the random number stream to use
     */
    def weightMat3 (rows: Int, cols: Int, stream: Int = 0): MatriD =
    {
        val beta = 0.7~^(1.0/rows.toDouble)
        val rmg  = new RandomMatD (rows, cols, 1.0, -1.0, stream = stream)      // change stream for different random numbers
        val w    = rmg.gen
        w * (beta / w.normF)
    } // weightMat3

} // Initializer object

