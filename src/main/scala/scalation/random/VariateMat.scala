
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Mar  9 19:19:53 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import math.{abs, exp, Pi, round, sqrt}

import scalation.linalgebra.{MatrixD, MatrixI}
import scalation.math.double_exp
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VariateMat` abstract class serves as a base class for all the random
 *  variate matrix (RVM) generators. They use one of the Random Number Generators
 *  (RNG's) from Random.scala to generate numbers following their particular
 *  multivariate distribution.
 *-----------------------------------------------------------------------------
 *  @param stream  the random number stream
 */
abstract class VariateMat (stream: Int = 0)
         extends Error
{
    /** Random number stream selected by the stream number
     */
    protected val r = Random (stream)

    /** Indicates whether the distribution is discrete or continuous (default)
     */
    protected var _discrete = false

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the distribution is discrete or continuous.
     */
    def discrete: Boolean = _discrete

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the matrix mean for the particular distribution.
     */
    def mean: MatrixD

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability function (pf):
     *  The probability density function (pdf) for continuous RVM's or
     *  the probability mass function (pmf) for discrete RVM's.
     *  @param z  the mass point/matrix whose probability is sought
     */
    def pf (z: MatrixD): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random double matrix for the particular distribution.
     */
    def gen: MatrixD

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random integer matrix for the particular distribution.
     *  It is only valid for discrete random variates.
     */
    def igen: MatrixI

} // VariateMat class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomMatD` class generates a random matrix of doubles.
 *  @param dim1     the number of rows in the matrix
 *  @param dim2     the number of columns in the matrix
 *  @param max      generate integers in the range 0 (inclusive) to max (inclusive)
 *  @param min      generate integers in the range 0 (inclusive) to max (inclusive)
 *  @param density  sparsity basis = 1 - density
 *  @param stream   the random number stream
 */
case class RandomMatD (dim1: Int = 10, dim2: Int = 10, max: Double = 20.0, min: Double = 0.0,
                       density: Double = 1.0, stream: Int = 0)
     extends VariateMat (stream)
{
    private val mu   = (min + max) / 2.0                             // mean
    private val rvec = RandomVecD (dim2, max, min, density, stream)  // random vector generator
    
    def mean: MatrixD = { val mv = new MatrixD (dim1, dim2); mv.set (mu); mv }

    def pf (z: MatrixD): Double = 1.0 / (max - min) ~^ (dim1 + dim2)

    def igen: MatrixI = gen.toInt

    def gen: MatrixD =
    {
        val mat = new MatrixD (dim1, dim2)
        for (i <- mat.range1) mat(i) = rvec.gen
        mat
    } // gen

} // RandomMatD class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VariateMatTest` object is used to test the Random Variate Matrix (RVM)
 *  generators from the classes derived from `VariateMat`.
 *  > run-main scalation.random.VariateMatTest
 */
object VariateMatTest extends App
{
     var rvm: VariateMat = null                                // variate matrix

     println ("Test: RandomMatD random matrix generation --------------------")
     rvm = RandomMatD ()                              // random matrix generator
     println ("mean = " + rvm.mean)
     for (k <- 0 until 30) println (rvm.gen)

} // VariateMatTest object

