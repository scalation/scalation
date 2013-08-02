
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Mar  9 19:19:53 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import math.{abs, exp, Pi, round, sqrt}

import scalation.linalgebra.{Cholesky, MatrixD, VectorD}
import scalation.linalgebra_gen.VectorN
import scalation.linalgebra_gen.Vectors.VectorI
import scalation.math.Combinatorics.fac
import scalation.math.DoubleWithExp._
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This abstract class serves as a base class for all the random variate
 *  vector (RVV) generators. They use one of the Random Number Generators
 *  (RNG's) from Random.scala to generate numbers following their particular
 *  multivariate distribution.
 */
abstract class VariateVec (stream: Int = 0)
         extends Error
{
    /** Random number stream selected by the stream number
     */
    protected val r = RNG.rand (stream)

    /** Indicates whether the distribution is discrete or continuous (default)
     */
    protected var _discrete = false

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the distribution is discrete or continuous.
     */
    def discrete: Boolean = _discrete

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the vector mean for the particular distribution.
     */
    def mean: VectorD

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability function (pf):
     *  The probability density function (pdf) for continuous RVV's or
     *  the probability mass function (pmf) for discrete RVV's.
     *  @param z  the mass point/vector whose probability is sought
     */
    def pf (z: VectorD): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random double vector for the particular distribution.
     */
    def gen: VectorD

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random integer vector for the particular distribution.
     *  It is only valid for discrete random variates.
     */
    def igen: VectorI

} // VariateVec class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates Normal (Gaussian) random variate vectors according
 *  to the Multivariate Normal distribution with mean 'mu' and covariance 'cov'.
 *  This continuous RVV models normally distributed multidimensional data. 
 *  @see http://onlinelibrary.wiley.com/doi/10.1111/1467-9639.00037/pdf
 *  @see http://www.statlect.com/mcdnrm1.htm
 *  @param mu      the mean vector
 *  @param cov     the covariance matrix
 *  @param stream  the random number stream
 */
case class NormalVec (mu: VectorD, cov: MatrixD, stream: Int = 0)
     extends VariateVec (stream)
{
    private val normal = Normal ()                      // generator for standard normals

    private val c_cov = new Cholesky (cov).cholesky ()  // compute Cholesky decomposition of cov

    def mean: VectorD = mu

    def pf (z: VectorD): Double =
    {
        val n    = z.dim.toDouble                       // n-dimensional vectors
        val z_mu = z - mu                               // subtract mean
        val zz   = z_mu dot cov.inverse * z_mu
        exp (-.5 * zz) / sqrt ((2.0 * Pi)~^n * abs (cov.det))
    } // pf

    def gen: VectorD =
    {
        val z = new VectorD (mu.dim)
        for (i <- 0 until mu.dim) z(i) = normal.gen
        c_cov * z + mu                                  // Cholesky covariance * standard Normal + mean
    } // gen

    def igen: VectorI = throw new NoSuchMethodException ("'igen' not implemented, use 'gen' instead")

} // NormalVec


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates random permutations of a vector of doubles.
 *  @see maths-people.anu.edu.au/~brent/pd/Arndt-thesis.pdf
 *  @param x       the vector of doubles to permute
 *  @param stream  the random number stream
 */
case class PermutedVecD (x: VectorD, stream: Int = 0)
     extends VariateVec (stream)
{
    private val mu = x.sum / x.dim.toDouble

    def mean: VectorD =  { val mv = new VectorD (x.dim); mv.set (mu); mv }

    def pf (z: VectorD): Double = 1.0 / fac (x.dim)

    def gen: VectorD = 
    {
        val rng = Randi0 (x.dim-1)                      // random interger generator
        val y = new VectorD (x)                         // copy vector x
        for (i <- 0 until x.dim) {
            val j = rng.igen % (i+1)                    // random integer 0, ... , i
            val t = y(i); y(i) = y(j); y(j) = t         // swap x(i) and x(j)
        } // for
        y
    } // igen

    def igen: VectorI = throw new NoSuchMethodException ("'igen' not implemented, use 'gen' instead")

} // PermutedVecD


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates random permutations of a vector of integers.
 *  @see maths-people.anu.edu.au/~brent/pd/Arndt-thesis.pdf
 *  @param x       the vector of integers to permute
 *  @param stream  the random number stream
 */
case class PermutedVecI (x: VectorI, stream: Int = 0)
     extends VariateVec (stream)
{
    _discrete = true

    private val mu = x.sum / x.dim.toDouble

    def mean: VectorD =  { val mv = new VectorD (x.dim); mv.set (mu); mv }

    def pf (z: VectorD): Double = 1.0 / fac (x.dim)

    def gen: VectorD = throw new NoSuchMethodException ("'gen' not implemented, use 'igen' instead")

    def igen: VectorI =
    {
        val rng = Randi0 (3 * x.dim)                    // random integer generator
        val y = new VectorI (x)                         // copy vector x
        for (i <- 0 until x.dim) {
            val j = rng.igen % (i+1)                    // random integer 0 to i
            val t = y(i); y(i) = y(j); y(j) = t         // swap y(i) and y(j)
        } // for
        y
    } // igen

} // PermutedVecI


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Random Variate Vector (RVV) generators.
 */
object VariateVecTest extends App
{
     println ("Test: NormalVec random vector generation ---------------------")
     val mu  = VectorD (5.0, 5.0)
     val cov = new MatrixD ((2, 2), 2.0, 1.0,
                                    1.0, 2.0)
     val nv = NormalVec (mu, cov)
     println ("mean = " + nv.mean)
     for (k <- 0 until 30) println (nv.gen)

     println ("Test: PermutedVecI random vector generation ---------------------")
     val x  = VectorN (1, 2, 3, 4, 5, 6, 7, 8, 9)
     val rp = PermutedVecI (x)                      // random permutation generator
     println ("mean = " + rp.mean)
     for (k <- 0 until 30) println (rp.igen)

} // VariateVecTest object

