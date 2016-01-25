
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Mar  9 19:19:53 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import math.{abs, exp, Pi, round, sqrt}

import scalation.linalgebra.{Fac_Cholesky, MatrixD, VectorD, VectorI}
import scalation.math.Combinatorics.fac
import scalation.math.double_exp
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VariateVec` abstract class serves as a base class for all the random
 *  variate vector (RVV) generators. They use one of the Random Number Generators
 *  (RNG's) from Random.scala to generate numbers following their particular
 *  multivariate distribution.
 *-----------------------------------------------------------------------------
 *  @param stream  the random number stream
 */
abstract class VariateVec (stream: Int = 0)
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
/** The `ProbabilityVec` class generates a probability vector where the ith
 *  probability is '1/n' with a +/- randomizing displacement of at most 'd'.
 *  Note, the probability vector must add to one.
 *  @param n  the dimension/size of the probability vector
 *  @param d  the randomizing displacement, must be in [0, 1]
 */
case class ProbabilityVec (n: Int, d: Double = 0.5, stream: Int = 0)
     extends VariateVec (stream)
{
     private val mu  = new VectorD (n); mu.set (1.0 / n.toDouble)   // mean
     private val rng = Random (stream)                              // random number generator

     if (d < 0.0 || d > 1.0) flaw ("constructor", "d must be in [0, 1]")

     def mean: VectorD = mu

     def pf (z: VectorD): Double =
     {
         0.0    // FIX
     } // pf

     def gen: VectorD =
     {
        val z = new VectorD (n)
        for (i <- 0 until n) z(i) = 1.0 + (rng.gen - .5) * d
        z / z.sum
     } // gen
    
    def igen: VectorI = throw new NoSuchMethodException ("'igen' not implemented, use 'gen' instead")
     
} // ProbabilityVec class

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NormalVec` class generates Normal (Gaussian) random variate vectors according
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
    private val normal = Normal (0.0, 1.0, stream)           // generator for standard normals
    private val c_cov  = new Fac_Cholesky (cov).factor1 ()   // compute Cholesky Factorization of cov

    def mean: VectorD = mu

    def pf (z: VectorD): Double =
    {
        val n    = z.dim.toDouble                        // n-dimensional vectors
        val z_mu = z - mu                                // subtract mean
        val zz   = z_mu dot cov.inverse * z_mu
        exp (-.5 * zz) / sqrt ((2.0 * Pi)~^n * abs (cov.det))
    } // pf

    def gen: VectorD =
    {
        val z = new VectorD (mu.dim)
        for (i <- 0 until mu.dim) z(i) = normal.gen
        c_cov * z + mu                                   // Cholesky covariance * standard Normal + mean
    } // gen

    def igen: VectorI = throw new NoSuchMethodException ("'igen' not implemented, use 'gen' instead")

} // NormalVec class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PermutedVecD` class generates random permutations of a vector of doubles.
 *  @see maths-people.anu.edu.au/~brent/pd/Arndt-thesis.pdf
 *  @param x       the vector of doubles to permute
 *  @param stream  the random number stream
 */
case class PermutedVecD (x: VectorD, stream: Int = 0)
     extends VariateVec (stream)
{
    private val mu  = x.sum / x.dim.toDouble            // mean
    private val rng = Randi0 (x.dim - 1, stream)        // random integer generator

    def mean: VectorD =  { val mv = new VectorD (x.dim); mv.set (mu); mv }

    def pf (z: VectorD): Double = 1.0 / fac (x.dim)

    def gen: VectorD = 
    {
        val y = new VectorD (x)                         // copy vector x
        for (i <- 0 until x.dim) {
            val j = rng.igen % (i+1)                    // random integer 0, ... , i
            val t = y(i); y(i) = y(j); y(j) = t         // swap x(i) and x(j)
        } // for
        y
    } // igen

    def igen: VectorI = throw new NoSuchMethodException ("'igen' not implemented, use 'gen' instead")

} // PermutedVecD class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PermutedVecI` class generates random permutations of a vector of integers.
 *  @see maths-people.anu.edu.au/~brent/pd/Arndt-thesis.pdf
 *  @param x       the vector of integers to permute
 *  @param stream  the random number stream
 */
case class PermutedVecI (x: VectorI, stream: Int = 0)
     extends VariateVec (stream)
{
    _discrete = true

    private val mu  = x.sum / x.dim.toDouble            // mean
    private val rng = Randi0 (3 * x.dim, stream)        // random integer generator

    def mean: VectorD =  { val mv = new VectorD (x.dim); mv.set (mu); mv }

    def pf (z: VectorD): Double = 1.0 / fac (x.dim)

    def gen: VectorD = throw new NoSuchMethodException ("'gen' not implemented, use 'igen' instead")

    def igen: VectorI =
    {
        val y = new VectorI (x)                         // copy vector x
        for (i <- 0 until x.dim) {
            val j = rng.igen % (i+1)                    // random integer 0 to i
            val t = y(i); y(i) = y(j); y(j) = t         // swap y(i) and y(j)
        } // for
        y
    } // igen

} // PermutedVecI class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomVecI` class generates a random vector of integers.
 *  Ex: (3, 2, 0, 4, 1) has count = 5 and max = 4.
 *  @param count   the size of the vector (number of integer elements)
 *  @param max     generate integers in the range 0 (inclusive) to max (inclusive)
 *  @param skip    skip this number, i.e, do not use it
 *  @param unique  whether the integers must be unique
 *  @param stream  the random number stream
 */
case class RandomVecI (count: Int = 10, max: Int = 20, skip: Int = -1, unique: Boolean = true, stream: Int = 0)
     extends VariateVec (stream)
{
    _discrete = true

    if (unique && max < count-1) flaw ("constructor", "requires max >= count-1")

    private val mu  = max / 2.0                         // mean
    private val rng = Randi0 (max, stream)              // random integer generator

    def mean: VectorD =  { val mv = new VectorD (count); mv.set (mu); mv }

    def pf (z: VectorD): Double = 1.0 / max ~^ count

    def gen: VectorD = throw new NoSuchMethodException ("'gen' not implemented, use 'igen' instead")

    def igen: VectorI =
    {
        val y   = new VectorI (count)
        var num = 0
        for (i <- 0 until count) {
            do num = rng.igen while (unique && (num == skip || i > 0 && (y.slice (0, i) contains num)))
            y(i) = num
        } // for
        y
    } // igen

} // RandomVecI class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Multinomial` random variates vectors.
 *  This discrete RV models the ...?
 *  @see http://www.math.uah.edu/stat/bernoulli/Multinomial.html
 *  @param p       array of probabilities
 *  @param n       the number of independent trials
 *  @param stream  the random number stream
 */
case class Multinomial (p: Array [Double] = Array (.4, .3, .3), n: Int = 5, stream: Int = 0)
     extends Variate (stream)
{
    for (pi <- p if pi < 0 || pi > 1) flaw ("constructor", "parameter pi must be in [0, 1]*")
    if (n <= 0) flaw ("constructor", "parameter n must be positive")
    _discrete = true

    val mean: Double = 0.0   // FIX

    def pf (z: Double): Double =
    {
        // var coeff = choose (n, j(0), j(1))
        0.0   // FIX
    } // pf

    def gen: Double =
    {
        0.0   // FIX
    } // gen

} // Multinomial class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VariateVecTest` object is used to test the Random Variate Vector (RVV)
 *  generators from the classes derived from `VariateVec`.
 *  > run-main scalation.random.VariateVecTest
 */
object VariateVecTest extends App
{
     var vv: VariateVec = null                                // variate vector

     println ("Test: ProbabilityVec random vector generation ----------------")
     vv = ProbabilityVec (10)
     println ("mean = " + vv.mean)              // probability vector generator
     for (k <- 0 until 30) println (vv.gen)

     println ("Test: NormalVec random vector generation ---------------------")
     val mu  = VectorD (5.0, 5.0)
     val cov = new MatrixD ((2, 2), 2.0, 1.0,
                                    1.0, 2.0)
     vv = NormalVec (mu, cov)                  // multivariate normal generator
     println ("mean = " + vv.mean)
     for (k <- 0 until 30) println (vv.gen)

     println ("Test: PermutedVecD random vector generation ------------------")
     val x = VectorD (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
     vv = PermutedVecD (x)                      // random permutation generator
     println ("mean = " + vv.mean)
     for (k <- 0 until 30) println (vv.gen)

     println ("Test: PermutedVecI random vector generation ------------------")
     val y = VectorI (1, 2, 3, 4, 5, 6, 7, 8, 9)
     vv = PermutedVecI (y)                      // random permutation generator
     println ("mean = " + vv.mean)
     for (k <- 0 until 30) println (vv.igen)

     println ("Test: RandomVecI random vector generation --------------------")
     vv = RandomVecI ()                              // random vector generator
     println ("mean = " + vv.mean)
     for (k <- 0 until 30) println (vv.igen)

} // VariateVecTest object

