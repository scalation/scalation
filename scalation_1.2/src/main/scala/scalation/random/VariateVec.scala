
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Mar  9 19:19:53 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import scala.math.{abs, exp, Pi, round, sqrt}

import scalation.linalgebra.{Fac_Cholesky, MatrixD, VectorD, VectorI, VectorS}
import scalation.math.double_exp
import scalation.math.Combinatorics.{choose, fac}
import scalation.math.ExtremeD.NaN
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
/** The `ProbabilityVec` class generates a probability vector where the 'i'th
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
    
    override def igen: VectorI = gen.toInt
     
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
                             .asInstanceOf [VectorD]

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

    def igen: VectorI = gen.toInt

} // NormalVec class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**
 */
case class Dir (alpha: VectorD, stream: Int = 0)
{
    import scalation.math.Combinatorics.gammaF

    private val a_sum = alpha.sum

    def mean: VectorD = alpha / a_sum

    def pf (z: VectorD): Double =
    {
        var prod = 1.0
        for (i <- alpha.range) {
            val a_i = alpha(i)
            prod *= (z(i) ~^ (a_i-1.0)) / gammaF (a_i)
        } // for
        prod * gammaF (a_sum)
    } // pf

    def gen: VectorD =
    {
        null                 // FIX - to be implemented
    } // gen

    def igen: VectorI = gen.toInt

} // Dir class


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

    def igen: VectorI = gen.toInt

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

    def gen: VectorD = igen.toDouble

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
/** The `RandomVecD` class generates a random vector of doubles.
 *  Ex: (3.0, 2.0, 0.0, 4.0, 1.0) has 'dim' = 5 and 'max' = 4.
 *  @param dim        the dimension/size of the vector (number of elements)
 *  @param max        generate integers in the range min (inclusive) to max (inclusive)
 *  @param min        generate integers in the range min (inclusive) to max (inclusive)
 *  @param density    sparsity basis = 1 - density
 *  @param runLength  the maximum run length
 *  @param stream     the random number stream
 */
case class RandomVecD (dim: Int = 10, max: Double = 20.0, min: Double = 0.0,
                       density: Double = 1.0, runLength: Int = 10, stream: Int = 0)
     extends VariateVec (stream)
{
    private val mu  = (max - min) / 2.0                 // mean
    private val rng = Uniform (0.0, max, stream)        // random Double generator
    private val rn  = Random (stream)                   // random number generator
    private val ri  = Randi0 (runLength, stream)        // random integer for repetition
    
    def mean: VectorD = { val mv = new VectorD (dim); mv.set (mu); mv }

    def pf (z: VectorD): Double = 1.0 / (max - min) ~^ dim

    def igen: VectorI = gen.toInt

    def gen: VectorD =
    {
        VectorD (for (i <- 0 until dim) yield if (rn.gen < density) rng.gen else 0.0)
    } // gen

    def repgen: VectorD =
    {
        val v   = new VectorD (dim)
        var cnt = 0
        while (cnt < dim) {
            val x   = rng.gen                    // value
            val rep = ri.igen                    // repetition 
            for (j <- 0 until rep if cnt < dim)  { v(cnt) = x; cnt += 1}
        } // while
        v
    } // repgen

} // RandomVecD class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomVecI` class generates a random vector of integers.
 *  Ex: (3, 2, 0, 4, 1) has 'dim' = 5 and 'max' = 4.
 *  @param dim     the dimension/size of the vector (number of elements)
 *  @param max     generate integers in the range min (inclusive) to max (inclusive)
 *  @param min     generate integers in the range min (inclusive) to max (inclusive)
 *  @param skip    skip this number, i.e, do not use it
 *  @param unique  whether the integers must be unique
 *  @param stream  the random number stream
 */
case class RandomVecI (dim: Int = 10, max: Int = 20, min: Int = 10, skip: Int = -1,
                       unique: Boolean = true, stream: Int = 0)
     extends VariateVec (stream)
{
    _discrete = true

    if (unique && max < dim-1) {
        flaw ("constructor", "requires max >= dim-1")
        throw new IllegalArgumentException ("max too small")
    } // if

    private val mu  = (max - min) / 2.0                 // mean
    private val rng = Randi0 (max, stream)              // random integer generator

    def mean: VectorD =  { val mv = new VectorD (dim); mv.set (mu); mv }

    def pf (z: VectorD): Double = 1.0 / (max - min) ~^ dim

    def gen: VectorD = igen.toDouble

    def igen: VectorI =
    {
        val y   = new VectorI (dim)
        var num = 0
        for (i <- 0 until dim) {
            do num = rng.igen while (unique && (num == skip || i > 0 && (y.slice (0, i) contains num)))
            y(i) = num
        } // for
        y
    } // igen

} // RandomVecI class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomVecS` class generates a random vector of integers.
 *  Ex: (3, 2, 0, 4, 1) has 'dim' = 5 and 'max' = 4.
 *  @param dim     the dimension/size of the vector (number of elements)
 *  @param unique  whether the strings must be unique
 *  @param stream  the random number stream
 */
case class RandomVecS (dim: Int = 10, unique: Boolean = true, stream: Int = 0)
     extends VariateVec (stream)
{
    _discrete = true

    private val mu  = NaN                               // mean
    private val rng = RandomStr (stream = stream)       // random string generator

    def mean: VectorD =  { val mv = new VectorD (dim); mv.set (mu); mv }

    def pf (z: VectorD): Double = NaN

    def gen: VectorD = sgen.toDouble

    def igen: VectorI = sgen.toInt

    def sgen: VectorS =
    {
        val y   = new VectorS (dim)
        var str = ""
        for (i <- 0 until dim) {
            do str = rng.sgen while (unique && i > 0 && (y.slice (0, i) contains str))
            y(i) = str
        } // for
        y
    } // sgen

} // RandomVecS class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Multinomial` class generates random variate vectors following the 
 *  multinomial distribution.  This discrete RV models the multinomial trials,
 *  which generalize Bernoulli trials ({0, 1} to the case where the outcome is
 *  in {0, 1, ..., k-1}.
 *  @see http://www.math.uah.edu/stat/bernoulli/Multinomial.html
 *  @param p       array of cumulative probabilities as CDF values
 *  @param n       the number of independent trials
 *  @param stream  the random number stream
 */
case class Multinomial (p: Array [Double] = Array (.4, .7, 1.0), n: Int = 5, stream: Int = 0)
     extends VariateVec (stream)
{
    for (pi <- p if pi < 0 || pi > 1) flaw ("constructor", "parameter pi must be in [0, 1]*")
    if (n <= 0) flaw ("constructor", "parameter n must be positive")
    _discrete = true

    private val dice = Dice (p, stream)

    val mean: VectorD = { val mn = new VectorD (p.length); mn.set (dice.mean); mn }

    def pf (z: VectorD): Double =
    {
        val x = z.toInt
        var prob = choose (n, x(0), x(1)).toDouble
        for (j <- 2 until p.length) prob /= fac (x(j))
        prob
    } // pf

    def gen: VectorD = igen.toDouble

    def igen: VectorI = VectorI (for (i <- p.indices) yield dice.igen)

} // Multinomial class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VariateVecTest` object is used to test the Random Variate Vector (RVV)
 *  generators from the classes derived from `VariateVec`.
 *  > run-main scalation.random.VariateVecTest
 */
object VariateVecTest extends App
{
     var rvv: VariateVec = null                               // variate vector

     println ("Test: ProbabilityVec random vector generation ----------------")
     rvv = ProbabilityVec (10)
     println ("mean = " + rvv.mean)             // probability vector generator
     for (k <- 0 until 30) println (rvv.gen)

     println ("Test: NormalVec random vector generation ---------------------")
     val mu  = VectorD (5.0, 5.0)
     val cov = new MatrixD ((2, 2), 2.0, 1.0,
                                    1.0, 2.0)
     rvv = NormalVec (mu, cov)                 // multivariate normal generator
     println ("mean = " + rvv.mean)
     for (k <- 0 until 30) println (rvv.gen)

     println ("Test: PermutedVecD random vector generation ------------------")
     val x = VectorD (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
     rvv = PermutedVecD (x)                     // random permutation generator
     println ("mean = " + rvv.mean)
     for (k <- 0 until 30) println (rvv.gen)

     println ("Test: PermutedVecI random vector generation ------------------")
     val y = VectorI (1, 2, 3, 4, 5, 6, 7, 8, 9)
     rvv = PermutedVecI (y)                     // random permutation generator
     println ("mean = " + rvv.mean)
     for (k <- 0 until 30) println (rvv.igen)

     println ("Test: RandomVecD random vector generation --------------------")
     rvv = RandomVecD ()                             // random vector generator
     println ("mean = " + rvv.mean)
     for (k <- 0 until 30) println (rvv.gen)

     println ("Test: RandomVecI random vector generation --------------------")
     rvv = RandomVecI ()                             // random vector generator
     println ("mean = " + rvv.mean)
     for (k <- 0 until 30) println (rvv.igen)

     println ("Test: RandomVecS random vector generation --------------------")
     rvv = RandomVecS ()                             // random vector generator
     println ("mean = " + rvv.mean)
     for (k <- 0 until 30) println (rvv.igen)

     println ("Test: Multinomial random vector generation --------------------")
     rvv = Multinomial ()                             // random vector generator
     println ("mean = " + rvv.mean)
     for (k <- 0 until 30) println (rvv.igen)

} // VariateVecTest object

