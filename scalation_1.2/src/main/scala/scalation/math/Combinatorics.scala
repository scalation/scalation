
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.2
 *  @date    Sat Apr 12 13:45:50 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import scala.math.{abs, asin, E, exp, floor, log, Pi, sin, sqrt}

import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Combinatorics` object provides several common combinatorics functions,
 *  such as factorial permutations, combinations, gamma and beta functions.
 */
object Combinatorics extends Error
{
    private val _1_3      = 1.0 / 3.0               // one third
    private val _1_6      = 1.0 / 6.0               // one sixth
    private val _1_30     = 1.0 / 30.0              // one thirtieth
    private val _5_90     = 5.0 / 90.0              // five over ninety
    private val _2PI      = 2.0 * Pi                // two Pi
    private val SQRT_PI   = sqrt (Pi)               // square root of Pi
    private val SQRT_2PI  = sqrt (_2PI)             // square root of two Pi
    private val LOG_2PI   = log (_2PI)              // natural log of two Pi
    private val LOG_R_PI  = log (SQRT_PI)           // natural log of square root of Pi

    /** Table of all factorial numbers that can be represented as a long (64-bit) integer
     */
    val lfac = Array (1l, 1l, 2l, 6l, 24l, 120l, 720l, 5040l, 40320l, 362880l, 3628800l, 39916800l, 479001600l,
                      6227020800l, 87178291200l, 1307674368000l, 20922789888000l, 355687428096000l,
                      6402373705728000l, 121645100408832000l, 2432902008176640000l)

    /** Initial part of Pascal's Triangle, precomputed to speed calculations
     *  (Binomial Coefficients)
     */
    val pascalTri = Array (         Array (1),
                                  Array (1,  1),
                                Array (1,  2,  1),
                              Array (1,  3,  3,  1),
                            Array (1,  4,  6,  4,  1),
                          Array (1,  5, 10, 10,  5,  1),
                        Array (1,  6, 15, 20, 15,  6,  1),
                      Array (1,  7, 21, 35, 35, 21,  7,  1),
                    Array (1,  8, 28, 56, 70, 56, 28,  8,  1),
                  Array (1,  9, 36, 84,126,126, 84, 36,  9,  1),
                Array (1, 10, 45,120,210,252,210,120, 45, 10,  1),
              Array (1, 11, 55,165,330,462,462,330,165, 55, 11,  1),
            Array (1, 12, 66,220,495,792,924,792,495,220, 66, 12,  1),
          Array (1, 13,78,286,715,1287,1716,1716,1287,715,286,78,13, 1),
        Array (1,14,91,364,1001,2002,3003,3432,3003,2002,1001,364,91,14,1),
     Array (1,15,105,455,1365,3003,5005,6435,6435,5005,3003,1365,455,105,15,1))

    /** Initial part of Pascal's Tetrahedron, precomputed to speed calculations
     *  (Trinomial Coefficients)
     *  @see https://sites.google.com/site/pascalloids/pascal-s-pyramid-3-var
     */
    val pascalTet = Array (         Array (Array (1)),          // layer 0

                                    Array (Array (1),           // layer 1
                                         Array (1,  1)),
                              
                                    Array (Array (1),           // layer 2
                                         Array (2,  2),
                                       Array (1,  2,  1)),

                                    Array (Array (1),           // layer 3
                                         Array (3,  3),
                                       Array (3,  6,  3),
                                     Array (1,  3,  3,  1)),

                                    Array (Array (1),           // layer 4
                                         Array (4,  4),
                                       Array (6, 12,  6),
                                     Array (4, 12, 12,  4),
                                   Array (1,  4,  6,  4,  1)),

                                    Array (Array (1),           // layer 5
                                         Array (5,  5),
                                       Array (10, 20, 10),
                                     Array (10, 30, 30, 10),
                                   Array (5, 20,  30, 20,  5),
                                 Array (1,  5, 10,  10,  5,  1)),

                                    Array (Array (1),           // layer 6
                                         Array (6,  6),
                                       Array (15, 30, 15),
                                     Array (20, 60, 60, 20),
                                   Array (15, 60, 90, 60, 15),
                                 Array (6, 30, 60, 60,  30,  6),
                               Array (1,  6, 15, 20, 15,  6,  1)),

                                    Array (Array (1),           // layer 7
                                         Array (7,  7),
                                       Array (21, 42, 21),
                                     Array (35,105,105, 35),
                                   Array (35,140,210,140, 35),
                                 Array (21,105,210,210,105, 21),
                               Array (7, 42,105,140,105, 42,  7),
                             Array (1,  7, 21, 35, 35, 21,  7,  1)),

                                    Array (Array (1),           // layer 8
                                         Array (8,  8),
                                       Array (28, 56, 28),
                                     Array (56,168,168, 56),
                                   Array (70,280,420,280, 70),
                                 Array (56,280,560,560,280, 56),
                               Array (28,168,420,560,420,168, 28),
                             Array (8, 56,168,280,280,168, 56,  8),
                           Array (1,  8, 28, 56, 70, 56, 28,  8,  1)),

                                    Array (Array(1),            // layer 9
                                         Array(9,  9),
                                       Array(36, 72, 36),
                                     Array(84,252,252, 84),
                                   Array(126,504,756,504,126),
                                 Array(126,630,1260,1260,630,126),
                               Array(84,504,1260,1680,1260,504,84),
                             Array(36,252,756,1260,1260,756,252, 36),
                           Array(9, 72, 252, 504,630,504, 252, 72, 9),
                         Array(1,  9, 36, 84, 126, 126, 84, 36,  9,  1)))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For small 'k', compute 'k' factorial by iterative multiplication.
     *  <br>
     *      k! = k * (k-1) * ... * 2 * 1
     *  <br>
     *  @param k  the nonnegative integer-valued argument to the factorial function
     */
    def mfac (k: Int): Long =
    {
        var prod = 1l
        for (i <- 2 to k) prod *= i
        prod
    } // mfac

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute 'k' factorial 'k!' using three techniques (requires 'k <= 170').
     *  @param k  the nonnegative integer-valued argument to the factorial function
     */
    def fac (k: Int): Double =
    {
        if (k < lfac.length) lfac(k).toDouble       // exact value from table lookup
        else if (k < 142)    gammaF (k + 1)         // Gamma Function (very close)
        else ramanujan (k)                          // Ramanujan's Factorial Approximation
    } // fac

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute 'k!' using Stirling's 2-nd Order Factorial Approximation.
     *  @see http://en.wikipedia.org/wiki/Stirling%27s_approximation     
     *  @param k  the nonnegative integer-valued argument to the factorial function
     */
    def stirling (k: Int): Double =
    {
        if (k < 2) return 1.0
        val n = k.toDouble
        sqrt (_2PI * n) * (n/E)~^n * (1.0 + 1.0/(12.0*n))
    } // stirling

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute 'k!' using Mortici's Factorial Approximation (more accurate than
     *  Stirling's 2nd Order Factorial Approximation).
     *  @see http://but.unitbv.ro/BU2010/Series%20III/BULETIN%20III%20PDF/Mathematics/Mortici.pdf
     *  @param k  the nonnegative integer-valued argument to the factorial function
     */
    def mortici (k: Int): Double =
    {
        if (k < 2) return 1.0
        val n = k.toDouble
        SQRT_2PI * (n/E)~^n * n / ((n - _1_3) * n + _5_90)~^0.25
    } // mortici

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute 'k!' using Ramanujan's Factorial Approximation (more accurate than
     *  Mortici's Factorial Approximation).
     *  @see http://files.ele-math.com/articles/jmi-05-53.pdf
     *  @param k  the nonnegative integer-valued argument to the factorial function
     */
    def ramanujan (k: Int): Double =
    {
        if (k < 2) return 1.0
        val n = k.toDouble
        SQRT_PI * (n/E)~^n * (((8.0 * n + 4.0) * n + 1.0) * n + _1_30)~^_1_6
    } // ramanujan

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the natural log factorial 'ln (k!)' so 'k! = exp (logfac (k))'.
     *  The formula is a log transformation of Ramanujan's Factorial Approximation.
     *  @param k  the value to take the log factorial of
     */
    def logfac (k: Int): Double = 
    {
        if (k < 2) return 0.0
        val n = k.toDouble
        LOG_R_PI + n * (log (n) - 1.0) + _1_6 * log (((8.0 * n + 4.0) * n + 1.0) * n + _1_30)
    } // logfac
 
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute permutations of 'k' items selected from 'n' total items.
     *  @param n  the total number of items
     *  @param k  the of items selected
     */
    def perm (n: Int, k: Int): Long =
    {
        var prod = 1l
        for (i <- n until n-k by -1) prod *= i
        prod
    } // perm

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute 'n' choose 'k' (combinations of 'n' things, 'k' at a time).
     *  A more efficient implementation is given below.
     *  @param n  the total number of items
     *  @param k  the of items to choose (requires k <= n)
     */
    def chose (n: Int, k: Int): Long = perm (n, k) / lfac(k)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute binomial coefficients:  'n' choose 'k', combinations of 'n' things,
     *  'k' at a time, using Pascal's Triangle.
     *  @see http://www.mathsisfun.com/pascals-triangle.html
     *  @param n  the total number of items
     *  @param k  the of items to choose (requires k <= n)
     */
    def choose (n: Int, k: Int): Long = 
    {
        if (k == 0 || k == n) 1l
        else if (n < pascalTri.length) pascalTri(n)(k)
        else choose (n-1, k-1) + choose (n-1, k)
    } // choose

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute trinomial coefficients:  'n' choose '(k, l'), combinations of
     *  'n' things, '(k, l)' at a time, using Pascal's Tetrahedron.
     *  Ex: Given 'n' balls, counts ways in which 'k' are chosen for group 1
     *  and 'l' are chosen for group 2.
     *  @see http://people.sju.edu/~pklingsb/bintrin.pdf
     *  @param n  the total number of items
     *  @param k  the of items to choose
     *  @param l  the of items to choose (requires 0 <= k + l <= n)
     */
    def choose (n: Int, k: Int, l: Int): Long = 
    {
        println ("(n, k, l) = (" + n + ", " + k + ", " + l + ")")
        if ((k == 0 && l == 0) || k == n || l == n) 1l
        else if (n < pascalTet.length) if (k >= l) pascalTet(n)(k)(l) else pascalTet(n)(l)(k)
        else choose (n-1, k-1, l) + choose (n-1, k, l-1) + choose (n-1, k, l)
    } // choose

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the gamma function 'gamma (a)' using the Lanczos Approximation.
     *  @see http://en.wikipedia.org/wiki/Lanczos_approximation
     *  @param a  the parameter, a real number
     */
    def gammaF (a: Double): Double =
    {
        val g = 7
        val p = Array (0.99999999999980993, 676.5203681218851, -1259.1392167224028,
                       771.32342877765313, -176.61502916214059, 12.507343278686905,
                      -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7)

        def gamma (zz: Double): Double = 
        {
            var z = zz
            if (z < 0.5) {
                Pi / (sin (Pi * z) * gamma (1 - z))
            } else {
                z -= 1.0
                var x = p(0)
                for (i <- 1 until g + 2) x += p(i) / (z + i)
                val t = z + g + 0.5
                SQRT_2PI * t ~^ (z + 0.5) * exp (-t) * x
            } // if
        } // gamma
        gamma (a)

    } // gammaF

    // @see http://mathworld.wolfram.com/GammaFunction.html
//  def gammaF (a: Double): Double =
//  {
//      if (a <= 0) flaw ("gammaF", "only handle positive cases")
//      var prod = 1.0
//      val ia   = floor (a).toInt
//      val frac = a - floor (a)
//      if (frac < TOL) {
//          prod = fac (ia - 1)
//      } else if (approx (frac, .5)) {
//          for (i <- 2 to ia) prod *= 2.0 * i - 1.0
//          prod *= SQRT_PI / 2~^ia
//      } else {
//          flaw ("gammaF", "only handle positive integer and halves cases")
//      } // if
//      prod
//  } // gammaF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'th degree rising factorial of 'x'.  When 'x = 1', this is
     *  the regular factorial function 'k!'.  Also known as Pochhammer's symbol.
     *  Caveat: only works when 'k' is a nonnegative integer
     *  @param k  the number of factors in the product
     *  @param x  the base number to start the product
     */
    def rfac (k: Int, x: Double = 1.0): Double =
    {
        var prod = x
        for (i <- 1 until k) prod *= x + i
        prod
    } // rfac

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'th degree rising factorial of 'x'.  When 'x = 1', this is
     *  the regular factorial function 'k!'.  Also known as Pochhammer's symbol.
     *  @param k  the number of factors in the product (generalized to a real)
     *  @param x  the base number to start the product
     */
//  def rfac (k: Double, x: Double = 1.0): Double = gammaF (x + k) / gammaF (x)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Gauss Hypergeometric function '2F1(a, b, c; z)' using a
     *  power series expansion.
     *  @see http://dx.doi.org/10.1016/j.cpc.2007.11.007
     *  @see en.wikipedia.org/wiki/Hypergeometric_function
     *  For faster or more robust algorithms,
     *  @see people.maths.ox.ac.uk/porterm/research/pearson_final.pdf
     *  @param a  the first parameter, a real/complex number
     *  @param b  the second parameter, a real/complex number
     *  @param c  the third parameter, a real/complex number, may not be a negative integer
     *  @param z  the variable, a real/complex number s.t. |z| < 1
     */
    def hyp2f1 (a: Double, b: Double, c: Double, z: Double): Double =
    {
        val MAX_ITER = 35           // for 9 sig-digits in t-dist with 10 dof

        (a, b, c) match {
        case (  _, 1.0, 1.0) => (1.0 / (1.0 - z))~^(-a)
        case (0.5, 0.5, 1.5) => asin (z) / z
        case (1.0, 1.0, 2.0) => log (1.0 - z) / -z
        case (1.0, 2.0, 1.0) => 1.0 / ((1.0 - z) * (1.0 - z))
        case (1.0, 2.0, 2.0) => 1.0 / (1.0 - z)
        case _               => var sum  = 0.0
                                var prod = 1.0
                                for (k <- 0 until MAX_ITER) {
                                    sum  += prod
                                    prod *= z * ((a + k) * (b + k)) / ((c + k) * (k + 1.0))
                                } // for
                                sum
        } // match
    } // hyp2f1

//  def hyp2f1 (a: Double, b: Double, c: Double, z: Double): Double =
//  {
//      if (b == c) return (1.0-z)~^(-a)    // special cases
//
//      val MAX_ITER = 35
//      var sum      = 0.0
//      for (k <- 0 until MAX_ITER) {
//          sum += ((rfac (k, a) * rfac (k, b)) / rfac (k, c)) * (z~^k / fac (k))
//      } // for
//      sum
//  } // hyp2f1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the beta function 'B(a, b)' for the following two cases:
     *  (1) when 'a' or 'b' are integers and (2) when 'a' or 'b' are integers + 1/2.
     *  @see http://mathworld.wolfram.com/BetaFunction.html
     *  @param a  the first parameter, a real number satisfying (1) or (2)
     *  @param b  the second parameter, a real number satisfying (1) or (2)
     */
    def betaF (a: Double, b: Double): Double =
    {
        gammaF (a) * gammaF (b) / gammaF (a + b)
    } // betaF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the incomplete beta function 'B(z; a, b)', a generalization of
     *  the beta function 'z = 1'.
     *  @see http://mathworld.wolfram.com/IncompleteBetaFunction.html
     *  @param z  the variable, a real/complex number s.t. 0 <= |z| <= 1
     *  @param a  the first parameter, a real/complex number > 0
     *  @param b  the second parameter, a real/complex number > 0
     */
    def iBetaF (z: Double, a: Double, b: Double): Double = 
    {
        (z~^a / a) * hyp2f1 (a, 1.0-b, a+1.0, z)
    } // iBetaF
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the regularized (incomplete) beta function 'I(z; a, b)'.
     *  @see http://mathworld.wolfram.com/RegularizedBetaFunction.html
     *  @param z  the variable, a real/complex number s.t. 0 <= |z| <= 1
     *  @param a  the first parameter, a real/complex number > 0
     *  @param b  the second parameter, a real/complex number > 0
     */
    def rBetaF (z: Double, a: Double, b: Double): Double = iBetaF (z, a, b) / betaF (a, b)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the complement of the regularized (incomplete) beta function
     *  '1.0 - I(z; a, b) = I(1.0 - z; b, a)'.
     *  @author Michael Cotterell
     *  @param z  the variable, a real/complex number s.t. 0 <= |z| <= 1
     *  @param a  the first parameter, a real/complex number > 0
     *  @param b  the second parameter, a real/complex number > 0
     */
    def rBetaC (z: Double, a: Double, b: Double): Double = 1.0 - rBetaF (z, a, b)

} // Combinatorics object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CombinatoricsTest` object tests the methods in the `Combinatorics` object.
 */
object CombinatoricsTest extends App
{
    import Combinatorics._

    println ("\nTest Combinatorics functions")
    println ("perm (5, 2)      = " + perm (5, 2))
    println ("perm (10, 3)     = " + perm (10, 3))
    println ("gammaF (5)       = " + gammaF (5))
    println ("gammaF (5.5)     = " + gammaF (5.5))
    println ("betaF (5, 6)     = " + betaF (5, 6))
    println ("betaF (5.5, 6)   = " + betaF (5.5, 6))
    println ("perm (22, 10)    = " + perm (22, 10))
    println ("choose (22, 10 ) = " + choose (22, 10))
    println ("choose (9, 3, 4) = " + choose (9, 3, 4))
    println ("chose (22, 10)   = " + chose (22, 10))
    
    println ("\nCheck Pascal's Tetrahedron")
    for (n <- 0 until pascalTet.length) {
        var sum = 0.0
        for (k <- 0 to n; l <- 0 to k) sum += pascalTet(n)(k)(l)
        println ("sum for layer " + n + " = " + sum + " =? " + 3~^n)
    } // for

    println ("\nBuild Pascal's Triangle using choose (n, k)")
    val max = 16
    for (n <- 0 to max) {
        for (i <- 1 to (max - n) / 2) print ("\t")
        for (k <- 0 to n) {
            val c = choose (n, k)
            if (n % 2 == 1) if (c < 1000) print ("    ") else print ("   ")
            print (c + "\t")
        } // for
        println ()
    } // for

} // CombinatoricsTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CombinatoricsTest2` object tests the Gamma and factorial methods in the
 *  `Combinatorics` object.
 */
object CombinatoricsTest2 extends App
{
    import Combinatorics._   

    println ("\nTest Gamma and Factorial functions")
    for (i <- 0 to 171) {
        val f0 = fac (i)
        val f1 = gammaF (i+1)
        val f2 = ramanujan (i)
        val f3 = exp (logfac (i))
        val f4 = mortici (i)
        val f5 = stirling (i)
        println (i + ":\t" + f0 + "\t" + f1 + "\t" + f2 + "\t[ " + abs (f2 - f1) + " ]" +
                                              "\t" + f3 + "\t[ " + abs (f3 - f1) + " ]" +
                                              "\t" + f4 + "\t[ " + abs (f4 - f1) + " ]" +
                                              "\t" + f5 + "\t[ " + abs (f5 - f1) + " ]")
    } // for

} // CombinatoricsTest2 object

