
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Sep 30 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import math.{abs, floor, Pi, sqrt}

import scalation.math.DoubleWithExp._
import scalation.math.IntWithExp._
import scalation.util.Error
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This trait provides several common combinatorics functions, such as factorial
 *  permutations, combinations, gamma and beta functions.
 */
object Combinatorics extends Error
{
    /** Square root of Pi
     */
    val SQRT_PI = sqrt (Pi)

    /** Tolerance for real number comparisons
     */
    val EPSILON = 1E-9

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
    /** Return true if x == y approximately.
     *  @param x  the first value to compare
     *  @param y  the second value to compare
     */
    def approx (x: Double, y: Double): Boolean =  abs (x - y) < EPSILON

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute k! (k factorial).
     *  @param k  the argument to the factorial function
     */
    def fac (k: Int): Long =
    {
        var prod = 1l
        for (i <- 2 to k) prod *= i
        prod
    } // fac

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute permutations of k items selected from n total items.
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
    /** Compute n choose k (combinations of n things, k at a time).
     *  A more efficient implementation is given below.
     *  @param n  the total number of items
     *  @param k  the of items to choose (requires k <= n)
     */
    def chose (n: Int, k: Int): Long = perm (n, k) / fac (k)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute binomial coefficients:  n choose k, combinations of n things,
     *  k at a time, using Pascal's Triangle.
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
    /** Compute trinomial coefficients:  n choose (k, l), combinations of n things,
     *  (k, l) at a time, using Pascal's Tetrahedron.  Ex: Given n balls, counts
     *  ways in which k are chosen for group 1 and l are chosen for group 2.
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
    /** Compute the gamma function gamma(a) for the following two cases:
     *  http://mathworld.wolfram.com/GammaFunction.html
     *  (1) when a is an integer and (2) when a is an integer + 1/2.
     *  @param a  the parameter, a real number satisfying (1) or (2)
     */
    def gammaF (a: Double): Double =
    {
        if (a <= 0) flaw ("gammaF", "only handle positive cases")
        var prod = 1.
        val ia   = floor (a).toInt
        val frac = a - floor (a)
        if (frac < EPSILON) {
            prod = fac (ia - 1)
        } else if (approx (frac, .5)) {
            for (i <- 2 to ia) prod *= 2. * i - 1.
            prod *= SQRT_PI / 2~^ia
        } else {
            flaw ("gammaF", "only handle positive integer and halves cases")
        } // if
        prod
    } // gammaF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the kth degree rising factorial of x.  When x = 1, this is the
     *  regular factorial function k!.
     *  @param k  the number of factors in the product
     *  @param x  the base number to start the product
     */
    def rfac (k: Int, x: Double = 1.): Double =
    {
        gammaF (x + k) / gammaF (x)
    } // rfac

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Gauss's Hypergeometric function 2F1(z; a, b, c) via an approximation.
     *  @see 
     *  @param z  the variable, a real/complex number s.t. |z| < 1
     *  @param a  the first paramater, a real/complex number
     *  @param b  the second parameter, a real/complex number
     *  @param c  the third parameter, a real/complex number, may not be a negative integer
     */
    def hyp2f1 (z: Double, a: Double, b: Double, c: Double): Double =
    {
        if (b == c) return (1.-z)~^(-a)    // special cases

	val MAX_ITER = 15
        var sum      = 0.
        for (k <- 0 until MAX_ITER) {
            sum += ((rfac (k, a) * rfac (k, b)) / rfac (k, c)) * (z~^k / rfac (k))
        } // for
        sum
    } // hyp2f1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the beta function B(a, b) for the following two cases:
     *  (1) when a or b are integers and (2) when a or b are integers + 1/2.
     *  @see http://mathworld.wolfram.com/BetaFunction.html
     *  @param a  the first parameter, a real number satisfying (1) or (2)
     *  @param b  the second parameter, a real number satisfying (1) or (2)
     */
    def betaF (a: Double, b: Double): Double =
    {
        gammaF (a) * gammaF (b) / gammaF (a + b)
    } // betaF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the incomplete beta function B(z; a, b), a generalization of
     *  the beta function (z = 1).
     *  @see http://mathworld.wolfram.com/IncompleteBetaFunction.html
     *  @param z  the variable, a real/complex number s.t. 0 <= |z| <= 1
     *  @param a  the first parameter, a real/complex number > 0
     *  @param b  the second parameter, a real/complex number > 0
     */
    def iBetaF (z: Double, a: Double, b: Double): Double =
    {
        (z~^a / a) * hyp2f1 (z, a, 1.-b, a+1.)
    } // iBetaF
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the regularized (incomplete) beta function I(z; a, b).
     *  @see http://mathworld.wolfram.com/RegularizedBetaFunction.html
     *  @param z  the variable, a real/complex number s.t. 0 <= |z| <= 1
     *  @param a  the first parameter, a real/complex number > 0
     *  @param b  the second parameter, a real/complex number > 0
     */
    def rBetaF (z: Double, a: Double, b: Double): Double = 
    {
        iBetaF (z, a, b) / betaF (a, b)
    } // rBetaF

} // Combinatorics object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object test the methods in the Combinatorics object.
 */
object CombinatoricsTest extends App
{
    import Combinatorics._

    println ("\nTest Combinatorics functions")
    println ("approx (5, 5)    = " + approx (5, 5))
    println ("approx (5, 5.1)  = " + approx (5, 5.1))
    println ("fac (5)          = " + fac (5))
    println ("fac (10)         = " + fac (10))
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
        var sum = 0.
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

