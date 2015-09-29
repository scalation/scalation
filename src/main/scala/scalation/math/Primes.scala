
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.2
 *  @date    Wed Nov  2 22:32:00 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import collection.mutable.{IndexedSeq, ArrayBuffer}
import math.{floor, sqrt}
import util.control.Breaks.{breakable, break}
import util.Random

import scalation.util.Swap.swap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Primes` object provides an array of 1000 prime numbers as well as methods
 *  to generate prime numbers within a given range.
 */
object Primes
{
    /** Scala's random number generator
     */
    private val rn = new Random ()

    /** Precomputed and randomized array of 1000 4-digit prime numbers
     */
    val prime = Array [Int] (
        4919,   8689,   3089,   1447,   2371,   9923,   1811,   2671,   9433,   6581, 
        4339,   4397,   7879,   4549,   9907,   2609,   4751,   8731,   2381,   4567, 
        5573,   3137,   9851,   4957,   2713,   2633,   8693,   1153,   3923,   2857, 
        9619,   5557,   6229,   3217,   6917,   2447,   7417,   1831,   5119,   1429, 
        1409,   6703,   1171,   1663,   1399,   2731,   8237,   9859,   5519,   5099, 
        3433,   8941,   6211,   8243,   3343,   9371,   8443,   4937,   1217,   1423, 
        1301,   9011,   3221,   9461,   5881,   3163,   8219,   2011,   6469,   7823, 
        8311,   5521,   6163,   7027,   3701,   6277,   1721,   3889,   6967,   3167, 
        9013,   4481,   7001,   4159,   9929,   9467,   3463,   3911,   7907,   2879, 
        6547,   2621,   3041,   1901,   5653,   7673,   5813,   4987,   1297,   3797, 
        5021,   5839,   9091,   9677,   4801,   6203,   5393,   6067,   3853,   4253, 
        5023,   5531,   9377,   1109,   3779,   9281,   3761,   7919,   9067,   2971, 
        8741,   2143,   9967,   5087,   5417,   8999,   6337,   3467,   4523,   6563, 
        5147,   7577,   7039,   4289,   6449,   6991,   2473,   2591,   8191,   8581, 
        5441,   1709,   2549,   1039,   8573,   5591,   3559,   3739,   3557,   6653, 
        4073,   4229,   9257,   7283,   8963,   2551,   9109,   6073,   2243,   2141, 
        3299,   4363,   3257,   2851,   8269,   5051,   1543,   1289,   5669,   8293, 
        6779,   1777,   1699,   7369,   7297,   8893,   7621,   3581,   6263,   6737, 
        6857,   9161,   3947,   7213,   8761,   5449,   1307,   1279,   4517,   6257, 
        9473,   5077,   4871,   8861,   4463,   7949,   7841,   2843,   3727,   4679, 
        4111,   4561,   5407,   4493,   4597,   9719,   4451,   5779,   2719,   3517, 
        2399,   4273,   5039,   2423,   7643,   6553,   5903,   8887,   6679,   7459, 
        1327,   7013,   6247,   2777,   3659,   2903,   7489,   3203,   4211,   8011, 
        9643,   1483,   4391,   4591,   1453,   1907,   6481,   8263,   2647,   9769, 
        4051,   2467,   9887,   6823,   7757,   4787,   7817,   1879,   3307,   5939, 
        8629,   8599,   8863,   8111,   3119,   3907,   1609,   4057,   4373,   3677, 
        5851,   9431,   3733,   1783,   1061,   3457,   2281,   9437,   2087,   3209, 
        3643,   6101,   4703,   6689,   1621,   5507,   9697,   9511,   3331,   2749, 
        6793,   9029,   7589,   2309,   3881,   9181,   1553,   9901,   6151,   5737, 
        7703,   1607,   6869,   2383,   4639,   5153,   5443,   7561,   4657,   8681, 
        2663,   7687,   1187,   2837,   5623,   6571,   8467,   9323,   2029,   1193, 
        3769,   3637,   4093,   4139,   3001,   7331,   7937,   3989,   4951,   7681, 
        3967,   2251,   9791,   5827,   1471,   6397,   5701,   2293,   2269,   5711, 
        2999,   3229,   6883,   6719,   2347,   8539,   3823,   1033,   7669,   6701, 
        1319,   1877,   1213,   5309,   8737,   5323,   4621,   5059,   4877,   6983, 
        8377,   9811,   6661,   7433,   5261,   4019,   4519,   3461,   1511,   3613, 
        1367,   9941,   7541,   6451,   7517,   6217,   9137,   8291,   3083,   6949, 
        2003,   1049,   2707,   2819,   2791,   9157,   8923,   1693,   1861,   7901, 
        3079,   6709,   1093,   8513,   2357,   4421,   5927,   4243,   4261,   9049, 
        9857,   6871,   6007,   7109,   8719,   2617,   8009,   6673,   7219,   6047, 
        5011,   4099,   2221,   4231,   2953,   7607,   3109,   7309,   2437,   5651,
        1579,   7753,   6911,   8053,   4817,   6599,   5501,   4507,   1451,   3851, 
        9679,   3469,   2521,   6299,   6343,   7789,   4007,   3347,   9767,   4447, 
        4973,   2711,   6803,   7573,   9103,   9403,   6607,   7193,   2179,   7927, 
        9241,   6173,   7537,   1151,   1847,   6761,   7559,   7951,   2833,   9293, 
        8419,   1667,   2083,   2213,   7321,   1973,   4637,   8783,   7549,   5843, 
        7393,   2339,   6133,   2311,   2417,   2153,   6691,   6367,   4933,   6863, 
        7741,   6113,   6763,   9391,   3391,   8363,   1873,   9277,   1433,   7043, 
        3533,   2377,   9203,   2203,   1759,   2741,   7411,   1361,   5563,   3583, 
        7759,   3709,   3491,   8837,   8369,   3539,   4013,   6271,   8803,   9781, 
        2111,   2687,   7547,   5857,   7867,   8521,   4663,   2017,   8663,   3389, 
        1627,   5743,   2129,   8929,   6373,   6961,   3931,   5527,   2579,   6907, 
        6833,   3863,   1087,   6011,   4177,   5233,   5167,   8171,   7933,   9421, 
        5749,   3019,   7307,   8087,   5741,   2963,   7717,   2341,   4603,   4337, 
        6121,   4003,   3011,   5801,   1013,   8849,   4831,   8447,   4441,   1933, 
        2411,   5683,   1949,   1459,   1069,   5003,   4969,   2069,   9533,   3673, 
        7079,   1303,   6959,   6079,   8753,   7333,   5861,   2677,   4157,   5479, 
        8329,   3571,   5791,   9479,   8429,   5641,   1657,   2099,   9661,   6427, 
        1381,   4931,   6269,   2753,   6971,   9739,   7829,   6131,   5101,   4129, 
        1129,   5783,   6529,   7247,   7019,   4513,   7351,   7207,   5399,   6037, 
        9133,   3617,   1091,   2237,   1753,   9787,   8209,   3691,   7103,   5647, 
        3319,   4783,   2861,   7477,   7793,   3943,   1697,   7853,   8831,   1871, 
        7057,   6221,   6781,   1163,   3449,   2027,   9341,   1487,   5231,   3301, 
        4733,   7649,   5279,   6029,   9833,   3511,   1117,   4813,   2543,   9629, 
        5387,   5381,   1993,   8623,   4861,   2729,   6091,   4651,   1229,   4457, 
        4219,   7603,   2267,   7451,   2089,   9227,   5303,   3329,   9871,   4217, 
        4723,   7129,   6491,   5693,   5657,   8273,   1571,   5981,   1259,   6827, 
        3833,   7069,   6791,   5471,   8221,   6421,   3547,   2797,   3271,   1427, 
        7523,   6899,   8069,   3803,   8161,   5179,   1733,   6043,   9721,   8089, 
        3929,   9187,   9343,   8101,   2503,   9613,   1439,   8147,   8081,   6089, 
        1741,   3631,   8543,   1867,   1231,   9127,   9283,   9319,   3767,   3037, 
        7349,   4283,   1669,   9649,   1481,   2039,   7127,   3371,   3407,   4789, 
        1223,   8933,   1583,   4271,   1999,   8167,   9817,   9059,   4357,   5639, 
        4889,   9001,   4079,   5281,   3719,   7963,   4799,   2239,   8287,   9397, 
        1249,   1889,   6317,   3541,   6947,   1549,   8093,   7487,   7727,   8627, 
        5351,   8527,   4583,   7457,   9311,   7151,   4297,   5879,   8839,   6569, 
        7723,   9601,   3023,   7237,   8389,   6521,   3529,   9973,   1321,   6287, 
        7159,   8819,   1201,   9337,   6619,   5923,   9151,   1747,   4943,   2909, 
        3361,   8669,   9587,   9419,   7877,   1123,   5197,   1277,   6197,   1637, 
        8233,   4909,   4649,   2113,   2459,   5297,   5431,   6659,   9043,   9539, 
        2137,   4027,   4127,   6551,   1987,   7243,   2801,   6301,   3623,   1567, 
        2389,   1597,   7529,   5419,   2081,   7591,   8353,   2789,   4409,   3697, 
        1373,   3191,   5009,   9497,   5483,   8563,   6733,   7177,   2273,   4153, 
        1531,   2287,   9803,   7499,   2887,   2393,   1913,   8677,   1051,   5689, 
        3187,   6977,   8867,   9413,   3917,   5477,   6829,   8059,   1031,   7993, 
        8609,   4729,   9743,   9439,   8821,   4349,   9491,   5867,   1723,   4241, 
        9883,   8461,   8297,   1619,   1291,   2161,   6199,   4483,   7211,   2803, 
        2767,   6637,   3169,   8431,   3821,   8807,   8597,   9521,   4903,   2957, 
        4673,   8317,   5273,   9551,   9221,   9007,   1523,   7507,   2683,   1237, 
        7699,   6353,   6389,   2131,   9949,   8387,   8641,   7253,   9463,   4759, 
        3049,   4021,   4133,   4423,   1613,   5081,   1097,   1789,   8117,   8647, 
        2693,   5717,   1063,   4793,   3413,   2557,   8779,   4643,   7187,   3067, 
        3607,   3181,   9829,   2351,   2539,   8423,   2531,   3253,   3313,   2897, 
        5807,   1103,   1493,   6379,   4091,   8501,   2333,   4259,   1559,   7873, 
        4001,   5107,   7229,   6323,   2441,   8969,   9749,   8039,   4999,   8971, 
        6361,   6577,   5953,   1997,   8951,   3593,   1019,   2659,   5437,   1601, 
        5171,   3061,   5659,   6359,   6053,   9209,   4721,   5113,   4049,   1283, 
        9199,   8707,   3259,   9349,   8231,   3877,   3671,   1181,   7121,   6473, 
        1931,   3323,   1489,   5581,   8537,   4993,   7691,   8747,   7481,   1499, 
        9547,   9689,   5869,   5897,   9931,   3527,   5333,   2477,   5821,   2939)
      
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make an array of prime numbers for storing within a program.
     */
    def makePrimeList
    {
        val prime = genPrimesSoA ()
        shuffle (prime)
        println ("makePrimeList: " + prime.size + " prime numbers")
        println ("    val prime = Array (")
        printAll (prime)
    } // makePrimeList

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shuffle the elements in the array buffer to randomize the prime numbers.
     */
    def shuffle (a: ArrayBuffer [Int])
    {
        for (i <- 0 until a.size) swap (a, i, rn.nextInt (a.size))
    } // shuffle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate, based on the Sieve of Atkin (SoA), prime numbers between
     *  integers lb and ub.
     *  @see http://en.wikipedia.org/wiki/Sieve_of_Atkin
     *  @param lb  the lower bound
     *  @param ub  the uppper bound 
     */
    def genPrimesSoA (lb: Int = 1000, ub: Int = 10000): ArrayBuffer [Int] =
    {
        val sieve  = IndexedSeq.fill [Boolean] (ub + 1)(false)   // the sieve
	val primes = ArrayBuffer [Int] ()                        // array of prime numbers
        val limit  = sqrt (ub).toInt                             // square root of upper bound
	var n      = 1                                           // candidate numbers
        var xx     = 0                                           // x^2
        var yy     = 0                                           // y^2
        var nn     = 0                                           // n^2
        var knn    = 0                                           // k * n^2

	// put in candidate primes: 
	// integers having odd number of representations by certain quadratic forms

        for (x <- 1 to limit; y <- 1 to limit) {
            xx = x * x; yy = y * y
	    n = 4*xx + yy
	    if (n <= ub && (n % 12 == 1 || n % 12 == 5)) sieve(n) = ! sieve(n)
	    n -= xx
	    if (n <= ub && n % 12 == 7) sieve(n) = ! sieve(n)
	    n = 3*xx - yy
	    if (x > y && n <= ub && n % 12 == 11) sieve(n) =  ! sieve(n) 
	} // for

        // eliminate composite numbers by sieving:
        // if n is prime, omit multiples of its square

	for (n <- 5 to limit if sieve(n)) {
            nn  = n * n
            knn = nn
            breakable { for (k <- 1 to ub) {
                if (knn > ub) break
                sieve(knn) = false
                knn += nn
            }} // for
	} // for

	for (n <- 2 to 3) sieve(n) = true             // initialize the frist two primes
	for (n <- lb to ub if sieve(n)) primes += n   // create the prime number array
	primes                                        // return the array of primes
    } // genPrimesSoA

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate, based on the Sieve of Eratosthenes (SoE), prime numbers between
     *  integers lb and ub.  This generator is simpler, but less efficient.
     *  @see http://en.wikipedia.org/wiki/Formula_for_primes
     *  @param lb  the lower bound
     *  @param ub  the upper bound
     */
    def genPrimesSoE (lb: Int = 1000, ub: Int = 10000): ArrayBuffer [Int] =
    {
        val primes = ArrayBuffer [Int] ()      // array to hold prime numbers
        var jroot  = 0.0                       // floor of square root of j
        var sum    = 0.0                       // sum of terms

        for (j <- lb to ub) {                  // if integer j is prime, add to array
            jroot = floor (sqrt (j))
            sum   = 0.0
            for (s <- 2.0 to jroot by 1.0) sum += floor (j / s) - floor ((j-1) / s)
            if ((floor (-1.0 * sum / j)).toInt == 0) primes += j
        } // for
        primes                                 // return the array of primes
    } // genPrimesSoE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print an array buffer of prime numbers.
     *  @param primes  the prime numbers to print
     */
    def printAll (primes: ArrayBuffer [Int])
    {
        for (i <- 0 until primes.size) {
            print ("%6d, ".format (primes(i)))
            if (i % 10 == 9) println ()
        } // for
        println ()
    } // printAll

} // Primes object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PrimesTest` object is use to perform timing test on the `Primes` object.
 */
object PrimesTest extends App
{
    import Primes._

    /** Timings for generating primes using SoA
     */
    val timeGenSoA = for (trial <- 1 to 10) yield time ("genPrimesSoA") { genPrimesSoA () }

    /** Timings for generating primes using SoE
     */
    val timeGenSoE = for (trial <- 1 to 10) yield time ("genPrimesSoE") { genPrimesSoE () }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Time the execution of method/function f.
     *  @param title  display indicator
     *  @param f      the method/function to time
     */
    def time (title: String) (f: => Unit): String =
    {
        val start = System.currentTimeMillis
	f
	val stop = System.currentTimeMillis
	"TIMING: %s = %dms".format (title, (stop - start))
    } // time
    
    for (s <- timeGenSoA) println (s)
    for (s <- timeGenSoE) println (s)
    makePrimeList

} // PrimesTest

