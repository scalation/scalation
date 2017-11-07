
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.4
 *  @date    Mon Mar 24 22:08:32 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  Many of the algorithms used are from software overseen by Pierre L'Ecuyer 
 */

package scalation.random

import java.util.Date

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StreamMaker` object computes seeds for `Random` and `Random2`, both
 *  of which implement the 'MRG31k3p' random number generator.  This generator
 *  has a period length around 2^185.  Each seed is a 6-dimensional vector of
 *  32-bit integers.
 *  @see  http://www.iro.umontreal.ca/~simardr/ssj/indexe.html
 */
object StreamMaker
{
    // moduli for computations
    private val M1 = 2147483647        // 2^31 - 1
    private val M2 = 2147462579        // 2^31 - 21069

    // stream variables
    private val stream = Array (12345, 12345, 12345,
                                12345, 12345, 12345)

    // streams constants
    private val a1p134 = Array (Array (1702500920, 1849582496, 1656874625),
                                Array (828554832, 1702500920, 1512419905),
                                Array (1143731069, 828554832, 102237247))

    private val a2p134 = Array (Array (796789021, 1464208080, 607337906), 
                                Array (1241679051, 1431130166, 1464208080),
                                Array (1401213391, 1178684362, 1431130166))
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the first 'k' seeds for the 'MRG31k3p' random number generator.
     *  @param k  the number of seeds to print (defaults to 1000)
     */
    def printSeeds (k: Int = 1000)
    {
        val INDENT = "                       "

        println ("\n" +
                 "//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n" +
                 "/** @author  John Miller, Casey Bowman (code generated)\n" +
                 " *  @version 1.4\n" +
                 " *  @date    " + new Date () + "\n" +
                 " *  @see     LICENSE (MIT style license file).\n" +
                 " */\n" +
                 "\n" +
                 "package scalation.random\n" +
                 "\n" +
                 "//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n" +
                 "/** The first " + k + " seeds for the 'MRG31k3p' random number generator.\n" +
                 " */\n" +
                 "object RandomSeeds\n" +
                 "{")
        print ("    val seeds = Array (")

        for (i <- 0 until k) {
            if (i == 0)         println (stream.deep + ",")
            else if (i < k - 1) println (INDENT + stream.deep + ",")
            else                println (INDENT + stream.deep + ")") 
            multMatVect (stream, a1p134, M1, a2p134, M2)
        } // for

        println ("\n} // RandomSeeds object" +
                 "\n")
    } // printSeeds

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply the first half of vector 'v' by matrix 'a' with a modulo of 'm1'
     *  and the second half by 'b' with a modulo of 'm2'.
     *  @param v   column vector
     *  @param a   first matrix
     *  @param m1  first modulus
     *  @param b   second matrix
     *  @param m2  second modulus
     */
    private def multMatVect (v: Array [Int], a: Array [Array [Int]], m1: Int,
                                             b: Array [Array [Int]], m2: Int) 
    {
        val vv = v.slice (0, 3)
        matVecModM (a, vv, vv, m1)
        for (i <- vv.indices) v(i) = vv(i)
 
        for (i <- vv.indices) vv(i) = v(i + 3)
        matVecModM (b, vv, vv, m2)
        for (i <- vv.indices) v(i + 3) = vv(i)
    } // multMatVect

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the result of 'a * s mod m' and put the result in vector 'v',
     *  where 's' and 'v' are both column vectors.  This method works even if
     *  's = v'.
     *  @param a  matrix for multiplication 
     *  @param s  column vector
     *  @param v  result vector
     *  @param m  modulus for the computation
     */
    private def matVecModM (a: Array [Array [Int]], s: Array [Int], v: Array [Int], m: Int)
    {
        val x = Array.ofDim [Int] (v.length)
        for (i <- v.indices) {
            x(i) = 0
            for (j <- s.indices) x(i) = multModM (a(i)(j), s(j), x(i), m)
        } // for
        for (i <- v.indices) v(i) = x(i)
    } // matVecModM

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute '(a * s + c) mod m'.  This Works also if 's' or 'c' are negative. 
     *  The result is always positive (and thus always between 0 and m - 1).
     *  @param a  first integer
     *  @param s  second integer
     *  @param c  additive integer
     *  @param m  modulus for the computation
     */
    private def multModM (a: Int, s: Int, c: Int, m: Int): Int =
    {
       val r = ((a.toLong * s + c) % m).toInt
       if (r < 0) r + m else r
    } // multModM   

} // StreamMaker object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StreamMakerGen` object generates and prints the first 'k' seeds for the
 *  the 'MRG31k3p' random number generator's streams.
 */
object StreamMakerGen extends App
{
    StreamMaker.printSeeds ()

} // StreamMakerGen object

