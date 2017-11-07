
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Fri Mar 28 14:14:24 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import java.util.Date

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StreamMaker3` object finds seeds for the `Random3` random number generator.
 *  This generator has a period length around 2^31.  Each seed is a 32-bit integer.
 */
object StreamMaker3 extends App
{
    val r = Random3 ()
    printSeeds (RandomSeeds.seeds.length)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print out 'k' seed values to initialize the streams for the Random3 generator.
     *  @param k  the number of seeds to print
     */
    def printSeeds (k: Int)
    {
        val streamLen = ((r.getM - 1) / k).toInt
        var seed      = 123
        val INDENT    = "                       "
        val COLS      = 5
        val ROWS      = k / COLS

        println ("\n" +
                 "//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n" +
                 "/** @author  John Miller (code generated)\n" +
                 " *  @version 1.4\n" +
                 " *  @date    " + new Date () + "\n" +
                 " *  @see     LICENSE (MIT style license file).\n" +
                 " */\n" +
                 "\n" +
                 "package scalation.random\n" +
                 "\n" +
                 "//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n" +
                 "/** The first " + k + " seeds for the LCG random number generator.\n" +
                 " */\n" +
                 "object RandomSeeds3\n" +
                 "{")
        print ("    val seeds = Array (")

        for (i <- 0 until ROWS; j <- 0 until COLS) {
            if (i > 0 && j == 0)   print (INDENT)
            if (j < COLS-1)      { print ("\t" + seed + ","); if (seed < 1000000) print ("\t") }
            else if (i < ROWS-1) println ("\t" + seed + ",")
            else                 println ("\t" + seed + ")")
            for (j <- 0 until streamLen) seed = r.igen              // iterate for the next seed
        } // for

        println ("\n} // RandomSeeds3 object" +
                 "\n")
    } // printSeeds

} // StreamMaker3 object

