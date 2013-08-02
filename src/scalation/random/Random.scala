
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Sep 30 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import math.pow

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates random real numbers in the range (0, 1).
 *  It is a Multiplicative Linear Congruential Generator (MLCG) shown to have
 *  statistical properties adequate for simple simulations (x = ax % m).
 *  In case a better generator is needed, a Multiple Recursive Generator (MRG)
 *  should be used.
 *  @see http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.1024
 *  @param stream  the random number stream index
 */
case class Random (stream: Int = 0)
{
//  private val a: Long = 16807l        // multiplier for a popular 32-bit generator
//  private val m: Long = 2147483647l   // modulus for a popular 32-bit generator (2^31 - 1)
//  def checkM: Boolean = m == pow (2, 31) - 1

    /** The multiplier - a primitive element modulo-m
     */
    private val a: Long = 530399l

    /** The modulus - a large prime number (2^33 - 9)
     */
    private val m: Long = 8589934583l
    def checkM: Boolean = m == pow (2, 33) - 9

    /** The starting (stream 0) seed
     */
    private val seed0 = 1l

    /** Divide the main stream into
     *  10 mini-streams (0, 10, 20, 30, 40, 50, ... , 80, 90) or
     *  100 micro-streams (0, 1, 2, 3, 4, 5, ... , 98, 99)
     */
    private val nStreams = 100

    /** Seeds for all the streams
     */
    private val seeds: Array [Long] = Array (
                 1l,    2054618423l,    7999263932l,    1277953890l,    2682728002l,
        8355538080l,    4830021454l,    3929414814l,    1089222674l,    1580741903l,
         809555195l,    2717632235l,    7175650126l,    6952344063l,    2988067958l,
        5284951711l,    3079615556l,      15300810l,    3785043392l,    5649153646l,
        6301835101l,    7092284905l,    4540092147l,    4666709313l,    1001117845l,
        4263059332l,    4251095828l,    1345764873l,    8126784776l,    5309441349l,
        8407102731l,    1993905144l,    4142590931l,    2354344297l,    4709531018l,
        6060899471l,    6129890359l,    6196388105l,    3648280341l,    2883176831l,
           6835220l,    7578100113l,    8486096776l,     100487683l,    8250219927l,
        8573447168l,    5314218085l,    8566245490l,    8504000520l,    6893950438l,
        6620421794l,     928663362l,    3764775583l,    3172889993l,    3388442486l,
        5976358208l,     396676245l,    1948980175l,    1230123107l,     334682593l,
        4784108311l,    6039389851l,    6984461977l,    1490807498l,    7158533521l,
         516128780l,    3511548060l,    7397479429l,    3667416280l,    5984589647l,
        2111445732l,    7288853046l,    3621371940l,     468536893l,    3834276456l,
        1825603305l,    8084275714l,    7009164972l,    3909596445l,    8349319641l,
        8168186046l,    5903992386l,    6269627001l,    4251338580l,    6705492240l,
        4832972660l,    5684681584l,     104634090l,    1500686680l,    1209816588l,
        6373503190l,     325755960l,    4761644751l,    6749197125l,    1290789842l,
        5773188021l,    8091832448l,    8223717950l,    7676073986l,     724853815l)

    /** Set the stream value to the seed for that stream
     */
    private var x: Long = seeds(stream)      // multi-stream
//  private var x: Long = seed0              // uni-stream

    /** The reciprocal of the modulus
     */
    private val one_by_m: Double = 1.0 / m.asInstanceOf [Double]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean for the random number generator's gen method.
     */
    def mean: Double = 0.5

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next random number as a Double in the interval (0, 1).
     */
    def gen: Double = { x = a * x % m; x * one_by_m }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next stream value as a Long in the set {1, 2, ... , m-1}.
     */
    def lgen: Long = { x = a * x % m; x }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and print out 100 (nStreams) seed values as a val declaration that
     *  can be copied into a class to initialize the seeds (as done above).
     */
    def findSeeds
    {
        val streamLen = (m - 1) / nStreams
        println ("    private val seeds: Array [Long] = Array (")
        var seed = seed0
        for (i <- 0 until nStreams) {
            if (i % 5 != 4) {
                print ("\t" + seed + "l,")
                if (seed < 100000l) print ("\t")
            } else if (i != nStreams - 1) {
                println ("\t" + seed + "l,")
            } else {
                println ("\t" + seed + "l)")
            } // if
            for (j <- 0l until streamLen) seed = lgen
        } // for
    } // find seeds

} // Random class

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates random real numbers in the range (0, 1).
 *  It uses Scala's built-in random number generator.
 *  @param stream  the random number stream index
 *
case class Random (stream: Int = 0)
{
     def mean: Double = 0.5

     def gen: Double = random

} // Random class
 */

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Random Number Generator (RNG) object provides multiple pre-built
 *  random number streams.
 */
object RNG
{
    /** Array of random number streams
     */
    val rand = Array [Random] (
         Random (0),  Random (1),  Random (2),  Random (3),  Random (4),
         Random (5),  Random (6),  Random (7),  Random (8),  Random (9),
         Random (10), Random (11), Random (12), Random (13), Random (14),
         Random (15), Random (16), Random (17), Random (18), Random (19),
         Random (20), Random (21), Random (22), Random (23), Random (24),
         Random (25), Random (26), Random (27), Random (28), Random (29),
         Random (30), Random (31), Random (32), Random (33), Random (34),
         Random (35), Random (36), Random (37), Random (38), Random (39),
         Random (40), Random (41), Random (42), Random (43), Random (44),
         Random (45), Random (46), Random (47), Random (48), Random (49),
         Random (50), Random (51), Random (52), Random (53), Random (54),
         Random (55), Random (56), Random (57), Random (58), Random (59),
         Random (60), Random (61), Random (62), Random (63), Random (64),
         Random (65), Random (66), Random (67), Random (68), Random (69),
         Random (70), Random (71), Random (72), Random (73), Random (74),
         Random (75), Random (76), Random (77), Random (78), Random (79),
         Random (80), Random (81), Random (82), Random (83), Random (84),
         Random (85), Random (86), Random (87), Random (88), Random (89),
         Random (90), Random (91), Random (92), Random (93), Random (94),
         Random (95), Random (96), Random (97), Random (98), Random (99))

} // RNG object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SeedFinder object find seeds for all the random number streams.
 */
object SeedFinder extends App
{
    val rng = Random ()
    rng.findSeeds

} // SeedFinder object

