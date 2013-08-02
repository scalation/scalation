
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Sep  2 20:50:53 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes MassSpectra.scala
 *  @run     scala -cp ../../classes:classes montecarlo.MassSpectraTest
 */

package montecarlo

import collection.mutable.Set

import scalation.random.{Binomial, Trinomial}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object maintains information about the isotopes of elements commonly
 *  found in organic molecules such as glycans.
 */
object Isotopes
{
    /** Maps elements to an array of abundant (>1E-4) stable isotopes
     *  @see http://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl
     *  Each row is a tuple (baryon count, AMUs in daltons, relative abundance)
     */
    val elementMap = Map ("H" -> Array (( 1,  1.00782503207, .999885),
                                        ( 2,  2.0141017778,  .000115)),
                          "C" -> Array ((12, 12.0000000000,  .9893),
                                        (13, 13.0033548378,  .0107)),
                          "N" -> Array ((14, 14.0030740048,  .99636),
                                        (15, 15.0001088982,  .00364)),
                          "O" -> Array ((16, 15.99491461956, .99757),
                                        (17, 16.99913170,    .00038),
                                        (18, 17.9991610,     .00205)),
                         "Na" -> Array ((23, 22.9897692809, 1.0000)),
                          "P" -> Array ((31, 30.97376163,   1.0000)),
                          "S" -> Array ((32, 31.97207100,    .9499),
                                        (33, 32.97145876,    .0075),
                                        (34, 33.96786690,    .0425)),
                          "K" -> Array ((39, 38.96370668,    .932581),
                                        (40, 39.96399848,    .000117),
                                        (41, 40.96182576,    .067302)))
} // Isotopes object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class calculates the masses and corresponding probabilities for organic
 *  molecules, providing a Monte Carlo simulation of their mass spectra. 
 *  @param thres  the lowest detectable relative abundance (probability) level 
 *  @param noise  the noise level of the mass spectrometer
 */
class MassSpectra (thres: Double = 1E-6, noise: Double = 1E-5)
      extends Error
{
    import Isotopes.elementMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an elemental composition, a set of (element, atom count) tuples, of an
     *  organic molecule, simulate its spectrum, a set of (mass, probability) tuples.
     *  @param comp  the elemental composition of an organic molecule
     */
    def simulate (comp: Set [Tuple2 [String, Int]]): Set [Tuple2 [Double, Double]] =
    {
        var mpSet  = Set [Tuple2 [Double, Double]] ()            // complete set of (mass, prob) tuples
        for ((e, n) <- comp) {                                   // tuple = (element, atom count)
            val mpSet2 = Set [Tuple2 [Double, Double]] ()        // temp set
            val mpeSet = getElementMasses (e, n)                 // set of (mass, prob) tuples for e

	    println("mpeSet = " + mpeSet)

            if (mpSet.isEmpty) {                                 // when e is first element
                mpSet = mpeSet
            } else {
                for ((m, p) <- mpSet; (me, pe) <- mpeSet) {      // iterate over all possibilities
                    val prob = p * pe                            // multiply probabilities
                    if (prob > thres) mpSet2 add (m + me, prob)  // add masses
                } // for
                mpSet = mpSet2
            } // if
        } // for
        mpSet                   // return the complete mass-probability set
     } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get all the masses for n atoms of element e, so long as its relative abundance
     *  (probability) is above threshold thres.  Return a set (mass, prob) tuples.
     *  @param e  the given element (e.g, "O" for Oxygen)
     *  @param n  the number of such atoms in the molecule (e.g., 2 in CO2)
     */
    def getElementMasses (e: String, n: Int): Set [Tuple2 [Double, Double]] =
    {
        val mpeSet = Set [Tuple2 [Double, Double]] ()    // set of (mass, prob) tuples for e
        if (elementMap contains e) {
            val iso = elementMap(e)                      // get the isotopes of element e
            iso.length match {
            case 1 =>                                    // case of 1 isotope
                val prob = 1.0
                mpeSet add (n * iso(0)._2, prob)         // add (mass, prob) tuple to mpSet
            case 2 =>                                    // case of 2 isotopes
                val bin = Binomial (iso(0)._3, n).pmf()  // for n fixed, get distribution over all k
                for (k <- 0 to n) {
                    val prob = bin(k)
                    if (prob > thres)  {
                        mpeSet add (k * iso(0)._2 + (n-k) * iso(1)._2, prob)
                    } // if
                } // for
            case 3 =>                                     // case of 3 isotopes
                val trind = Trinomial (iso(0)._3, iso(1)._3, n)
                for (k <- 0 to n) {
		    val trin = trind.pmf(k)               // for n, k fixed, get distribution over all l
		    for (l <- 0 to n-k) {
		      val prob = trin(l)
		      if (prob > thres) {		        
                          mpeSet add (k * iso(0)._2 + l * iso(1)._2 + (n-k-l) * iso(2)._2, prob)
                      } // if
		    } // for
                } // for
            case _ =>
                flaw ("getElementMasses", "too many isotopes for current system")
            } // match
        } else {
            flaw ("getElementMasses", "element " + e + " not found in elementMap")
        } // if
        mpeSet                                  // return the mass-probability set for this element
    } // getElementMasses

} // MassSpectra class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the MassSpectra class.
 */
object MassSpectraTest extends App
{
    val ms   = new MassSpectra ()
    val comp = Set (("H", 8), ("C", 5), ("O", 3))        // 
    //val comp = Set (("C", 1), ("O", 2))                // Carbon Dioxide
    //val comp = Set (("O", 2))                          // Oxygen
    println ("spectra = " + ms.simulate (comp))

/*------------------------------------------------------------------------------
spectra = Set((43.98982923912, 0.9844978437175699),
              (44.99318407692, 0.010648061182430042),
              (44.99404631956, 7.5004096076E-4))
              (45.99407561956, 0.0040462736041000855),
              (45.99740115736, 8.112239240000031E-6),
              (46.99743045736, 4.37633959000011E-5),
              (47.998322,      4.157533250000175E-6),
------------------------------------------------------------------------------*/

} // MassSpectraTest object

