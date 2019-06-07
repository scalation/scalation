
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Dec 30 14:48:41 EST 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package tableau

import java.io.File

import scalation.linalgebra.MatrixD
import scalation.model.Modelable
import scalation.random.{Exponential, Randi, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` class support tableau oriented simulation models in which each
 *  simulation entity's events are recorded in tabular form (in a matrix).
 *  This is analogous to Spreadsheet simulation.
 *  @see http://www.informs-sim.org/wsc06papers/002.pdf
 *  @param name   the name of simulation model
 *  @param m      the number entities to process before stopping
 *  @param rv     the random variate generators to use
 *  @param label  the column labels for the matrix
 */
class Model (name: String, m: Int, rv: Array [Variate], label: Array [String])
      extends Modelable
{
    private val mm  = m.toDouble             // m as a double
    private val n   = label.length           // number of column labels

    /** the table holding information about entity timing
     */
    protected val table = new MatrixD (m+2, n)

    for (i <- 1 to m) table(i, 0) = i                                  // ID-0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform tableau-based simulation by recording timing information about
     *  the 'i'th entity in the 'i'th row of the matrix.  This method may need to
     *  be overridden for other models.
     */
    def simulate (startTime: Double = 0.0)
    {
        for (i <- 1 to m) {
            table(i, 1) = if (i == 1) startTime else rv(0).gen               // IArrival-1
            table(i, 2) = table(i, 1) + table(i-1, 2)                  // Arrival-2
            table(i, 3) = table(i, 2) max table(i-1, 5)                // Start-3
            table(i, 4) = rv(1).gen                                    // Service-4
            table(i, 5) = table(i, 3) + table(i, 4)                    // End-5
            table(i, 6) = table(i, 3) - table(i, 2)                    // Wait-6
            table(i, 7) = table(i, 5) - table(i, 2)                    // Total-7
        } // for
    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report the simulation results.
     */
    def report ()
    {
        for (j <- 0 until n) table(m+1, j) = table.col(j).sum / mm
        for (j <- label.indices) { print ("\t" + label(j)); if (label(j).length < 8) print ("\t") }
        println ()
        println ("table = " + table)
        for (j <- label.indices) { print ("\t" + label(j)); if (label(j).length < 8) print ("\t") }
        println ()
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save the table/matrix in a Comma Separated Value (.csv) file suitable for
     *  opening in a spreadsheet.  Note, the file 'data.tableau.csv' is overwritten.
     */
    def save ()
    {
        table.write ("data" + ⁄ + "tableau.csv")
    } // save

} // Model


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ModelTest` object is used to test the `Model` class.
 *  @see `apps.tableau`for additional examples.
 */
object ModelTest extends App
{
    val maxCusts   = 10                             // stopping rule: simulate maxCustss customers
    val iArrivalRV = Randi (1, 10)                  // customer interarrival time 
    val serviceRV  = Randi (1, 10)                  // customer service time
    val label      = Array ("ID-0", "IArrival-1", "Arrival-2", "Start-3", "Service-4", "End-5", "Wait-6", "Total-7")
    val mm1 = new Model ("G/G/1 Queue", maxCusts, Array (iArrivalRV, serviceRV), label)
    mm1.simulate ()
    mm1.report
    mm1.save ()

} // ModelTest

