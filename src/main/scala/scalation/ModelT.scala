
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Oct 10 11:42:43 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import collection.mutable.ListBuffer

import scalation.stat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The ModelT trait defines what is common to all models supported in ScalaTion.
 */
trait ModelT
{
    /** The clock that keep track of the current simulation time
     */
    protected var _clock = 0.0

    /** Simulation execution/termination flag
     */
    protected var simulating = false

    /** Animation execution flag
     */
    protected var animating = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current value of the director's clock.
     */
    def clock = _clock

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the simulation (includes scheduling all Sources) returning summary
     *  statistics.
     *  @param startTime  the start time of the simulation
     */
    def simulate (startTime: Double): ListBuffer [Statistic]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation.
     */
    def report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the statistical results of the simulation (statistics for each part).
     */
    def getStatistics: ListBuffer [Statistic]

} // ModelT trait

