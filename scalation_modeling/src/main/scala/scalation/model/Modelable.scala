
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Oct 10 11:42:43 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.model

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Modelable` trait defines what is common to all models supported in ScalaTion.
 */
trait Modelable
{
    /** The number of seconds in one minute
     */
    protected val MINUTE = 60.0

    /** The number of seconds in one hour
     */
    protected val HOUR = 3600.0

    /** The clock that keep track of the current simulation time
     */
    protected var _clock = 0.0

    /** Simulation execution/termination flag
     */
    protected var simulating = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current value of the director's clock.
     */
    def clock = _clock

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation beginning with 'startTime' and continuing until a
     *  stopping rule evaluates to true.
     *  @param startTime  the start time of the simulation
     */
    def simulate (startTime: Double)

} // Modelable trait

