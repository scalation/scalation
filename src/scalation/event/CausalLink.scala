
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Dec 6 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.event

import scalation.animation.CommandType._
import scalation.stat.{Statistic, TimeStatistic}
import scalation.random.Variate
import scalation.scala2d.QCurve
import scalation.scala2d.Colors._
import scalation.util.Identity

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class provides casual links between events.  After an event has updated
 *  the state, it checks its causal links to schedule/cancel other events.
 *  @param _name      the name of the causal link
 *  @param condition  the condition under which it is followed
 *  @param makeEvent  function to create an event
 *  @param delay      the time delay in scheduling the event
 *  @param cancel     whether to schedule (default) or cancel the event
 */
case class CausalLink (_name: String, director: Model, condition: () => Boolean, causedEvent: Event,
                       makeEvent: () => Event, delay: Variate, cancel: Boolean = false)
      extends Identity
{
    /** Collector of sample statistics (e.g., service time)
     */
    private val _durationStat = new Statistic ()

    /** Collector of time persistent statistics
     */
    private val _persistentStat = new TimeStatistic ()

    /** Amount of bend in the QCurve
     */
    private val bend = .25

    {
        setName (name)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this CausalLink.
     *  @param from  the starting event prototype
     *  @param to    the ending event prototype
     */
    def display (from: Event, to: Event)
    {
        if (from.id == to.id) {
            director.animate (this, CreateEdge, green, new QCurve (), from, to, Array (16. * bend))
        } else {
            director.animate (this, CreateEdge, green, new QCurve (), from, to, Array (bend))
        } // if
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tally the duration (e.g., service time) of a delay on a causal link.
     *  @param duration  the time duration
     */
    def tally (duration: Double) { _durationStat.tally (duration) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Accumulate the a value (e.g., number in  queue) weighted by its time duration.
     *  @param value  the value to accumulate
     *  @param time   the current time of the observation
     */
    def accumulate (value: Double, time: Double) { _persistentStat.accumulate (value, time) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sample statistics for durations for the component.
     */
    def durationStat = _durationStat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Persistent statistics for value for the component.
     */
    def persistentStat = _persistentStat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the causal link to a string.
     */
    override def toString = "CausalLink ( " + getClass.getSimpleName () + ", " + name + ", " + cancel + " )"

} // CausalLink class

