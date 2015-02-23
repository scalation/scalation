
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import scalation.util.{Locatable, PQItem}
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimActor` abstract class represents entities that are active in the model.
 *  The 'act' abstract method, which specifies entity behavior, must be defined
 *  for each subclass.  Each `SimActor` extends Scala's `Actor` class and may be
 *  roughly thought of as running in its own thread.
 *  @param name      the name of the entity/SimActor
 *  @param director  the director controlling the model
 */
abstract class SimActor (name: String, director: Model)
//       extends Actor with Ordered [SimActor] with Monitor with Identifiable with PQItem
         extends Coroutine with PQItem with Ordered [SimActor] with Locatable
{
    setName (name)

    /** The time at which the entity/sim-actor arrived
     */
    val arrivalT = director.clock

    /** The indicator of subtype of the sim-actor
     */
    private var _subtype = 0

    /** Value of the trajectory along the Qcurve for this sim-actor
     */
    private var _trajectory = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sim-actor's subtype.
     */
    def subtype: Int = _subtype

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the sim-actor's subtype.
     *  @param subtype  the sim-actors subtype
     */
    def setSubtype (subtype: Int) { _subtype = subtype }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current trajectory (along the Qcurve) of this SimActor.
     */
    def trajectory: Double = _trajectory

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the trajectory along the curve for this SimActor.
     *  @param t  the new trajectory for the SimActor
     */
    def setTrajectory (t: Double) { _trajectory = t }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the activation times of the two actors, 'this' and 'actror2'.
     *  Their activation times are used to order them in the director's agenda
     *  (a time-based priority queue).
     *  @param actor2  the other actor to compare with this
     */
    def compare (actor2: SimActor): Int = actor2.actTime compare actTime

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The abstract method, act, is defined in each subclass to provide specific behavior.
     */
    def act ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Schedule a reactivation of this SimActor delay time units in the future.
     *  @param delay  the time delay before reactivation
     */
    def schedule (delay: Double)
    {
        actTime = director.clock + delay
        director.reschedule (this)
    } // schedule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Yield control to the director so the director can take the next action.
     *  @param quit  the flag indicating whether this actor is done
     */
    def yieldToDirector (quit: Boolean = false)
    {
        trace (this, "resumes", director, director.clock)
        yyield (director, quit)
    } // yieldToDirector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the SimActor's full name and activation time.
     */
    override def toString = "SimActor ( " + me + " at " + actTime + " ) "

} // SimActor class

