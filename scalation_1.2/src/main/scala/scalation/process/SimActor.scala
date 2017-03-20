
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import scalation.util.{Locatable, PQItem}
import scalation.util.Error
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimActor` abstract class represents entities that are active in the model.
 *  The 'act' abstract method, which specifies entity behavior, must be defined
 *  for each subclass.  Each `SimActor` extends Scala's `Actor` class and may be
 *  roughly thought of as running in its own thread.
 *  @param label     the label/name of the entity (`SimActor`)
 *  @param director  the director controlling the model
 */
abstract class SimActor (label: String, director: Model)
//       extends Actor with Ordered [SimActor] with Monitor with Identifiable with PQItem
         extends Coroutine (label) with PQItem with Ordered [SimActor] with Locatable with Error
{
    name = label           // set the name for 'this' entity (`SimActor`

    /** The time at which 'this' entity (`SimActor`) arrived
     */
    var arrivalT = director.clock

    /** The indicator of subtype of 'this' entity (`SimActor`) 
     */
    var subtype = 0

    /** The indicator of subtype of 'this' entity (`SimActor`)
     */
    var mySource: Source = null 

    /** Value of the trajectory along the `QCurve` for this entity (`SimActor`)
     */
    private var _trajectory = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the current trajectory (along the `QCurve`) of 'this' `SimActor`.
     */
    def trajectory: Double = _trajectory

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value of the trajectory along the `QCurve` for 'this' `SimActor`.
     *  @param t  the new trajectory for the `SimActor`
     */
    def trajectory_= (trajectory: Double)
    {
        if (trajectory >= 0.0) _trajectory = trajectory
        else flaw ("trajectory_=", "the trajectory can not be negative")
    } // trajectory_=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the activation times of the two actors, 'this' and 'actor2'.
     *  Their activation times are used to order them in the director's agenda
     *  (a time-based priority queue).
     *  @param actor2  the other actor to compare with this
     */
    def compare (actor2: SimActor): Int = actor2.actTime compare actTime

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The abstract method, 'act', is defined in each subclass to provide specific
     *  behavior.
     */
    def act ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Schedule a reactivation of 'this' `SimActor` delay time units in the future.
     *  @param delay     the time delay before reactivation
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
    /** Show the `SimActor`s full name and activation time.
     */
    override def toString = "SimActor ( " + me + " at " + actTime + " ) "

} // SimActor class

