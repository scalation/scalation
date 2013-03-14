
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import actors.Actor

import scalation.util.PQItem
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SimActor abstract class represents entities that are active in the model.
 *  The 'act' abstract method, which specifies entity behavior, must be defined
 *  for each subclass.  Each SimActor extends Scala's Actor class and therefore
 *  executes in its own thread.
 *  @param name      the name of the entity/SimActor
 *  @param director  the director controlling the model
 */
abstract class SimActor (name: String, director: Model)
//       extends Actor with Ordered [SimActor] with Monitor with Identity with PQItem
         extends Actor with Signals with PQItem with Ordered [SimActor]
{
    setName (name)

    /** Flag indicating whether the actor is new (has yet to act) (FIX protection level)
     */
    private var _yetToAct = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the activation times of the two actors.
     */
    def compare (sa: SimActor): Int = sa.actTime.compare (actTime)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The abstract method, act, is defined in each subclass to provide specific behavior.
     */
    def act ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this actor has yet to act.
     */
    def yetToAct = _yetToAct

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the yetToAct flag to false once a SimActor has acted.
     */
    def nowActing () { _yetToAct = false }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the time on the director's clock
     */
    def time = director.clock

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
        trace (this, "resumes", director, time)
        director ! RESUME_DIRECTOR
        if (quit) {
            exit ()
        } else {
            receive { case RESUME_ACTOR => trace (this, "receives " + RESUME_ACTOR, this, time) }
        } // if
    } // yieldToDirector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare SimActors based on their activation times, which used for ordering
     *  in the director's agenda (a time-based priority queue).
     *  @param actor2  the other actor to compare with this
     *
    def compare (actor2: SimActor) = actTime compare actor2.actTime
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the SimActor's full name and activation time.
     */
    override def toString = "SimActor ( " + me + " at " + actTime + " ) "

} // SimActor

