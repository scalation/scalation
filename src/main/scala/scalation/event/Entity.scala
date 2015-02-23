
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.event

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Entity` class represents a single simulation entity for event-scheduling 
 *  simulation.
 *  @param iArrivalT  the time from the last arrival
 *  @param serviceT   the amount of time required for the entity's next service
 *  @param director   the controller/scheduler that this event is a part of
 */
case class Entity (val iArrivalT: Double, var serviceT: Double, director: Model)
{
    /** the entity id
     */
    val eid = Entity.next ()

    /** the time at which the entity arrived
     */
    val arrivalT = director.clock + iArrivalT

    /** the time at which waiting started
     */
    var startWait = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the entity to a string
     */
    override def toString = "Entity-" + eid

} // Entity class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Entity` companion object provides a counter for generating entity ids.
 */
object Entity
{
    private var eCount = 0           // counter for generating entity ids

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next id number
     */
    def next (): Int = { eCount += 1; eCount }

} // Entity companion object

