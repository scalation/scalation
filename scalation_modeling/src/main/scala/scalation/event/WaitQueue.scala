
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Feb  5 19:43:10 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.event

import scala.collection.mutable.Queue

import scalation.stat.{Statistic, TimeStatistic}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WaitQueue` class is used to hold entities waiting for service and
 *  collect statistics on waiting times.  When the queue is full, entities
 *  are 'barred' from entering the queue.
 *  @param director  the controller/scheduler that this event is a part of
 *  @param ext       the extension to distinguish the wait queues
 *  @param cap       the capacity of the queue (defaults to unbounded)
 */
case class WaitQueue (director: Model, ext: String = "", cap: Int = Int.MaxValue)
     extends Queue [Entity]
{
    /** The number of entities barred from entering due to the wait queue being full
     */
    private var _barred = 0

    /** The time in Queue (t_q) sample statistics
     */
    val t_q_stat = new Statistic ("t_q" + ext)

    /** The number in Queue (l_q) time-persistent statistics
     */
    val l_q_stat = new TimeStatistic ("l_q" + ext)

    director.addStats (t_q_stat, l_q_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number entities barred because of this wait queue being full.
     */
    def barred: Int = _barred

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the entity 'ent' in the wait queue and record when waiting started.
     *  @param  the entity to be enqueued
     */
    override def += (ent: Entity): WaitQueue.this.type =
    {
         ent.startWait = director.clock
         if (length <= cap) super.+= (ent) else _barred += 1
         null
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Dequeue the first entity from the wait queue and record the time in the
     *  wait queue.
     */
    override def dequeue (): Entity =
    {
         val ent = super.dequeue ()
         val timeInQ = director.clock - ent.startWait
         t_q_stat.tally (timeInQ)
         l_q_stat.accum (length + 1, director.clock)
         ent
    } // dequeue

} // WaitQueue class

