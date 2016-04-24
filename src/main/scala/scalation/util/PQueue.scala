
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import collection.mutable.ResizableArray
import math.random

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PQItem` trait should be mixed in for items going on a `PQueue`.
 */
trait PQItem extends Identifiable
{
    /** The activation time for the item in the time-ordered priority queue
     */
    var actTime: Double = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two items `PQItems` based on their 'actTime'.
     *  @param other  the other item to compare with this item
     */
    def compare (other: PQItem) = { actTime compare other.actTime }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the item `PQItem` to a string.
     */
    override def toString = "PQItem (" + me + ", " + actTime + ")"

} // PQItem


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PQueue` class provides a simple linear implementation for priority queues.
 *  Once bug in scala 2.8 if fixed, may wish to switch to logarithmic implementation
 *  in `scala.collection.mutable.PriorityQueue`.
 */
class PQueue [T <: PQItem]
      extends ResizableArray [T] with Serializable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add an item to the priority queue ordered by 'actTime'.
     *  @param item  the item to add
     */
    def += (item: T)
    {
        var i = size0 - 1
        size0 = i + 2
        ensureSize (size0)
        while (i >= 0 && item.actTime < this(i).actTime) {
            this(i + 1) = this(i)
            i -= 1
        } // while
        this(i + 1) = item
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the specific item from the priority queue.
     *  @param item  the item to remove
     */
    def -= (item: T): Boolean =
    {
        var found = false
        var i     = 0
        while (i < size0 && ! found) {
            if (this(i).id == item.id) found = true
            i += 1
        } // while
        if (found) {
            // println ("item to remove found at = " + (i - 1) + ", queue size = " + size0)          
            while (i < size0) { this(i - 1) = this(i); i += 1 }
            // this (size0 - 1) = null
            size0 -= 1
        } // if
        found
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove and return the first item (least 'actTime') from the priority queue.
     */
    def dequeue (): T =
    {
        val item = this(0)
        for (i <- 0 until size0 - 1) this(i) = this(i + 1)
        // this (size0 - 1) = null
        size0 -= 1
        item
    } // dequeue 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size (number of contained items) of the priority queue.
     */
    //def size = size0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate whether the priority queue is empty.
     */
    override def isEmpty: Boolean = size0 == 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the contents of the priority queue.
     */
    override def toString: String =
    {
        var s = "PQueue ( " 
        foreach (s += _ + " ")
        s + ")"
    } // toString

} // PQueue class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PQueueTest` object is used to test the `PQueue` class.
 */
object PQueueTest extends App
{
    class Element (time: Double) extends PQItem
    {
        {
            actTime = time        // must set activation time
        } // primary constructor
    } // Element class

    val pq = new PQueue [Element]
    for (i <- 1 to 10) pq += new Element (10.0 * random)
    while ( ! pq.isEmpty) println (pq.dequeue ())

    val e1 = new Element (10.0)
    val e2 = new Element (30.0)
    val e3 = new Element (20.0)
    pq += e1
    pq += e2
    pq += e3
    println ("pq = " + pq)
    println ("found = " + (pq -= e3) + ", queue size = " + pq.size)
    println ("pq = " + pq)

} // PQueueTest object

