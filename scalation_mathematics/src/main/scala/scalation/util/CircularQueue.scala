
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Santosh Uttam Bobade, John Miller
 *  @version 1.6
 *  @date    Sat Aug  4 15:06:28 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see courses.cs.vt.edu/cs3114/Spring09/book.pdf
 */

package scalation.util

import java.nio.{BufferOverflowException, BufferUnderflowException}

import scala.collection.{LinearSeq, LinearSeqOptimized}
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CircularQueue` provides a circular queue that can be used to store the
 *  latest 'cap' elements.
 *  @param cap  the capacity or maximum number of elements that can be stored
 */
class CircularQueue [A: ClassTag] (cap: Int)
      extends LinearSeqOptimized [A, CircularQueue [A]]
{
    private val maxSize = cap + 1                              // need space one more than max elements
    private val store   = Array.ofDim [A](maxSize)             // storage space for elements
    private var front   = 1                                    // index of front of circular queue
    private var rear    = 0                                    // index of rear of circular queue
    private var nElem   = 0                                    // number of elements in queue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current size of the queue (number of elements contained).
     */
    override def size: Int = nElem

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the queue storage array as a sequence.
     */
    def seq: LinearSeq [A] = store.toList

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** FIX - TBD
     */
    protected [this] def newBuilder: Builder [A, CircularQueue [A]] = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether the queue is empty (i.e., has no element).
     */
    override def isEmpty: Boolean = nElem == 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether the queue is full (i.e., no space left for new elements).
     */
    def isFull: Boolean = nElem == cap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add element 'elem' to the front of 'this' circular queue, throwing an
     *  exeception when the queue is full.
     *  @param elem  the element to add
     */
    def enqueue (elem: A): CircularQueue.this.type =
    {
        if (isFull) throw new BufferOverflowException ()       // isFull => throw exception
        rear = (rear + 1) % maxSize                            // advance rear index
        store(rear) = elem                                     // store new element
        nElem += 1
        this
    } // enqueue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add element 'elem' to the front of 'this' circular queue, overwriting
     *  the oldest element when queue is full.
     *  @param elem  the element to add
     */
    def += (elem: A): CircularQueue.this.type =
    {
        if (isFull) {
            front  = (front + 1) % maxSize                     // isFull => drop oldest element
            nElem -= 1
        } // if
        rear = (rear + 1) % maxSize                            // advance rear index
        store(rear) = elem                                     // store new element
        nElem += 1
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the oldest element from the rear of 'this' circular queue, throwing
     *  an exception when the queue is empty
     */
    def dequeue (): A =
    {
        if (isEmpty) throw new BufferUnderflowException ()     // isEmpty => throw exception
        val elem = store(front)                                // remove oldest element
        front    = (front + 1) % maxSize                       // advance front index
        nElem   -= 1
        elem                                                   // return the oldest element
    } // dequeue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the oldest element from the rear of 'this' circular queue, returning
     *  null when the queue is empty.
     */
    def dequeueOrNull (): A =
    {
        if (isEmpty) return null.asInstanceOf [A]              // isEmpty => return null
        val elem = store(front)                                // remove oldest element
        front    = (front + 1) % maxSize                       // advance front index
        nElem   -= 1
        elem                                                   // return the oldest element
    } // dequeue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' circular queue into a string.
     *  FIX - also print empty slot
     */
    override def toString: String = s"CircularQueue (${store.deep})"

} // CircularQueue class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CircularQueueTest` object is used to test the `CircularQueue` class.
 *  > runMain scalation.util.CircularQueueTest
 */
object CircularQueueTest extends App
{
    val qu = new CircularQueue [Int](4)
    println (s"size = ${qu.size}")
    qu += 10
    println (s"size = ${qu.size}")
    qu += 20
    println (s"size = ${qu.size}")
    qu += 30
    println (s"size = ${qu.size}")
    qu += 40
    println (s"size = ${qu.size}")
    qu += 50
    println (s"size = ${qu.size}")
    println (s"qu = $qu")
    println (s"dequeue = ${qu.dequeue}")
    println (s"size = ${qu.size}")
    println (s"dequeue = ${qu.dequeue}")
    println (s"size = ${qu.size}")
    println (s"dequeue = ${qu.dequeue}")
    println (s"size = ${qu.size}")
    println (s"dequeue = ${qu.dequeue}")
    println (s"size = ${qu.size}")

} // CircularQueueTest object

