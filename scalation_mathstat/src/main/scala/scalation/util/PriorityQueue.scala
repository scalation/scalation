
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Nov  8 23:26:52 EST 2015
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  @see github.com/scala/scala/blob/v2.11.7/src/library/scala/collection/mutable/PriorityQueue.scala
 *  Added 'decreaseKey' and 'increaseKey' methods and a `PriorityQueueTest` object.
 */

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalation.util

import scala.collection.{IterableLike, GenTraversableOnce, AbstractIterator}
import scala.collection.generic._
import scala.collection.mutable._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PriorityQueue` class implements priority queues using a heap.
 *  To prioritize elements of type `A` there must be an implicit
 *  `Ordering [A]` available at creation.
 *
 *  Only the `dequeue` and `dequeueAll` methods will return methods in priority
 *  order (while removing elements from the heap).  Standard collection methods
 *  including `drop` and `iterator` will remove or traverse the heap in whichever
 *  order seems most convenient.
 *
 *  @tparam A    type of the elements in this priority queue.
 *  @param ord   implicit ordering used to compare the elements of type `A`.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 *  @since   1
 *
 *  @define Coll PriorityQueue
 *  @define coll priority queue
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
// @deprecatedInheritance("PriorityQueue is not intended to be subclassed due to extensive private implementation details.", "2.11.0")
class PriorityQueue [A] (implicit val ord: Ordering [A])
   extends AbstractIterable [A]
      with Iterable [A]
      with GenericOrderedTraversableTemplate [A, PriorityQueue]
      with IterableLike [A, PriorityQueue [A]]
      with Growable [A]
      with Builder [A, PriorityQueue [A]]
      with Serializable
      with scala.Cloneable
{
  import ord._


  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Inner class for accessing resizable arrays.
   */
  private class ResizableArrayAccess [B]
          extends AbstractSeq [B] with ResizableArray [B] with Serializable
  {
    def p_size0 = size0

    def p_size0_= (s: Int) = size0 = s

    def p_array = array

    def p_ensureSize (n: Int) = super.ensureSize (n)

    def p_swap (a: Int, b: Int) = super.swap (a, b)

  } // ResizableArrayAccess inner class


  protected [this] override def newBuilder = new PriorityQueue [A]

  private val resarr = new ResizableArrayAccess [A]

  resarr.p_size0 += 1                  // we do not use array(0)

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the adjusted length.
   */
  def length: Int = resarr.length - 1  // adjust length accordingly

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the size.
   */
  override def size: Int = length

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Determine whether the priority is empty.
   */
  override def isEmpty: Boolean = resarr.p_size0 < 2

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the representation. 
   */
  override def repr = this

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns this priority queue.
   */
  def result = this

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** 
   */
  override def orderedCompanion = PriorityQueue

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Treat 'x' as an instance of type 'A'.
   */
  private def toA (x: AnyRef): A = x.asInstanceOf [A]

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Re-establish the heap order from position 'm' up the tree to the root.
   */
  protected def fixUp (as: Array [AnyRef], m: Int): Unit =
  {
    var k: Int = m
    while (k > 1 && toA (as(k / 2)) < toA (as(k))) {
      resarr.p_swap (k, k / 2)
      k = k / 2
    } // while
    k
  } // fixUp

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Re-establish the heap order from position 'm' down the tree to 'n'.
   */
  protected def fixDown (as: Array [AnyRef], m: Int, n: Int): Unit =
  {
    var k: Int = m
    while (n >= 2 * k) {
      var j = 2 * k
      if (j < n && toA (as(j)) < toA (as(j + 1)))
        j += 1
      if (toA (as(k)) >= toA (as(j)))
        return
      else {
        val h = as(k)
        as(k) = as(j)
        as(j) = h
        k = j
      } // if
    } // while
  } // fixDown

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Inserts a single element into the priority queue.
   *
   *  @param  elem        the element to insert.
   *  @return             this $coll.
   */
  def += (elem: A): this.type =
  {
    resarr.p_ensureSize (resarr.p_size0 + 1)
    resarr.p_array (resarr.p_size0) = elem.asInstanceOf [AnyRef]
    fixUp (resarr.p_array, resarr.p_size0)
    resarr.p_size0 += 1
    this
  } // +=

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Adds all elements provided by a `TraversableOnce` object into the priority queue.
   *
   *  @param  xs    a traversable object.
   *  @return       a new priority queue containing elements of both `xs` and `this`.
   */
  def ++ (xs: GenTraversableOnce [A]): PriorityQueue [A] = { this.clone () ++= xs.seq }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  def enqueue (elems: A*): Unit = { this ++= elems }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the element with the highest priority in the queue, and removes
   *  this element from the queue.
   *
   *  throws java.util.NoSuchElementException
   *  @return   the element with the highest priority.
   */
  def dequeue (): A =
  {
    if (resarr.p_size0 > 1) {
      resarr.p_size0 = resarr.p_size0 - 1
      resarr.p_swap (1, resarr.p_size0)
      fixDown (resarr.p_array, 1, resarr.p_size0 - 1)
      toA (resarr.p_array (resarr.p_size0))
    } else
      throw new NoSuchElementException ("no element to remove from heap")
  } // dequeue

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns all elements in the queue, and removes them from the queue.
   */
  def dequeueAll [A1 >: A, That] (implicit bf: CanBuildFrom [_, A1, That]): That =
  {
    val b = bf.apply ()
    while (nonEmpty) b += dequeue ()
    b.result ()
  } // dequeueAll

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Since the element's priority is being decreased, move it to a lower priority
   *  position (toward the back).
   *
   *  @param  elem       the element to reposition
   *  @param  upElem     the update version of the element to reposition
   */
  def decreaseKey (elem: A, upElem: A)
  {
    if (upElem < elem) {                                   // make sure priority is decreased
      val m = resarr.p_array.indexOf (elem)                // find the element in the heap
      resarr.p_array(m) = upElem.asInstanceOf [AnyRef]     // replace it with its updated version
      fixDown (resarr.p_array, m, resarr.p_size0)          // re-position in heap if needed
    } // if
  } // decreaseKey

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Since the element's priority is being increased, move it to a higher priority
   *  position (toward the front).
   *
   *  @param  elem       the element to reposition
   *  @param  upElem     the update version of the element to reposition
   */
  def increaseKey (elem: A, upElem: A)
  {
    if (upElem > elem) {                                   // make sure priority is increased
      val m = resarr.p_array.indexOf (elem)                // find the element in the heap
      resarr.p_array(m) = upElem.asInstanceOf [AnyRef]     // replace it with its updated version
      fixUp (resarr.p_array, m)                            // re-position in heap if needed
    } // if
  } // increaseKey

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the element with the highest priority in the queue, or throws an error
   *  if there is no element contained in the queue.
   *
   *  @return   the element with the highest priority.
   */
  override def head: A =
  {
    if (resarr.p_size0 > 1) toA (resarr.p_array(1))
    else                    throw new NoSuchElementException ("queue is empty")
  } // head

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   */
  def clear (): Unit = { resarr.p_size0 = 1 }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns an iterator which yields all the elements.  Note: The order of elements
   *  returned is undefined.  If you want to traverse the elements in priority queue
   *  order, use `clone ().dequeueAll.iterator`.
   *
   *  @return  an iterator over all the elements.
   */
  override def iterator: Iterator [A] = new AbstractIterator [A]
  {
    private var i = 1
    def hasNext: Boolean = i < resarr.p_size0
    def next (): A = {
      val n = resarr.p_array(i)
      i += 1
      toA (n)
    } // next
  } // iterator

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Print the priority queue in order.
   */
//def printInOrder: Unit =
  def printInOrder ()
  {
      print ("PriorityQueue( ")
      for (e <- clone ().dequeueAll.iterator) print (e + " ")
      println (")")
  } // printInOrder

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the reverse of this queue. The priority queue that gets returned will
   *  have an inversed ordering - if for some elements `x` and `y` the original
   *  queue's ordering had `compare` returning an integer ''w'', the new one will
   *  return ''-w'', assuming the original ordering abides its contract.
   *  Note that the order of the elements will be reversed unless the `compare`
   *  method returns 0. In this case, such elements will be subsequent, but their
   *  corresponding subinterval may be inappropriately reversed. However, due to
   *  the compare-equals contract, they will also be equal.
   *
   *  @return   A reversed priority queue.
   */
  def reverse =
  {
    val revq = new PriorityQueue [A] () (new scala.math.Ordering [A] {
      def compare(x: A, y: A) = ord.compare(y, x)
    })
    for (i <- 1 until resarr.length) revq += resarr(i)
    revq
  } // reverse

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns an iterator which yields all the elements in the reverse order
   *  than that returned by the method `iterator`.
   *
   *  Note: The order of elements returned is undefined.
   *
   *  @return  an iterator over all elements sorted in descending order.
   */
  def reverseIterator: Iterator [A] = new AbstractIterator [A]
  {
    private var i = resarr.p_size0 - 1
    def hasNext: Boolean = i >= 1
    def next (): A = {
      val n = resarr.p_array(i)
      i -= 1
      toA(n)
    } // next
  } // reverseIterator

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** The hashCode method always yields an error, since it is not safe to use mutable
   *  queues as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode (): Int =
  {
    throw new UnsupportedOperationException ("unsuitable as hash key")
  } // hashCode

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns a regular queue containing the same elements. Note: the order of
   *  elements is undefined.
   */
  def toQueue: Queue[A] = new Queue[A] ++= this.iterator

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns a textual representation of a queue as a string.
   *
   *  @return the string representation of this queue.
   */
  override def toString () = toList.mkString ("PriorityQueue(", ", ", ")")

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Converts this $coll to a list.  Note: the order of elements is undefined.
   *
   *  @return a list containing all elements of this $coll.
   */
  override def toList = this.iterator.toList

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** This method clones the priority queue.
   *
   *  @return  a priority queue with the same elements.
   */
  override def clone (): PriorityQueue [A] = new PriorityQueue [A] ++= this.iterator

} // PriorityQueue class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PriorityQueue` companion object provides builder methods.
 */
object PriorityQueue extends OrderedTraversableFactory [PriorityQueue]
{
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /**
   */
  def newBuilder [A] (implicit ord: Ordering [A]) = new PriorityQueue [A]

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /**
   */
  implicit def canBuildFrom [A] (implicit ord: Ordering [A]): CanBuildFrom [Coll, A, PriorityQueue [A]] =
  {
    new GenericCanBuildFrom [A]
  } // canBuildFrom

} // PriorityQueue object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PriorityQueueTest` object is used to test the `PriorityQueue` class.
 *  Note, element 'e1' has higher priority than 'e2' when 'e1 > e2'.
 *  To change from "largest first" to "smallest first", could use negative numbers
 *  or swap the comparison order.
 *  > run-main scalation.util.PriorityQueueTest
 */
object PriorityQueueTest extends App
{
  case class E (id: Int, key: Double)

  object EOrder extends Ordering [E] { def compare (e1: E, e2: E): Int = e1.key compare e2.key }

  val pq = PriorityQueue (E(0, 5.5), E(1, 2.2), E(2, 4.4), E(3, 3.3), E(4, 1.1))(EOrder)

  pq.increaseKey (E(1, 2.2), E(1, 4.0))      // increase the priority of 1

  while (pq.nonEmpty) println (pq.dequeue ())

} // PriorityQueueTest

