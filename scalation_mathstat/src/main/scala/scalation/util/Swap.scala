
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Nov  5 19:29:13 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util 

import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Swap` class provides a method to swap elements in an `Array` or `ArrayBuffer`.
 *  Note, `ArrayBuffer` is resizable (similar to Java's `ArrayList`).
 */
object Swap
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap two elements in a regular array.
     *  @param i  first element to swap
     *  @param j  second element to swap
     */
    def swap [T] (a: Array [T], i: Int, j: Int)
    {
        val t = a(i); a(i) = a(j); a(j) = t
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap two elements in a resizable array.
     *  @param i  first element to swap
     *  @param j  second element to swap
     */
    def swap [T] (a: ArrayBuffer [T], i: Int, j: Int)
    {
        val t = a(i); a(i) = a(j); a(j) = t
    } // swap

} // Swap object

import Swap.swap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This `SwapTest` is used to test the `Swap` object.
    > run-main scalation.util.SwapTest
 */
object SwapTest extends App
{
    val a = Array (0, 1, 2, 3, 4, 5, 6, 7)
    swap (a, 1, 6)
    println (a.deep)
    swap (a, 2, 5)
    println (a.deep)

    val b = ArrayBuffer (0, 1, 2, 3, 4, 5, 6, 7)
    swap (b, 1, 6)
    println (b)
    swap (b, 2, 5)
    println (b)

} // SwapTest object

