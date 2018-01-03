
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.4
 *  @date    Thu Sep 24 04:28:31 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 */

package scalation.util

import scala.collection.mutable.MutableList

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MultiSet` class provides an implementation for Union, Intersection and
 *  Subset for the generic `MultiSet` data type that extends `MutableList`.
 */
class MultiSet [T] 
      extends MutableList [T] 
{ 
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'b' is a subset of 'this' `MultiSet`.
     *  @param b  the other `MultiSet`
     */
    def subsetOf (b: MultiSet [T]): Boolean = super.diff (b).isEmpty
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'b' is a subset of 'this' `MultiSet`.
     *  @param b  the other `MultiSet`
     */
    def ⊆ (b: MultiSet [T]): Boolean = super.diff (b).isEmpty

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new `MultiSet` that is the union of 'this' and 'b'.
     *  @param b  the other `MultiSet`
     */
    def union (b: MultiSet [T]): MultiSet [T] = MultiSet (super.union (b))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new `MultiSet` that is the intersection of 'this' and 'b'.
     *  @param b  the other `MultiSet`
     */
    def intersect (b: MultiSet [T]): MultiSet [T] = MultiSet (super.intersect (b))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new `MutableList` that is the union of 'b' and 'this'. 
     *  More efficient since it does not create a new `MultiSet`.
     *  @param b  the other `MultiSet`
     */
    def ⋃ (b: MultiSet [T]): MutableList [T] = super.union (b)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new `MutableList` that is the intersection of 'b' and 'this'.
     *  More efficient since it does not create a new `MultiSet`.
     *  @param b  the other `MultiSet`
     */
    def ⋂ (b: MultiSet [T]): MutableList [T] = super.intersect (b)

} // MultiSet class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MultiSet` object is the companion object for the `MultiSet' class.
 */
object MultiSet 
{   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `MultiSet` from the argument list of elements 'es'.
     *  @param es  one or more elements for the `MultiSet`
     */
    def apply [T] (es: T*): MultiSet [T] = 
    {
        val ms = new MultiSet [T] ()
        for (e <- es) ms += e
        ms
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `MultiSet` from a `MutableList`.
     *  @param ml  the mutable list of elements for the `MultiSet`
     */ 
    def apply [T] (ml: MutableList [T]): MultiSet [T] =
    {
        MultiSet ((for (e <- ml) yield e) :_*)
    } // apply

} // MultiSet object

import MultiSet._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MultiSetTest` object is used to test the `MultiSet` class.
 *  > runMain scalation.util.MultiSetTest
 */
object MultiSetTest extends App 
{
    banner ("Test MultiSet with Integer elements")
    val l1 = MultiSet (1, 2)
    val l2 = MultiSet (1, 1, 2, 3)
    val l3 = MultiSet (1, 1, 2)
    val v4 = MutableList (1, 2, 3)

    println ("l1 = " + l1)
    println ("l2 = " + l2)
    println ("l3 = " + l3)
    println ("lv = " + MultiSet (v4))
    println ("l1 ⊆ l2  = " + l1 ⊆ l2)
    println ("l2 ⊆ l3  = " + l2 ⊆ l3)
    println ("l1 ⊆ l3  = " + l1 ⊆ l3)
    println ("l3 ⋃  l2 = " + l3 ⋃  l2)
    println ("l3 ⋂  l2 = " + l3 ⋂  l2)
    println ("l1 ⋂  l2 = " + l1 ⋂  l2)
    println ("l1 union l2     = " + (l1 union  l2))
    println ("l1 intersect l2 = " + (l1 intersect l2))
   
    banner ("Test MultiSet with String elements")
    val l4 = MultiSet ("a", "b")
    val l5 = MultiSet ("a", "b", "c")
    val l6 = MultiSet ("a", "a", "b")

    println ("l4 = " + l4)
    println ("l5 = " + l5)
    println ("l6 = " + l6)
    println ("l4 ⊆ l5  = " + l4 ⊆ l5)
    println ("l6 ⊆ l5  = " + l6 ⊆ l5)
    println ("l6 ⋃  l5 = " + l6 ⋃  l5)
    println ("l6 ⋂  l5 = " + l6 ⋂  l5)

} // MultiSetTest object

