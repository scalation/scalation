
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Yung Long Li, John A. Miller 
 *  @version 1.0
 *  @date    Sun Nov  4 19:20:05 EST 2012
 *  @see     LICENSE (MIT style license file).
 *  @see     www.scala-lang.org/api/current/scala/collection/mutable/LinkedHashMap.html
 */

package scalation.util

import collection.mutable.LinkedHashMap
import util.Random

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SortedLinkedHashMap class extends the scala LinkedHashMap class.
 *  It maintains the keys (type A) in order, so when you iterate the entry set,
 *  the entries are in key order.  This is useful for merge algorithms, such
 *  as those used for sparse matrices.
 */
class SortedLinkedHashMap [A, B] (implicit val ordering: Ordering [A])
     extends LinkedHashMap [A, B] with Serializable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a SortedLinkedHashMap and put all (key, value) pairs into this map.
     *  @param pairs  the (key, value) pairs to put in this map
     */
    def this (pairs: List [Tuple2 [A, B]]) (implicit ordering: Ordering [A])
    {
        this ()
        for ((k, v) <- pairs) put (k, v)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the first entry in the SortedLinkedHashMap.
     */
    def getFirstEntry () = firstEntry

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put (key, value) pair into this map.
     *  @param key    the key for the map entry
     *  @param value  the value for the map entry
     */
    override def put (key: A, value: B): Option [B] =
    {
        val e = findEntry (key)               // see if key is already in map
        if (e == null) {                      // no old entry found
            val e = new Entry (key, value)    // make a new entry
            addEntry (e)                      // add to map
            updateLinkedEntries (e)           // update links
            None                              // none found before put
        } else {
            val v = e.value                   // save old value
            e.value = value                   // change value in old entry
            Some (v)                          // old value returned
        } // if
    } // put

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put all (key, value) pairs into this map.
     *  @param pairs  the (key, value) pairs to put in this map
     */
    def += (pairs: List [Tuple2 [A, B]]) { for ((k, v) <- pairs) put (k, v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the two keys (to establish the correct ordering).
     *  @param k0  the first key
     *  @param k1  the second key
     */
    def compare (k0: A, k1: A): Int = ordering.compare (k0, k1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Insert a new entry into the position which maintains the correct key order.
     *  @param ent  the new entry to add
     */
    private def updateLinkedEntries (ent: Entry)
    {
        if (lastEntry == null) {           // insert into empty list
            firstEntry = ent
            lastEntry  = ent
        } else {
            // search last to first for insertion position
            var cur = lastEntry
            while (cur != null && compare (cur.key, ent.key) > 0) cur = cur.earlier

            // insert based on insertion position
            ent.earlier = cur
            if (cur == null) {             // insert at beginning: null <-> ent <-> firstEntry
                ent.later = firstEntry
                firstEntry.earlier = ent
                firstEntry = ent
            } else {                       // insert after cur: cur <-> ent <-> cur.later
                ent.later = cur.later
                if (cur.later == null) lastEntry = ent else cur.later.earlier = ent
                cur.later = ent
            } // if
        } // if
    } // updateLinkedEntries

} // SortedLinkedHashMap class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SortedLinkedHashMap class.
 */
object SortedLinkedHashMapTest extends App
{
    val size        = 10
    val ran         = new Random ()
    var insertOrder = true

    for (n <- 0 until size) {
        val smap = new SortedLinkedHashMap [Int, Double]
        for (i <- 0 until size) {
            val k = ran.nextInt (100)
            if (insertOrder) smap(i) = k else smap(k) = i
        } // for
        println ("SortedLinkedHashMap: build: smap = " + smap)

        // iterate through the map entries
        val it = smap.iterator
        while (it.hasNext) println ("SortedLinkedHashMap: iterate: entry = " + it.next) 
        
        // put new entries in the map
        smap += 14 -> 1.
        smap += 11 -> 2.
        println ("SortedLinkedHashMap: put: smap = " + smap)
        
        // remove entry from map according its key
        smap -= 5 
        println ("SortedLinkedHashMap: remove: smap = " + smap)
        insertOrder = ! insertOrder
    } // for

    val smap2 = new SortedLinkedHashMap [Int, Double]
    smap2 += List ((1, 2.), (2, 4.), (3, 8.), (5, 32.), (4, 16.))
    println ("SortedLinkedHashMap: smap2 = " + smap2)

} // SortedLinkedHashMapTest object

