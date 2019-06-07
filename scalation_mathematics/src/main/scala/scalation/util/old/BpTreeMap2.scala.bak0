
/************************************************************************************
 * @file BpTreeMap2.java
 *
 * @author  John Miller
 */

import scala.reflect.ClassTag
//import java.lang.reflect.Array
import java.util._

import scala.math.ceil

/************************************************************************************
 * The BpTreeMap2 class provides B+Tree maps.  B+Trees are used as multi-level index
 * structures that provide efficient access for both point queries and range queries.
 * All keys will be at the leaf level with leaf nodes linked by references.
 * Internal nodes will contain divider keys such that each divider key corresponds to
 * the largest key in its left subtree (largest left).  Keys in left subtree are "<=",
 * while keys in right substree are ">".
 */
class BpTreeMap2 [K <: Comparable [K]: ClassTag, V: ClassTag] ()
       extends AbstractMap [K, V]
       with Serializable with Cloneable with SortedMap [K, V]
{
    /** The debug flag.
     */
    private val DEBUG = true

    /** The maximum fanout (number of children) for a B+Tree node.
     *  May wish to increase for better performance.
     */
    private val ORDER = 5

    /** The maximum fanout (number of children) for a big B+Tree node.
     */
    private val BORDER = ORDER + 1

    /** The ceiling of half the ORDER.
     */
    private val MID = ceil (ORDER / 2.0).toInt

    /********************************************************************************
     * This inner class defines nodes that are stored in the B+tree map.
     * @param p       the order of the node (max refs)
     * @param isLeaf  whether the node is a leaf
     */
    class Node (p: Int, val isLeaf: Boolean = true)
    {
        var nKeys = 0                                        // number of active keys
        val key   = Array.ofDim [K] (p-1)                    // array of keys
        val ref   = Array.ofDim [Any] (p)                    // array of references/pointers

        /****************************************************************************
         * Copy keys and ref from node n to this node.
         * @param n     the node to copy from
         * @param from  where in n to start copying from
         * @param num   the number of keys/refs to copy
         */
        def copy (n: Node, from: Int, num: Int)
        {
            nKeys = num
            for (i <- 0 until num) { key(i) = n.key(from+i); ref(i) = n.ref(from+i) }
            ref(num) = n.ref(from+num)
        } // copy

        /****************************************************************************
         * Find the "<=" match position in this node.
         * @param k  the key to be matched.
         * @return  the position of match within node, where nKeys indicates no match
         */
        def find (k: K): Int =
        {
            for (i <- 0 until nKeys) if (k.compareTo (key(i)) == 0) return i
            nKeys
        } // find
        
        override def toString: String = "" + key.deep

    } // Node inner class

    /** The root of the B+Tree
     */
    private var root = new Node (ORDER, true)

    /** The first (leftmost) leaf in the B+Tree
     */
    private val firstLeaf = root

    /** A big node to hold all keys and references/pointers before splitting
     */
    private val bn = new Node (BORDER, true)
    
    /** Flag indicating whether a split at the level below has occured that needs to be handled
     */
    private var hasSplit = false

    /** The counter for the number nodes accessed (for performance testing)
     */
    private var count = 0 

    /** The counter for the total number of keys in the B+Tree Map
     */
    private var keyCount = 0

    def stats = (count, keyCount, count / keyCount.toDouble)

    /********************************************************************************
     * Return null to use the natural order based on the key type.  This requires the
     * key type to implement Comparable.
     */
    def comparator (): Comparator [_ >: K] =
    {
        null
    } // comparator

    /********************************************************************************
     * Return a set containing all the entries as pairs of keys and values.
     * @return  the set view of the map
     */
    def entrySet: Set [Map.Entry [K, V]] =
    {
        val enSet = new HashSet [Map.Entry [K, V]] ()

        //  T O   B E   I M P L E M E N T E D
            
        enSet
    } // entrySet

    /********************************************************************************
     * Given the key, look up the value in the B+Tree map.
     * @param key  the key used for look up
     * @return  the value associated with the key or null if not found
     */
    override def get (key: Any): V =
    {
        find (key.asInstanceOf [K], root)
    } // get

    /********************************************************************************
     * Put the key-value pair in the B+Tree map.
     * @param key    the key to insert
     * @param value  the value to insert
     * @return  null, not the previous value for this key
     */
    override def put (key: K, value: V): V =
    {
        insert (key, value, root)
        null.asInstanceOf [V]
    } // put

    /********************************************************************************
     * Return the first (smallest) key in the B+Tree map.
     * @return  the first key in the B+Tree map.
     */
    def firstKey: K = firstLeaf.key(0)

    /********************************************************************************
     * Return the last (largest) key in the B+Tree map.
     * @return  the last key in the B+Tree map.
     */
    def lastKey: K = 
    {
        //  T O   B E   I M P L E M E N T E D

        null.asInstanceOf [K]
    } // lastKey

    /********************************************************************************
     * Return the portion of the B+Tree map where key < toKey.
     * @return  the submap with keys in the range [firstKey, toKey)
     */
    def headMap (toKey: K): SortedMap [K, V] =
    {
        //  T O   B E   I M P L E M E N T E D

        null
    } // headMap

    /********************************************************************************
     * Return the portion of the B+Tree map where fromKey <= key.
     * @return  the submap with keys in the range [fromKey, lastKey]
     */
    def tailMap (fromKey: K): SortedMap [K, V] =
    {
        //  T O   B E   I M P L E M E N T E D

        null
    } // tailMap

    /********************************************************************************
     * Return the portion of the B+Tree map whose keys are between fromKey and toKey,
     * i.e., fromKey <= key < toKey.
     * @return  the submap with keys in the range [fromKey, toKey)
     */
    def subMap (fromKey: K, toKey: K): SortedMap [K, V] =
    {
        //  T O   B E   I M P L E M E N T E D

        null
    } // subMap

    /********************************************************************************
     * Return the size (number of keys) in the B+Tree.
     * @return  the size of the B+Tree
     */
    override def size: Int = keyCount

    def printTree () { printT (root, 0) }

    /********************************************************************************
     * Print the B+Tree using a pre-order traveral and indenting each level.
     * @param n      the current node to print
     * @param level  the current level of the B+Tree
     */
    private def printT (n: Node, level: Int)
    {
        if (n == root) println ("BpTreeMap2")
        println ("-" * 50)

        for (j <- 0 until level) print ("\t")
        print ("[ . ")
        for (i <- 0 until n.nKeys) print (n.key(i) + " . ")
        println ("]")
        if ( ! n.isLeaf) {
            for (i <- 0 to n.nKeys) print (n.ref(i).asInstanceOf [Node], level + 1)
        } // if

        if (n == root) println ("-" * 50)
    } // printT

    /********************************************************************************
     * Recursive helper function for finding a key in B+trees.
     * @param key  the key to find
     * @param n    the current node
     */
    def find (key: K, n: Node): V =
    {
        count += 1
        val i = n.find (key)
        if (i < n.nKeys) {
            val k_i = n.key(i)
            if (n.isLeaf) return if (key == k_i) n.ref(i).asInstanceOf [V] else null.asInstanceOf [V]
            else          return find (key, n.ref(i).asInstanceOf [Node])
        } else {
            return if (n.isLeaf) null.asInstanceOf [V] else find (key, n.ref(n.nKeys).asInstanceOf [Node])
        } // if
    } // find

    /********************************************************************************
     * Recursive helper function for inserting a key in B+trees.
     * @param key  the key to insert
     * @param ref  the value/node to insert
     * @param n    the current node
     * @return  the node inserted into/current node
     */
    def insert (key: K, ref: V, n: Node): Node =
    {
        println ("=" * 50)
        println ("insert: key = " + key)
        println ("=" * 50)
        var rt: Node = null
        
        if (n.isLeaf) {                                                      // handle leaf node level
            if (n.nKeys < ORDER - 1) {                                       // current node is not full
                wedge (key, ref, n, n.find (key), true)                      // wedge (key, ref) pair in at position i
            } else {                                                         // current node is full
                rt = split (key, ref, n, true)                               // split current node, return right sibling
                n.ref(n.nKeys) = rt                                          // link leaf n to leaf rt
                if (n == root && rt != null) {
                    root = makeRoot (n, n.key(n.nKeys-1), rt)                // make a new root
                } else if (rt != null) {
                    hasSplit = true                                          // indicate an unhandled split
                } // if
            } // if
        } else {                                                             // handle internal node level
            val i = n.find (key)                                             // find "<=" position
            rt = insert (key, ref, n.ref(i).asInstanceOf [Node])             // recursive call to insert, returning n's child
            if (DEBUG) println ("insert: handle internal node level")
            
            if (hasSplit) {                                                  // there's an unhandled split from below
                val ltc = n.ref(i).asInstanceOf [Node]                      // get the left child
                val dKey = ltc.key(ltc.nKeys - 1)                              // the divider key
                if (! ltc.isLeaf) ltc.nKeys -= 1                             // promote divider key from previous split                
                
                if (n.nKeys < ORDER - 1) {                                   // current node is not full                                                          
                    wedge (dKey, rt, n, i, false) 
                    hasSplit = false                                         // split from below has now been handled
                } else {                                                     // current node is full
                    if (DEBUG) println ("insert: internal node split")
                    rt = split (dKey, rt, n, false)                          // split current node, return right sibling                         
                    if (n == root && rt != null) {
                        root = makeRoot (n, n.key(n.nKeys-1), rt)            // make a new root
                        n.nKeys -= 1
                        hasSplit = false
                    }  // if               
                } // if
            } // if

        } // if

        if (DEBUG) print (root, 0)
        rt                                                                   // return right node
    } // insert

    /********************************************************************************
     * Make a new root, linking to left and right child node, seperated by a divider key.
     * @param ref0  the reference to the left child node
     * @param key0  the divider key - largest left
     * @param ref1  the reference to the right child node
     * @return  the node for the new root
     */
    private def makeRoot (ref0: Node, key0: K, ref1: Node): Node =
    {
        val nr = new Node (ORDER, false)                              // make a node to become the new root
        nr.nKeys  = 1                                                 
        nr.ref(0) = ref0                                              // reference to left node
        nr.key(0) = key0                                              // divider key - largest left
        nr.ref(1) = ref1                                              // referenece to right node
        nr 
    } // makeRoot

    /********************************************************************************
     * Wedge the key-ref pair into node n.  Shift right to make room if needed.
     * @param key   the key to insert
     * @param ref   the value/node to insert
     * @param n     the current node
     * @param i     the insertion position within node n
     * @param left  whether to start from the left side of the key
     * @return boolean indicate whether wedge succeded (i.e no duplicate)
     */
    private def wedge (key: K, ref: Any, n: Node, i: Int, left: Boolean): Boolean =
    {
        if (i < n.nKeys && key == n.key(i)) {
             println ("BpTreeMap2.insert: attempt to insert duplicate key = " + key)
             return false
        } // if
        n.ref(n.nKeys+1) = n.ref(n.nKeys)                            // preserving the last ref
        for (j <- n.nKeys until i by -1) {
            n.key(j) = n.key(j-1)                                    // make room: shift keys right
            if (left || j > i + 1) n.ref(j) = n.ref(j-1)             // make room: shift refs right
        } // for
        n.key(i) = key                                               // place new key
        if (left) n.ref(i) = ref; else n.ref(i+1) = ref              // place new ref
        n.nKeys += 1                                                 // increment number of keys
        true
    } // wedge

    /********************************************************************************
     * Split node n and return the newly created right sibling node rt.  The bigger half
     * should go in the current node n, with the remaining going in rt.
     * @param key  the new key to insert
     * @param ref  the new value/node to insert
     * @param n    the current node
     * @return  the right sibling node (may wish to provide more information)
     */
    private def split (key: K, ref: Any, n: Node, left: Boolean): Node =
    {
        bn.copy (n, 0, ORDER-1)                                           // copy n into big node                           
        if (wedge (key, ref, bn, bn.find (key), left)) {                  // if wedge (key, ref) into big node was successful
            n.copy (bn, 0, MID)                                           // copy back first half to node n
            val rt = new Node (ORDER, n.isLeaf)                           // make a right sibling node (rt)
            rt.copy (bn, MID, ORDER-MID)                                  // copy second to node rt    
            rt
        } // if     
        null                                                              // No new node created as key is duplicate
    } // split
    
} // BpTreeMap2 class

     
/********************************************************************************
 * The main method used for testing.
 */
object BpTreeMap2Test extends App
{
    val totalKeys = 200
    val RANDOMLY  = true

    val bpt = new BpTreeMap2 [Integer, Integer] ()
   
    if (RANDOMLY) {
        val rng = new Random ()
        for (i <- 1 to totalKeys by 2) bpt.put (rng.nextInt (2 * totalKeys), i * i)
    } else {
        for (i <- 1 to totalKeys by 2) bpt.put (i, i * i)
    } // if

    bpt.printTree () 
    for (i <- 0 to totalKeys) {
        println ("key = " + i + " value = " + bpt.get (i))
    } // for
    println ("-" * 50)
    println ("stats = " + bpt.stats)
    //bpt.traverseLeaves ()

} // BpTreeMap2Test object

