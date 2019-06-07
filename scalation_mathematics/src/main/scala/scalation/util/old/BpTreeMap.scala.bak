
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Oct  9 14:37:09 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import scala.collection.mutable.{AbstractMap, Map, MapLike}
import scala.reflect.ClassTag

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpTreeMap` class provides B+Tree maps.  B+Trees are used as multi-level
 *  index structures that provide efficient access for both point queries and range
 *  queries.
 *  @see docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html
 *  @see www.scala-lang.org/api/current/#scala.collection.mutable.MapLike
 *  @param order  the maximum fanout (number of references) for a B+Tree node
 */
class BpTreeMap [K <% Ordered [K]: ClassTag, V: ClassTag] (order: Int = 5)
      extends AbstractMap [K, V] with Map [K, V] with MapLike [K, V, BpTreeMap [K, V]] with Serializable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Node` abstract class defines a base type for B+Tree nodes.
     *  @param isLeaf  whether the node is a leaf (true) or internal node (false)
     */
    abstract class Node (val isLeaf: Boolean)
    {
        var nKeys = 0
        val key   = Array.ofDim [K] (order - 1)
        def isFull: Boolean = nKeys == order - 1
        def find (k: K): Int = { for (i <- 0 until nKeys if k <= key(i)) return i; nKeys }
        override def toString = s"[[ isLeaf = $isLeaf, nKeys = $nKeys\n   key = ${key.deep} ]]"

    } // Node inner class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `LNode` class defines leaf nodes that are stored in the B+tree map.
     */
    class LNode extends Node (true)
    {
        val ref = Array.ofDim [V] (order-1)
        var nxt: LNode = null

        def wedge (k: K, r: V, ip: Int)
        {
            for (j <- nKeys until ip by -1) { key(j) = key(j-1); ref(j) = ref(j-1) }
            key(ip) = k; ref(ip) = r
            nKeys  += 1
        } // wedge

    } // LNode inner class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `INode` class defines internal nodes that are stored in the B+tree map.
     */
    class INode extends Node (false)
    {
        val ref = Array.ofDim [Node] (order)

        def this (trip: LeftKeyRight)
        {
            this ()
            ref(0) = trip._1
            key(0) = trip._2
            ref(1) = trip._3
            nKeys  = 1
        } // aux constructor

        def wedge (k: K, r: Node, ip: Int)
        {
            for (j <- nKeys until ip by -1) { key(j) = key(j-1); ref(j+1) = ref(j) }
            key(ip) = k; ref(ip+1) = r
            nKeys  += 1
        } // wedge

    } // INode inner class


    /** Three tuple for holding left node, divider key and right node
     */
    type LeftKeyRight = Tuple3 [Node, K, Node]

    /** The floor of half the order
     */
    private val MID = order / 2

    /** The root of 'this' B+Tree
     */
    private var root: Node = new LNode

    /** The first leaf node in 'this' B+Tree map
     */
    private var firstLeaf: LNode = root.asInstanceOf [LNode]

    /** The counter for the number of nodes accessed (for performance testing)
     */
    private var _count = 0

    /** The counter for the number of keys in 'this' B+Tree map
     */
    private var keyCount = 0

    //  P U B L I C   M E T H O D S  -----------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the key-value pair 'kv' in 'this' B+Tree map, returning a reference to
     *  the updated tree.
     *  @param kv  the key-value pair to insert
     */
    def += (kv: (K, V)): BpTreeMap.this.type = { keyCount += 1;  insert (kv._1, kv._2, root); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the key-value pair with key 'key' from 'this' B+Tree map, returning
     *  a reference to the updated tree.
     *  @param key  the key to remove
     */
    def -= (key: K): BpTreeMap.this.type = throw new NoSuchMethodException ("-= method not supported")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two keys.
     *  @param key1  the first key
     *  @param key2  the second key
     */
    def compare (key1: K, key2: K): Int = key1 compare key2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the total number of nodes accessed.
     */
    def count: Int = _count

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an empty B+Tree map.
     */
    override def empty: BpTreeMap [K, V] = new BpTreeMap [K, V] (order)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply function 'f' to each key-value pair in 'this' B+Tree map.
     *  @param f  the function to be applied
     */
    override def foreach [U] (f: ((K, V)) => U) { for (kv <- iterator) f(kv) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Optionally return the value in 'this' B+Tree map associated with the 
     *  key 'key'.  Use 'apply' method to remove optional.
     *  @param key  the key used for look up
     */
    def get (key: K): Option [V] = Option (find (key, root))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an iterator over the key-value pairs in 'this' B+Tree map.
     *  The iteration proceeds over the leaf level of the tree.
     */
    def iterator: Iterator [(K, V)] =
    {
        var nn = firstLeaf
        var ii = 0
        new Iterator [(K, V)] {
            def hasNext: Boolean = nn != null
            def next: (K, V) =
            {
                val kv = (nn.key(ii), nn.ref(ii))
                if (ii < nn.nKeys - 1) ii += 1 else { ii = 0; nn = nn.nxt }
                kv
            } // next
        } // Iterator
    } // iterator

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of 'this' B+Tree map.
     */
    override def size: Int = keyCount

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the keys in 'this' B+Tree map.
     */
    def printTree () { println ("BpTreeMap"); printT (root, 0); println ("-" * 50) }

    //  P R I V A T E   M E T H O D S  -----------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' B+Tree map using a preorder traversal and indenting each level.
     *  @param n      the current node to print
     *  @param level  the current level in the B+Tree
     */
    private def printT (n: Node, level: Int)
    {
        println ("-" * 50)
        print ("\t" * level + "[ . ")
        for (i <- 0 until n.nKeys) print (n.key(i) + " . ")
        println ("]")
        if (! n.isLeaf) for (j <- 0 to n.nKeys) printT (n.asInstanceOf [INode].ref(j), level + 1)
    } // printT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for finding key 'key' in 'this' B+tree map.
     *  @param key  the key to find
     *  @param n    the current node
     */
    private def find (key: K, n: Node): V =
    {
        _count += 1
        val ip = n.find (key)
        if (n.isLeaf) {
            if (ip < n.nKeys && key == n.key(ip)) n.asInstanceOf [LNode].ref(ip)
            else null.asInstanceOf [V]
        } else {
            find (key, n.asInstanceOf [INode].ref(ip))
        } // if
    } // find

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for inserting 'key' and 'ref' into 'this' B+tree map.
     *  @param key  the key to insert
     *  @param ref  the value/node to insert
     *  @param n    the current node
     */
    private def insert (key: K, ref: V, n: Node): LeftKeyRight =
    {
        var trip: LeftKeyRight = null                // left node, divider key, right node

        if (n.isLeaf) {                              // handle leaf node

            if (n.isFull) {
                trip = splitL (key, ref, n.asInstanceOf [LNode])
                if (n == root) root = new INode (trip)
            } else {
                val ip = n.find (key)
                if (key == n.key(ip)) {
                    println ("BpTreeMap.insert: attempt to insert duplicate key = " + key)
                    keyCount -= 1
                } else {
                    n.asInstanceOf [LNode].wedge (key, ref, ip)
                } // if
            } // if

       } else {                                      // handle internal node

           trip = insert (key, ref, n.asInstanceOf [INode].ref(n.find (key)))
           if (trip != null) {
               if (n.isFull) {
                   trip = splitI (trip._2, trip._3, n.asInstanceOf [INode])
                   if (n == root) root = new INode (trip)
               } else {
                   n.asInstanceOf [INode].wedge (trip._2, trip._3, n.find (key)) 
                   trip = null
               } // if
           } // if

       } // if
       trip
    } // insert

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split leaf node 'n' and return it, the divider key and the new right node.
     *  @param key  the key to insert
     *  @param ref  the value to insert
     *  @param n    the current leaf node
     */
    private def splitL (key: K, ref: V, n: LNode): LeftKeyRight =
    {
        val rt = new LNode                                   // create new right sibling leaf node
        for (j <- 0 until MID) {
            rt.key(j) = n.key(MID+j)
            rt.ref(j) = n.ref(MID+j)
        } // for
        rt.nKeys = MID
        n.nKeys -= MID
        n.nxt    = rt                                        // set next reference to right node
        val divKey = n.key(n.nKeys-1)
        if (key < divKey) n.wedge (key, ref, n.find (key))
        else              rt.wedge (key, ref, rt.find (key))
        (n, divKey, rt)                                      // left node, divider key, right node
    } // splitL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split internal node 'n' and return it, the divider key and the new right node.
     *  @param key  the key to insert
     *  @param ref  the child node to insert
     *  @param n    the current internal node
     */
    private def splitI (key: K, ref: Node, n: INode): LeftKeyRight =
    {
        val rt = new INode                                   // create new right sibling internal node
        for (j <- 0 until MID - 1) {
            rt.key(j) = n.key(MID+j+1)
            rt.ref(j) = n.ref(MID+j+1)
        } // for
        rt.ref(MID-1) = n.ref(order-1)
        rt.nKeys = MID - 1
        n.nKeys -= MID
        val divKey = n.key(n.nKeys)
        if (key < divKey) n.wedge (key, ref, n.find (key))
        else              rt.wedge (key, ref, rt.find (key))
        (n, divKey, rt)                                      // left node, divider key, right node
    } // splitI

} // BpTreeMap class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpTreeMapTest` object is used to test the `BpTreeMap` class by inserting
 *  increasing key values.
 *  > run-main scalation.util.BpTreeMapTest
 */
object BpTreeMapTest extends App
{
    val tree = new BpTreeMap [Int, Int] ()

    val totKeys = 50
    for (i <- 1 until totKeys by 2) tree.put (i, i * i)
    tree.printTree ()
    for (i <- 0 until totKeys) println ("key = " + i + " value = " + tree.get(i))
    println ("-" * 50)
    for (it <- tree.iterator) println (it)
    println ("-" * 50)
    tree.foreach (println (_))
    println ("-" * 50)
    println ("size = " + tree.size)
    println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)

} // BpTreeMapTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpTreeMapTest2` object is used to test the `BpTreeMap` class by inserting
 *  random key values.
 *  > run-main scalation.util.BpTreeMapTest2
 */
object BpTreeMapTest2 extends App
{

    val tree = new BpTreeMap [Int, Int] ()

    val totKeys = 50
    val max     = 10 * totKeys

    // for unique random integers

//  import scalation.random.RandiU0         // comment out due to package dependency
//  val stream = 2
//  val rng    = RandiU0 (max, stream)
//  for (i <- 1 until totKeys) tree.put (rng.iigen (max), i * i)

    // for random integers
    import java.util.Random
    val seed = 1
    val rng  = new Random (seed)
    for (i <- 1 until totKeys) tree.put (rng.nextInt (max), i * i)

    tree.printTree ()
    println ("-" * 50)
    println ("size = " + tree.size)
    println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)

} // BpTreeMapTest2

