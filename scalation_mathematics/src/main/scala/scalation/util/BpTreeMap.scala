
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Yang Fan
 *  @version 1.6
 *  @date    Fri Apr 20 22:55:45 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import java.io.{FileWriter, PrintWriter}

import scala.collection.mutable.{AbstractMap, Map, MapLike}
import scala.reflect.ClassTag

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpTreeMap` class provides B+Tree maps.  B+Trees are used as multi-level
 *  index structures that provide efficient access for both point queries and range
 *  queries.
 *  @see docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html
 *  @see www.scala-lang.org/api/current/#scala.collection.mutable.MapLike
 *------------------------------------------------------------------------------
 *  @param half  the number of keys per node range from 'half' to '2 * half'
 */
class BpTreeMap [K <% Ordered [K]: ClassTag, V: ClassTag] (half: Int = 2)
      extends AbstractMap [K, V] with Map [K, V] with MapLike [K, V, BpTreeMap [K, V]] with Serializable
{
    /** Provides access to a file for outputing large B+Trees
     */
    private var writer: PrintWriter = null

    /** The debug flag for tracking node splits
     */
    private val DEBUG = false

    /** The maximum number of keys per node
     */
    private val mx = 2 * half

    /** The root of 'this' B+Tree
     */
    private var root = new Node [K] (true, half)

    /** The first leaf node in 'this' B+Tree map
     */
    private val firstLeaf = root

    /** The counter for the number of nodes accessed (for performance testing)
     */
    private var _count = 0

    /** The counter for the number of keys in 'this' B+Tree map
     */
    private var keyCount = 0

    //  P U B L I C   M E T H O D S  -------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the key-value pair 'kv' in 'this' B+Tree map, returning a reference to
     *  the updated tree.
     *  @param kv  the key-value pair to insert
     */
    def += (kv: (K, V)): BpTreeMap.this.type = { keyCount += 1; insert (kv._1, kv._2, root); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the key-value pair with key 'key' from 'this' B+Tree map, returning
     *  a reference to the updated tree.
     *  @param key  the key to remove
     */
    def -= (key: K): BpTreeMap.this.type = throw new NoSuchMethodException ("-= method not yet supported")

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
    override def empty: BpTreeMap [K, V] = new BpTreeMap [K, V] (half)

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
                val kv = (nn.key(ii), nn.ref(ii).asInstanceOf [V])
                if (ii < nn.nKeys - 1) ii += 1
                else {ii = 0; nn = nn.ref(nn.nKeys).asInstanceOf [Node [K]]}
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the keys in 'this' B+Tree map to file.
     *  @param file  the file to be used for output
     */
    def printTreetoFile (file: String)
    {
        writer = new PrintWriter (new FileWriter (file, true))
        writer.println ("BpTreeMap");
        printTtoFile (root, 0);
        writer.println ("-" * 50)
        writer.close ()
    } // printTreetoFile

    //  P R I V A T E   M E T H O D S  -----------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' B+Tree map using a preorder traversal and indenting each level.
     *  @param n      the current node to print
     *  @param level  the current level in the B+Tree
     */
    private def printT (n: Node [K], level: Int)
    {
        println ("-" * 50)
        print ("\t" * level + "[ . ")
        for (i <- 0 until n.nKeys) print (n.key(i) + " . ")
        println ("]")
        if (! n.isLeaf) for (j <- 0 to n.nKeys) printT (n.asInstanceOf [Node [K]].ref(j).asInstanceOf [Node [K]], level + 1)
    } // printT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' B+Tree map using a preorder traversal and indenting each level.
     *  @param n      the current node to print
     *  @param level  the current level in the B+Tree
     */
    private def printTtoFile (n: Node [K], level: Int)
    {
        writer.println ("-" * 50)
        writer.print ("\t" * level + "[ . ")
        for (i <- 0 until n.nKeys) writer.print (n.key(i) + " . ")
        writer.println ("]")

//      for (i <- 0 to n.nKeys)  print ("i"+i +" "+ n.ref(i) +" ")
//      println ("]")
//      if (DEBUG) { println (n.ref(n.nKeys)); println ("]") }

        if (! n.isLeaf) {
           for (j <- 0 to n.nKeys) printTtoFile (n.asInstanceOf [Node [K]].ref(j).asInstanceOf [Node [K]], level + 1)
        } // if
        writer.flush ()
    } // printT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for finding key 'key' in 'this' B+tree map.
     *  @param key  the key to find
     *  @param n    the current node
     */
    private def find (key: K, n: Node [K]): V =
    {
        _count += 1
        val ip = n.find (key)
        if (n.isLeaf) {
            if (ip < n.nKeys && key == n.key(ip)) n.ref(ip).asInstanceOf [V]
            else null.asInstanceOf [V]
        } else {
            find (key, n.ref(ip).asInstanceOf [Node [K]])
        } // if
    } // find

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for inserting 'key' and 'ref' into 'this' B+tree map.
     *  @param key  the key to insert
     *  @param ref  the value/node to insert
     *  @param n    the current node
     */
    private def insert (key: K, ref: V, n: Node [K]): (K, Node [K]) =
    {
        var kd_rt: (K, Node [K]) = null
        if (n.isLeaf) {                                                 // handle leaf node
            kd_rt = add (n, key, ref)
            if (kd_rt != null) {
                if (n != root) return kd_rt
                root = new Node (root, kd_rt._1, kd_rt._2, half)
            } // if
        } else {                                                        // handle internal node
           kd_rt = insert (key, ref, n.ref(n.find (key)).asInstanceOf [Node [K]])
           if (kd_rt != null) {
               kd_rt = addI (n, kd_rt._1, kd_rt._2)
               if (kd_rt != null && n == root) root = new Node (root, kd_rt._1, kd_rt._2, half)
           } // if
       } // if
       kd_rt
    } // insert

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a new key 'k' and value 'v' into leaf node 'n'.  If it is already full,
     *  a 'split' will be triggered, in which case the divider key and new
     *  right sibling node are returned.
     *  @param n  the current node
     *  @param k  the new key
     *  @param v  the new left value
     */
    private def add (n: Node [K], k: K, v: Any): (K, Node [K]) =
    {
        var kd_rt: (K, Node [K]) = null                                 // divider key, right sibling
        var split = false
        if (n.isFull) {
            split = true
            if (DEBUG) println ("before leaf split: n = " + n)
            kd_rt = n.split ()                                          // split n -> r & rt
            if (DEBUG) println ("after leaf split: n = " + n + "\nkd_rt = " + kd_rt)
            if (k > n.key(n.nKeys - 1)) {
                kd_rt._2.wedge (k, v, kd_rt._2.find (k), true)          // wedge into right sibling
                return kd_rt
            } // if
        } // if
        n.wedge (k, v, n.find (k), true)                        // wedge into current node
        if (split) kd_rt else null
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a new key 'k' and value 'v' into internal node 'n'.  If it is already
     *  full,  a 'split' will be triggered, in which case the divider key and new
     *  right sibling node are returned.
     *  @param n  the current node
     *  @param k  the new key
     *  @param v  the new left value
     */
    private def addI (n: Node [K], k: K, v: Any): (K, Node [K]) =
    {
        var kd_rt: (K, Node [K]) = null                                 // divider key, right sibling
        var split = false
        if (n.isFull) {
            split = true
            if (DEBUG) println ("before internal split: n = " + n)
            kd_rt = n.split ()
            // split n -> n & rt
            val promotedvalue = n.key(n.nKeys-1)
            n.nKeys -= 1                                                // remove promoted largest left key
            if (DEBUG) println ("after internal split: n = " + n + "\nkd_rt = " + kd_rt)
            if (k > promotedvalue){
                kd_rt._2.wedge (k, v, kd_rt._2.find (k), false)         // wedge into right sibling
                return kd_rt
            } // if
        } // if
        n.wedge (k, v, n.find (k), false)                               // wedge into current node
        if (split) kd_rt else null
    } // addI

} // BpTreeMap class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpTreeMapTest` object is used to test the `BpTreeMap` class by inserting
 *  increasing key values.
 *  > run-main scalation.util.BpTreeMapTest
 */
object BpTreeMapTest extends App
{
    val tree = new BpTreeMap [Int, Int] ()

    val totKeys = 26
    for (i <- 1 until totKeys by 2) {
        tree.put (i, i * i)
        tree.printTree ()
        println ("=" * 50)
    } // for
    for (i <- 0 until totKeys) println ("key = " + i + " value = " + tree.get(i))
    println ("-" * 50)
    for (it <- tree.iterator) println (it)
    println ("-" * 50)
    tree.foreach (println (_))
    println ("-" * 50)
    println ("size = " + tree.size)
    println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)

} // BpTreeMapTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpTreeMapTest2` object is used to test the `BpTreeMap` class by inserting
 *  random key values.
 *  > run-main scalation.util.BpTreeMapTest2
 */
object BpTreeMapTest2 extends App
{

    val tree = new BpTreeMap [Int, Int] ()

    val totKeys = 50
    val mx      = 10 * totKeys

    // for unique random integers

//  import scalation.random.RandiU0                     // comment out due to package dependency
//  val stream = 2
//  val rng    = RandiU0 (mx, stream)
//  for (i <- 1 until totKeys) tree.put (rng.iigen (mx), i * i)

    // for random integers
    import java.util.Random
    val seed = 1
    val rng  = new Random (seed)
    for (i <- 1 until totKeys) tree.put (rng.nextInt (mx), i * i)

    tree.printTree ()
    println ("-" * 50)
    println ("size = " + tree.size)
    println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)

} // BpTreeMapTest2 object

