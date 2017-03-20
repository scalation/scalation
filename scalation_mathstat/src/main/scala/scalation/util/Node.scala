
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Thu Jun 23 14:29:06 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import scala.reflect.ClassTag

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Node` class defines a splittable node type with methods for finding
 *  entries (keys and values).
 *  @param isLeaf  whether the node is a leaf (true) or internal node (false)
 *  @param half    half the maximum number of keys allowed in a ndoe
 */
class Node [K <% Ordered [K]: ClassTag] (val isLeaf: Boolean = true, val half: Int = 2)
      extends Error
{
    /** The number of active keys
     */
    var nKeys = 0

    /** The maximum number of keys
     */
    val mx = 2 * half

    /** The array of keys
     */
    val key = Array.ofDim [K] (mx)

    /** The array of references
     */
    val ref = Array.ofDim [Any] (mx+1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a new root node with one key (and two references) in it.
     *  @param lt  the left node
     *  @param kd  the divider key
     *  @param rt  the right node
     */
    def this (lt: Node [K], kd: K, rt: Node [K], half: Int)
    {
        this (false, half)
        ref(0) = lt
        key(0) = kd
        ref(1) = rt
        nKeys  = 1
    } // aux. constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' mode is currently full.
     */
    def isFull: Boolean = nKeys == mx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the "<=" position of key 'k' in 'this' node.  If 'k' is larger
     *  than all keys in 'this' node, return 'nkeys'.
     *  @param k  the key whose position is sought
     */
    def find (k: K): Int =
    {
        for (i <- 0 until nKeys if k <= key(i)) return i
        nKeys
    } // find

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** When space is available, wedge the new key 'k' and value 'v' into
     *  'this' node at the given insertion position 'ip'.
     *  @param k     the new key
     *  @param v     the new value
     *  @param ip    the insertion position
     *  @param left  whether to start with from the left size of the key
     */
    def wedge (k: K, v: Any, ip: Int, left: Boolean = true)
    {
        if (nKeys > mx) {
            flaw ("wedge", "node is already full")
        } else {
            for (j <- nKeys until ip by -1) {
                key(j) = key(j-1)
                if (left || j > ip + 1) ref(j) = ref(j-1)
            } // for
            key(ip) = k; if (left) ref(ip) = v else ref(ip+1) = v
            nKeys += 1
        } // if
    } // wedge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split 'this' node by creating a right sibling 'rt' and moving half
     *  the keys and references to that new node.  Return the divider key
     *  and the right sibling node.
     */
    def split (): (K, Node [K]) =
    {
        val rt = new Node [K] (isLeaf, half)
        for (i <- 0 until half) {
            rt.key(i) = key(i + half)
            rt.ref(i) = ref(i + half)
        } // for
        rt.ref(half) = ref(mx)
        if (isLeaf) ref(mx) = rt
        rt.nKeys = half
        nKeys    = half
        (key(half-1), rt)
    } // split

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' node to a string.
     */
    override def toString =
    {
        s"[[ isLeaf = $isLeaf, nKeys = $nKeys\n   key = ${key.slice (0, nKeys).deep}\n   ref = ${ref.slice (0, nKeys+1).deep} ]]"
    } // toString

} // Node class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NodeTest` object is used to test the `Node` class.
 *  > run-main scalation.util.NodeTest
 */
object NodeTest extends App
{
     val keys = 10
     val n = new Node [Int] ()
     for (i <- 1 until keys) {
         n.wedge (i, i * i, n.find (i))
         println (s"n = $n")
     } // for

} // NodeTest object

