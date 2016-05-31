
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Phillip A McIntyre Jr.
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
 *
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
    private val MID = math.ceil(order / 2.0).toInt

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
    //def -= (key: K): BpTreeMap.this.type = throw new NoSuchMethodException ("-= method not supported")
    def -= (key: K): BpTreeMap.this.type = { keyCount -= 1; delete(key, root); this}

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
     *  key 'key'.  Use 'apply' method to remove optionality.
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
    def printTree { println ("BpTreeMap"); printT (root, 0); println ("-" * 50) }

    //  P R I V A T E   M E T H O D S  -----------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' B+Tree map using a pre-order traveral and indenting each level.
     *  @param n      the current node to print
     *  @param level  the current level in the B+Tree
     */
    private def printT (n: Node, level: Int)
    {
        if (n == null) return
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
    /** Recursive helper method for deleting key 'key' in 'this' B+tree map.
     *  @param key  the key to remove
     *  @param n    the current node
     */
    private def delete (key: K, n: Node): Node =
    {
        var maxKey = root.key(0)

        var currNode: Node = null
        var lowerNode: Node = null
        //var trip: LeftKeyRight = null

        //if the node is null, the key is not there
        if (n == null) {
            println("[delete, 241]: Key is not in the tree!")
            return null
        } // if

        var NodeKey = (currNode, maxKey)
        var nNodeKey = (n, maxKey)

        // if the node is not a leaf, navigate to a leaf in the right direction
        if (!n.isLeaf) {
            //DEBUG
            print("[delete, 252]: Node ")
            printNode(n)
            println(" is not a leaf.")

            currNode = delete(key, navToLeaf(key, n))

        } // if

//      var idx = findCurrentNode(n.asInstanceOf[INode], currNode)

        if (currNode != null) {
            //DEBUG
            print("[delete, 276]: Node ")
            printNode(currNode)
            println(" is not null.")

            //  Find the value and remove it from the node
            if (currNode.isLeaf) {
                //DEBUG
                print("[delete, 300]: currNode ")
                printNode(currNode)
                println(" is a leaf.")
                //find the key in the leaf node
                currNode = findInLeaf(key, currNode.asInstanceOf[LNode])

                println("currNode after findInLeaf = " + currNode)

                //if currNode != null, that means it was found
                if (currNode != null) {
                    //DEBUG
                    print("[delete, 306]: Node ")
                    printNode(currNode)
                    println(" is not null (2).")

                    //update the values and references in currNode and reassign values
                    println("currNode before updateL = " + currNode)
                    NodeKey = updateL(n, currNode, maxKey)
                    currNode = NodeKey._1
                    println("currNode after updateL = " + currNode)
                    maxKey = NodeKey._2
                    println("[delete, 289]: Updating parent key to " + maxKey.toString)
                } // if

            } // if

            println ("currNode = " + currNode)
            if (currNode.nKeys < 1) {
                var idx = findCurrentNode(n.asInstanceOf[INode], currNode)

                // if idx > -1, then the child we're looking for is n.ref(idx)

                if (idx > -1) {

                    println ("[delete, 330]: Deleting key " + n.asInstanceOf[INode].key(idx))

                    if (n.nKeys > 1) {
                        for (i <- idx until n.nKeys - 1) {
                            if (n.asInstanceOf[INode].ref(i) != null && (i + 1) <= n.nKeys - 1) {
                                n.key(i) = n.key(i + 1)
                                n.asInstanceOf[INode].ref(i) = n.asInstanceOf[INode].ref(i + 1)
                            } // if
                        } // for
                        n.asInstanceOf[INode].ref(n.nKeys - 1) = n.asInstanceOf[INode].ref(n.nKeys)
                        n.asInstanceOf[INode].ref(n.nKeys) = null
                        n.nKeys -= 1
                    } else {
                        n.key(0) = currNode.key(currNode.nKeys - 1)
                        n.asInstanceOf[INode].ref(0) = currNode
                        currNode.asInstanceOf[LNode].nxt = null
                        n.asInstanceOf[INode].ref(1) = null
                    } // if

                } // if
            } // if

            if (!currNode.isLeaf) {
                //DEBUG
                println("[delete, 360]: currNode is not a leaf")
                print("[delete, 361]: n is currently ")
                printNode(n)
                println()

                print("[delete, 362]: currNode is currently ")
                printNode(currNode)
                println()

                NodeKey = updateI(n, currNode, maxKey)
                currNode = NodeKey._1
                maxKey = NodeKey._2

                println("[delete, 387]: Updating parent key to " + maxKey.toString)
                return currNode
            } // if

            //root.key(0) = maxKey
        } // if

        if (currNode == null) {
            println("[delete, 394]: Recursion end")
            return n
        } // if
        //        currNode
        n
    } // delete

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Navigate to leaf node.
     */
    private def navToLeaf(key: K, n: Node): Node =
    {
        var currNode = n
        var break = false
        var i = 0
        if(!n.isLeaf) {
            while (i < n.nKeys && !break) {
                if (key <= n.asInstanceOf[INode].key(i)) {
                    println("[navToLeaf]: Move left at i = " + i.toString)
//                    currNode = navToLeaf(key, currNode.asInstanceOf[INode].ref(i))
                    currNode = n.asInstanceOf[INode].ref(i)
                    break = true
                } // if
                i += 1
            } // while
            if (!break) {
                println("[navToLeaf]: Move right at i = " + (n.nKeys - 1).toString)
//                currNode = navToLeaf(key, currNode.asInstanceOf[INode].ref(n.nKeys - 1));
                currNode = n.asInstanceOf[INode].ref(n.nKeys - 1)
            } // if
        } // if
        currNode
    } //navToLeaf()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find key in leaf.
     */
    private def findInLeaf(key: K, n: LNode): LNode =
    {
        var currNode = n

        val ip = currNode.find (key)
        println ("[findInLeaf]: Index (ip): " + ip.toString)

        //if ip == 0, it is not in the tree

        if (ip < 0 || ip >= currNode.nKeys) {
            println("[findInLeaf]: Key is not in the tree!")
            return null
        } else {                                          //otherwise, the value is at n.key(ip)
            println ("[findInLeaf]: We found a match!")
            printNode (currNode)

            println("[findInLeaf]: Removing key: " + key.toString)

//          currNode = deleteL(ip, key, currNode)
            deleteL(ip, key, currNode)
        } // else

        currNode
    } // findInLeaf()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update leaf node.
     */
    private def updateL (n: Node, currNode: Node, maxKey: K): (Node, K) =
    {
        println("[updateL]: Unravelling recursion")
        var idx = findCurrentNode(n.asInstanceOf[INode], currNode)
        if(idx >= 0) {
            println("[updateL]: childNode at node: " + idx.toString)
            //in case a root has immediate leaf descendants
            if(n == root){
                if (n.key(idx) != maxKey) {
                    n.key(idx) = maxKey
                } // if
            } else {                      //update parent key by checking if it's equal to it's rightmost child
                if (n.key(idx) != currNode.key(currNode.nKeys - 1))
                    n.key(idx) = currNode.key(currNode.nKeys - 1)

                var nextNode = n.asInstanceOf[INode].ref(idx + 1)

                //also check and update the parent's immediate neighbor
                if (n.key(idx + 1) != nextNode.key(nextNode.nKeys - 1))
                    n.key(idx + 1) = nextNode.key(nextNode.nKeys - 1)
            } // else
        } // if

        (currNode, maxKey)
    } // updateL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update internal node.
     */
    private def updateI (n: Node, currNode: Node, maxK: K): (Node, K) =
    {
        println("[updateI, 461]: Unrolling recursion....")
        var maxKey = currNode.key(currNode.nKeys - 1)
//      println("Unrolling recursion.... isLeaf")
//      if(currNode.isLeaf) currNode = deleteI(ip, key, n)

        var idx = findCurrentNode(n.asInstanceOf[INode], currNode)
        if (currNode.nKeys > 1) {
//          var idx = findCurrentNode(n.asInstanceOf[INode], currNode)
//          var idx = n.find(currNode.key(currNode.nKeys - 1))
            if (idx >= 0) {
                if (n == root) {
                    if (n.key(idx) != maxKey) n.key(idx) = maxKey
                } else {
                    if (n.key(idx) != currNode.key(currNode.nKeys - 1))
                        n.key(idx) = currNode.key(currNode.nKeys - 1)

                    var nextNode = n.asInstanceOf[INode].ref(idx + 1)

                    if (n.key(idx + 1) != nextNode.key(nextNode.nKeys - 1))
                        n.key(idx + 1) = nextNode.key(nextNode.nKeys - 1)
                } // if
            } // if
        } // if

        (currNode, maxKey)
    } // updateI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find current node.
     */
    private def findCurrentNode (n: INode, currNode: Node): Int =
    {
        var idx = -1
        var break = false
        var i = 0
        while (i < n.nKeys && !break) {
            if (n.asInstanceOf[INode].ref(i) == currNode) {
                idx = i
                break = true
            } // if
            i += 1
        } // while
        idx
    } // findCurrentNode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Delete internal node.
     */
    private def deleteI (ip: Int, key: K, n: INode): INode =
    {
        n                                                           // FIX: to be implemented
    } // deleteI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Delete leaf node.
     */
    private def deleteL (ip: Int, key: K, n: LNode): LNode =
    {
        if(n == null) {
            println ("[deleteL]: currNode = null")
            return null
        } // if

        // overwrite the value by moving over the elements after it
        for (i <- ip until n.nKeys - 1) {
            n.key(i) = n.key(i + 1)
            n.ref(i) = n.ref(i + 1)
        } // for

        n.nKeys -= 1    //reduce the number of keys in the node by 1

        //if node is still half full, return
        if (n.nKeys >= MID)
        {
            println("[deleteL]: Node is still half-full.")
            return n
        } else {                            //otherwise, get values from an adjacent neighbor
            if (n.nxt == null) {
                println("[deleteL, 524]: currNode = null")
                return null
            } // if

            var adj: LNode = n.nxt

            //if the adj has more than half, borrow the 1st (key, ref) pair
            if (adj.nKeys > MID) {
                println("[deleteL]: Node will borrow from right-neighbor")
                borrowL(n, adj)
            } else {                            //if it has less than half, merge the (key, ref) pairs of both
                println("[deleteL]: Nodes must merge")
                mergeL(n, adj)
            } // if
        } // if
        n
    } // deleteL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Merge leaf nodes.
     */
    private def mergeL (n: LNode, adj: LNode)
    {
        val n_keys = n.nKeys    //temporarily hold n.nKeys in a variable
        n.nKeys += adj.nKeys    //add the amount of keys from adj node to original node

        //add the adj node's keys and refs to the end of n
        for (k <- n_keys until n.nKeys) {
            n.key(k) = adj.key(k - n_keys)
            n.ref(k) = adj.ref(k - n_keys)
        } // for
        //remove adjacent node after absorbing its elements
        n.nxt = adj.nxt
        adj.nxt = null
    } // mergeL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Borrow at the leaf level.
     */
    private def borrowL (n: LNode, adj: LNode)
    {
        // if the adjacent node has more than half, borrow the 1st (key, value) pair from them.
        n.nKeys += 1
        n.key(n.nKeys - 1) = adj.key(0)
        n.ref(n.nKeys - 1) = adj.ref(0)

        // decrement adjacent node's number of keys by one since we move it
        adj.nKeys -= 1

        // adjust the right node's (key, value)
        for (j <- 0 until adj.nKeys) {
            adj.key(j) = adj.key(j + 1)
            adj.ref(j) = adj.ref(j + 1)
        } // for
    } // borrowL

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
            val ip = n.find(key)

            if (ip < n.nKeys && key == n.key(ip)) {
                println("BpTreeMap.insert: attempt to insert duplicate key = " + key)
                keyCount -= 1
            } else {
                if (n.isFull) {
                    trip = splitL(key, ref, n.asInstanceOf[LNode], ip)
                    if (n == root) root = new INode(trip)
                } else {
                    n.asInstanceOf[LNode].wedge(key, ref, ip)
                } //else
            } // else
        }
        else {                                      // handle internal node
            trip = insert (key, ref, n.asInstanceOf [INode].ref(n.find (key)))
            if (trip != null) {
                if (n.isFull) {
//                    val ip = n.find(key)
                    trip = splitI (trip._2, trip._3, n.asInstanceOf [INode], n.find(key))
//                    trip = splitI (trip._2, trip._3, n.asInstanceOf [INode])
                    if (n == root) root = new INode (trip)
                } else {
                    n.asInstanceOf [INode].wedge (trip._2, trip._3, n.find (key))
                    trip = null
                } // if
            } // if
        } // else
        trip
    } // insert

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split leaf node 'n' and return it, the divider key and the new right node.
     *  @param key  the key to insert
     *  @param ref  the value to insert
     *  @param n    the current leaf node
     *  @param pos  the position of the new key in the node
     */
    private def splitL (key: K, ref: V, n: LNode, pos: Int): LeftKeyRight =
    {
        var notAssigned = true
        val rt = new LNode()

        for (i <- MID until order) {                      // fill in right node
            if (i < pos) {
                rt.key(i-MID) = n.key(i)
                rt.ref(i-MID) = n.ref(i)
            } // if
            else if (i == pos) {
                notAssigned = false
                rt.key(i-MID) = key
                rt.ref(i-MID) = ref
            } // if
            else {
                rt.key(i-MID) = n.key(i-1)
                rt.ref(i-MID) = n.ref(i-1)
            } // else
        } // for

        for (i <- MID-1 until pos by -1){
            n.key(i) = n.key(i-1)
            n.ref(i) = n.ref(i-1)
        } // for

        if (notAssigned) {
            n.key(pos) = key
            n.ref(pos) = ref
        } // if
        n.nKeys = MID
        rt.nKeys = order - MID

        val divKey = n.key(n.nKeys - 1)
        (n, divKey, rt)                                   // return LeftKeyRight
    } // splitL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split internal node 'n' and return it, the divider key and the new right node.
     *  @param key  the key to insert
     *  @param ref  the child node to insert
     *  @param n    the current internal node
     *  @param pos  the position of the new key in the node
     */
    private def splitI (key: K, ref: Node, n: INode, pos: Int): LeftKeyRight =
    {
        var notAssigned = true
        val rt = new INode()

        for (i <- MID until order) {                      // fill in right node
            if (i < pos) {
                rt.key(i-MID) = n.key(i)
                rt.ref(i-MID) = n.ref(i)
            } // if
            else if (i == pos) {
                notAssigned = false
                rt.key(i-MID) = key
                rt.ref(i-MID) = n.ref(i)
                rt.ref((i+1)-MID) = ref
            } // if
            else {
                if(notAssigned) {
                    rt.key(i-MID) = n.key(i-1)
                    rt.ref(i-MID) = n.ref(i-1)
                } // if
                else {
                    rt.key(i-MID) = n.key(i-1)
                    rt.ref((i+1)-MID) = n.ref(i-1)
                } // else
            } // else
        } // for

        if(pos != n.nKeys) { rt.ref(order - MID) = n.ref(n.nKeys) }

        for (i <- MID-1 until pos by -1) {
            n.key(i) = n.key(i-1)
            n.ref(i) = n.ref(i-1)
        } // for

        if (notAssigned){
            n.key(pos) = key

            if(pos == MID - 1) { rt.ref(0) = ref }
            else {
                n.ref(pos) = n.ref(pos+1)
                n.ref(pos+1) = ref
            } // else
        } // if

        n.nKeys = MID
        rt.nKeys = order - MID

        val divKey = n.key(n.nKeys - 1)
        n.nKeys -= 1
        (n, divKey, rt)                                   // return LeftKeyRight
    } // splitI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show debugging information about an internal node.
     *  @param n    the internal node
     *  @param key  the key being inserted
     *  @param ref  the child node/ref being added
     *  @param pos  the insertion position
     */
    private def debug (n: INode, key: K, ref: Node, pos: Int)
    {
        println ()
        println ("*"*5)
        println ("DEBUG")
        println ("*"*5)
        println ("MID: " + MID)
        print ("n: "); printNode (n)
        println ("n.nKeys: " + n.nKeys)
        print ("key: " + key + ", ref: ")
        printNode (ref)
        println ("pos: " + pos)
        println()
        for (i <- 0 to order) {
            if (i < pos+1)       printNode (n.ref(i))
            else if (i == pos+1) printNode (ref)
            else                 printNode (n.ref(i-1))
        } // for
        println ("*"*5)
        println ()
    } // debug

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print node 'n'.
     *  @param n  the node to print
     */
    private def printNode (n: Node)
    {
        print ("[")
        print (n.key(0))
        for (i <- 1 until n.nKeys) print (" " + n.key(i))
        println ("]")
    } // printNode

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
    tree.printTree
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

    val totKeys = 100
    val max     = 10 * totKeys

//  // for unique random integers
//
//  import scalation.random.RandiU0         // comment out due to package dependency
//  val stream = 2
//  val rng    = RandiU0 (max, stream)
//  for (i <- 1 until totKeys) tree.put (rng.iigen (max), i * i)

    // for random integers

    import java.util.Random
    val seed = 1
    val rng  = new Random (seed)
    for (i <- 1 until totKeys) {
        var randInt = rng.nextInt(max);
        var value = i * i
//      tree.put (rng.nextInt (max), i * i)
        println("Inserting (K, V): " + "(" + randInt.toString + ", " + value.toString + ")")
        tree.put(randInt, value)
        tree.printTree
        println()
        println ("=" * 50)
        println()
    } // for

    tree.printTree
    println ("-" * 50)
    println ("size = " + tree.size)
    println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)

} // BpTreeMapTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpTreeMapTest3` object is used to test the `BpTreeMap` class by deleting
 *  key values.  Implementation of delete is incomplete and still has bugs.
 *  > run-main scalation.util.BpTreeMapTest3
 */
object BpTreeMapTest3 extends App
{
    val tree = new BpTreeMap [Int, Int] ()

    val totKeys = 50
    val max     = 10 * totKeys

//  val itemToRemove1 = 69
//  val itemToRemove2 = 62
//  val itemToRemove3 = 92
//  val itemToRemove4 = 0

    val itemToRemove1 = 347
    val itemToRemove2 = 334
//  val itemToRemove3 = 92
//  val itemToRemove4 = 0

//  // for unique random integers
//
//  import scalation.random.RandiU0         // comment out due to package dependency
//  val stream = 2
//  val rng    = RandiU0 (max, stream)
//  for (i <- 1 until totKeys) tree.put (rng.iigen (max), i * i)

    // for random integers

    import java.util.Random
    val seed = 1
    val rng  = new Random (seed)
    for (i <- 1 until totKeys) tree.put (rng.nextInt (max), i * i)

    //remove 1st value

    println("Looking for key: " + itemToRemove1.toString)
    tree.printTree
    println ("-" * 50)
    println ("size = " + tree.size)
    //println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)
    println ("-" * 50)
    println("Item to Remove: " + itemToRemove1.toString)
    tree.remove(itemToRemove1)
    println ("-" * 50)
    tree.printTree

    // remove 2nd value

    println("Looking for key: " + itemToRemove2.toString)
    println ("-" * 50)
    println ("size = " + tree.size)
    //println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)
    println ("-" * 50)
    println("Item to Remove: " + itemToRemove2.toString)
    tree.remove(itemToRemove2)
    tree.printTree

    // remove 3rd value

//  println("Looking for key: " + itemToRemove3.toString)
//  println ("-" * 50)
//  println ("size = " + tree.size)
//  println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)
//  println ("-" * 50)
//  println("Item to Remove: " + itemToRemove3.toString)
//  tree.remove(itemToRemove3)
//  tree.printTree
//
//  // remove 4th value
//
//  println("Looking for key: " + itemToRemove4.toString)
//  println ("-" * 50)
//  println ("size = " + tree.size)
//  println ("Average number of nodes accessed = " + tree.count / totKeys.toDouble)
//  println ("-" * 50)
//  println("Item to Remove: " + itemToRemove4.toString)
//  tree.remove(itemToRemove4)
//  tree.printTree

} // BpTreeMapTest3

