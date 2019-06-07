

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Luis A. Ibarra
 *  @version 1.6
 *  @date    Mon May 1 11:28:31 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  Dual simulation cardinality Using Immutable Sets
 */

package scalation.graph_db.pattern_matching

import java.util

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Set => SET}
import scala.reflect.ClassTag

import scalation.graph_db.Graph

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2` class provides a second implementation for Dual Graph Simulation.
 *  It differs from `DualSim` by not using inverse adjacency sets ('pa') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim2CAR [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
      extends GraphMatcher [TLabel] (g, q)
{
    private val DEBUG      = false                             // debug flag
    private val labelCAR   = q.label.distinct                  // build unique query labels set
    private val mapGraph   = buildMap (g.ch, "g")              //  map: (vertex id, labelCAR value) ->
                                                               //      (child list, parent list)
    private val mapQuery   = buildMap (q.ch, "q")              // same for query graph
    private val countSetsG = buildCountSets (g.ch, "g")        // count sets for data graph
    private val countSetsQ = buildCountSets (q.ch, "q")        // count sets for query graph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a graph pattern matching algorithm to find the mappings from the
     *  query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    override def mappings (): Array [SET [Int]] = prune (feasibleMatesWithSets ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the label matches using the count sets.
     */
    def feasibleMatesWithSets (): Array [SET [Int]] =
    {
        val phi = super.feasibleMates ()
        for (u <- qRange; v <- phi(u)) {
            val u_s = countSetsQ.get (u)
            val v_s = countSetsG.get (v)
            if (! includedInCountSets (u_s, v_s)) phi(u) -= v
        } // for
        phi
    } // feasibleMatesWithSets

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMateswithSets' method,
     *  eliminate mappings 'u -> v' when (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {
            alter = false

            for (u <- qRange; v <- phi(u)) {
                if (! includedInCh (u, v, phi)) {
                    phi(u) -= v
                    if (phi(u).isEmpty) return phi
                    alter = true
                } // if
            } // for

            for (u <- qRange; v <- phi(u)) {
                if (! includedInPa (u, v, phi)) {
                    phi(u) -= v
                    if (phi(u).isEmpty) return phi
                    alter = true
                } // if
            } // for

       } // while
       phi
    } // prune

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the map with keys equals to vertex id and query label
     *  and values equals to children list and parent list.
     *  @param ch    the array of child (adjacency) vertex sets (outgoing edges)
     *  @param name  name of the graph
     */
    private def buildMap (ch: Array [SET [Int]], name: String): util.HashMap [(Int, TLabel), (SET[Int], SET[Int])] =
    {
        var map = new util.HashMap [(Int, TLabel), (SET [Int], SET [Int])] ()
        for (i <- ch.indices; l <- labelCAR.indices) {
            val ch_list = listOfLabelsCh (i, l, ch, name)
            val pa_list = listOfLabelsPa (i, l, ch, name)
            map.put ((i, labelCAR(l)), (ch_list, pa_list))
        } // for
        map
    } // buildMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the children countsets and parent counsets
     *  @param ch    the array of child (adjacency) vertex sets (outgoing edges)
     *  @param name  name of the graph
     */
    private def buildCountSets (ch: Array [SET [Int]], name: String): util.HashMap [Int, Array [(Int, Int)]] =
    {
        var map = new util.HashMap [Int, Array [(Int, Int)]] ()
        for (i <- ch.indices) {
            val abs = ArrayBuffer [(Int, Int)] ()
            for (l <- labelCAR.indices) {
                val numCh = countLabelsCh (i, l, ch, name)       // number of children of 'i' with label 'l'
                val numPa = countLabelsPa (i, l, ch, name)       // number of parents of 'i' with label 'l'
                abs      += ((numCh, numPa))
            } // for
            map.put (i, abs.toArray)
        } // for
        map
    } // buildCountSets

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if children/parent count sets satisfied dual sim cardinality restriction.
     *  @param chu  children/parent count sets for u
     *  @param chv  children/parent count sets for v
     */
    private def includedInCountSets (chu: Array [(Int,Int)], chv: Array [(Int,Int)]): Boolean =
    {
        for (i <- chu.indices) {
            if (chu(i)._1 > chv(i)._1) return false        // check children count sets
            if (chu(i)._2 > chv(i)._2) return false        // check parent count sets
        } // for
        true
    } // includedInCountSets

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the children of vertex 'i' whose label matches 'labCAR'.
     *  @param i       the index of the vertex under consideration
     *  @param labCAR  the unique labelCAR to match
     *  @param name    the name of graph
     */
    private def listOfLabelsCh (i: Int, labelIndex: Int, ch: Array [SET [Int]], name: String): SET [Int] =
    {
        var abi = SET [Int] ()
        if (name contains "g") {
           for (ch <- g.ch(i)) if (g.label(ch) == labelCAR(labelIndex)) abi += ch
        } else {
           for (ch <- q.ch(i)) if (q.label(ch) == labelCAR(labelIndex)) abi += ch
        } // if
        abi
    } // listOfLabelsCh

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parents of vertex 'i' whose label matches 'labCAR'.
     *  @param i       the index of the vertex under consideration
     *  @param labCAR  the unique labelCAR to match
     *  @param name    the name of graph
     */
    private def listOfLabelsPa (i: Int, labelIndex: Int, ch: Array [SET [Int]], name: String): SET [Int] =
    {
        var abi = SET [Int] ()
        if (name contains "g") {
           for (pa <- g.pa(i)) if (g.label(pa) == labelCAR(labelIndex)) abi += pa
        } else {
           for (pa <- q.pa(i)) if (q.label(pa) == labelCAR(labelIndex)) abi += pa
        } // if
        abi
    } // listOfLabelsPa

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of children of vertex 'i' whose label matches 'labCAR'.
     *  @param i         the index of the vertex under consideration
     *  @param labIndex  the label index to match
     *  @param ch        the children
     *  @param name      the name of graph
     */
    private def countLabelsCh (i: Int, labIndex: Int, ch: Array [SET [Int]], name: String): Int =
    {
        var n = 0
        if (name contains "g") {
            for (ch <- g.ch(i)) if (g.label(ch) == labelCAR(labIndex)) n += 1
        } else {
            for (ch <- q.ch(i)) if (q.label(ch) == labelCAR(labIndex)) n += 1
        } // if
        n
    } // countLabelsCh

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of parents of vertex 'i' whose label matches 'labCAR'.
     *  @param i         the index of the vertex under consideration
     *  @param labIndex  the label index to match
     *  @param ch        the children
     *  @param name      the name of graph
     */
    private def countLabelsPa (i: Int, labIndex: Int, ch: Array [SET [Int]], name: String): Int =
    {
        var n = 0
        if (name contains "g") {
            for (pa <- g.pa(i)) if (g.label(pa) == labelCAR(labIndex)) n += 1
        } else {
            for (pa <- q.pa(i)) if (q.label(pa) == labelCAR(labIndex)) n += 1
        } // if
        n
    } // countLabelsPa

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if dual cardinality simulation restriction applies for u's children and v's children.
     *  @param u    a vertex in the query graph
     *  @param v    a vertex in the data graph
     *  @param phi  the current mapping of a vertex in the query graph to vertices in the data graph
     */
    private def includedInCh (u: Int, v: Int, phi: Array [SET [Int]]): Boolean =
    {
        var st_vc = SET [Int] ()
        for (u_c <- q.ch(u); v_c <- g.ch(v)) if (phi(u_c) contains v_c) st_vc += v_c

        if (st_vc.nonEmpty) {
            for (uc <- q.ch(u)) {
                val label = q.label(uc)
                val size  = mapQuery.get (u,label)._1.size
                val aichs = mapGraph.get (v, label)._1
                val c     = aichs & st_vc
                if (c.isEmpty) return false
                if (size > aichs.size) return false
            } // for
            true
        } else {
            q.ch(u).isEmpty
        } // if
    } // includedInCh

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if dual cardinality simulation restriction applies for u's parent and v's parent.
     *  @param u    a vertex in the query graph
     *  @param v    a vertex in the data graph
     *  @param phi  the current mapping of a vertex in the query graph to vertices in the data graph
     */
    private def includedInPa (u: Int, v: Int, phi: Array [SET [Int]]): Boolean =
    {
        var st_vp = SET [Int] ()
        for (u_p <- q.pa(u); v_p <- g.pa(v)) if (phi(u_p) contains v_p) st_vp += v_p

        if (st_vp.nonEmpty) {
            for (up <- q.pa(u)) {
                val label     = q.label(up)
                val size      = mapQuery.get (u,label)._2.size
                val aiparents = mapGraph.get (v, label)._2
                val c         = st_vp & aiparents
                if (c.isEmpty) return false
                if (size > aiparents.size) return false
            } // for
            true
        } else {
            q.pa(u).isEmpty
        } // if
    } // includedInPa

} // DualSim2CAR class

import scalation.graphalytics.stringArray

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2CARTest` object is used to test the `DualSim2CAR` class.
 *  > runMain scalation.graphalytics.DualSim2CARTest
 */
object DualSim2CARTest extends App
{
    // -----------------------------------------------------------------------
    // Simple data and query graphs.
    // -----------------------------------------------------------------------

    // data graph g1 ---------------------------------------------------------

    val g1 = new Graph (Array (SET (),                      // ch(0)
                               SET (0, 2, 3, 4),            // ch(1)
                               SET (0),                     // ch(2)
                               SET (4),                     // ch(3)
                               SET ()),                     // ch(4)
                        stringArray ("abc", "xyz" , "def","abc", "zzz"),          // vertex labels
                        true, "g1")                         // inverse, name

    // query graph q1 --------------------------------------------------------

    val q1 = new Graph (Array (SET (1, 2),                  // ch(0)
                               SET (),                      // ch(1)
                               SET (1)),                    // ch(2)
                        stringArray ("xyz", "abc", "def"),
                        true, "q1")

    println (s"g.checkEdges = ${g1.checkEdges}")
    g1.printG ()
    println (s"q.checkEdges = ${q1.checkEdges}")
    q1.printG ()

    (new DualSim2CAR (g1,q1)).test ("DualSim2CAR")

} // DualSim2CARTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2CARTest2` object is used to test the `DualSim2CAR` class.
 *  > runMain scalation.graphalytics.DualSim2CARTest2
 */
object DualSim2CARTest2 extends App
{
    val g2 = new Graph (Array (SET (1),                     // ch(0)
                               SET (0, 2, 3, 4, 5),         // ch(1)
                               SET (),                      // ch(2)
                               SET (),                      // ch(3)
                               SET (),                      // ch(4)
                               SET (6, 10),                 // ch(5)
                               SET (7, 4, 8, 9),            // ch(6)
                               SET (1),                     // ch(7)
                               SET (),                      // ch(8)
                               SET (),                      // ch(9)
                               SET (11),                    // ch(10)
                               SET (12),                    // ch(11)
                               SET (11, 13),                // ch(12)
                               SET (),                      // ch(13)
                               SET (13, 15),                // ch(14)
                               SET (16),                    // ch(15)
                               SET (17, 18),                // ch(16)
                               SET (14, 19),                // ch(17)
                               SET (20),                    // ch(18)
                               SET (14),                    // ch(19)
                               SET (19, 21),                // ch(20)
                               SET (),                      // ch(21)
                               SET (21, 23),                // ch(22)
                               SET (25),                    // ch(23)
                               SET (),                      // ch(24)
                               SET (24, 26),                // ch(25)
                               SET (28),                    // ch(26)
                               SET (),                      // ch(27)
                               SET (27, 29),                // ch(28)
                               SET (22)),                   // ch(29)
                        stringArray ("xyz", "abc", "pqr", "pqr", "pqr", "xyz",
                                     "abc", "xyz", "pqr", "efg", "pqr", "xyz",
                                     "abc", "pqr", "abc", "xyz", "abc", "pqr",
                                     "xyz", "xyz", "abc", "pqr", "abc", "xyz",
                                     "pqr", "abc", "xyz", "pqr", "abc", "xyz"),
                        true, "g")

    // query graph q2 --------------------------------------------------------

    val q2 = new Graph (Array (SET (1),                     // ch(0)
                               SET (0, 2, 3),               // ch(1)
                               SET (),                      // ch(2)
                               SET ()),                     // ch(3)
                        stringArray ("xyz", "abc", "pqr", "pqr"),
                        true, "q")

    (new DualSim2CAR (g2,q2)).test ("DualSim2CAR")

} // DualSim2CARTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2CARTest3` object is used to test the `DualSim2CAR` class.
 *  > runMain scalation.graphalytics.DualSim2CARTest3
 */
object DualSim2CARTest3 extends App
{
    val gSize     = 9          // size of the data graph
    val qSize     = 3          // size of the query graph
    val nLabels   = 4          // number of distinct labels
    val gAvDegree = 2          // average vertex out degree for data graph
    val qAvDegree = 2          // average vertex out degree for query graph

    val g = Graph.g44
    val q = Graph.q44

    //println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    g.printG()

    (new DualSim2CAR (g, q)).test ("DualSim2Car-----------")
    (new DualSim2CAR (g, q)).test ("DualSim2Car-----------")
    (new DualSim2CAR (g, q)).test ("DualSim2Car-----------")

    (new DualSim2CAR (g, q)).test ("DualSim2Car-----------")
    (new DualSim2CAR (g, q)).test ("DualSim2Car-----------")
    (new DualSim2CAR (g, q)).test ("DualSim2Car-----------")

} // DualSim2CARTest3 object

