
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Aug 13 13:49:49 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scala.math.ceil

import scalation.linalgebra.VectorI
import scalation.random.Randi0

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Partition` class is used to partition large directed graphs.
 *  It support the following three algorithms: group_ran, group_ord, group_lp.
 *
 *  (1) Random Partitioning - excellent balance, poor edge cuts
 *  Each vertex is given a randomly assigned integer label 'ilabel' and is grouped
 *  accordingly.
 *
 *  (2) Ordered Partitioning - excellence balance, edge cuts may or may not be good
 *  Each vertex is assigned an integer label 'ilabel' incrementally and is grouped
 *  accordingly, e.g., {0, 1, ..., 9}, {10, 11, ..., 19}, ...
 *
 *  (3) Label Propogation partitioning - fair balance, fair edge cuts
 *  Each vertex is initially given a unique integer label 'ilabel'.  On each iteration,
 *  each vertex will have its 'ilabel' reassigned to the most popular/frequent 'ilabel'
 *  in its neighborhood (which includes its children, parents and itself).
 *  @see research.microsoft.com/pubs/183714/Partition.pdf
 *----------------------------------------------------------------------------
 *  @param g  the directed graph to partition
 */
class Partition (g: Graph)
{
    private val DEBUG    = true                                  // debug flag
    private val MAX_ITER = 10                                    // maximum number of iterations
    private val ilabel   = VectorI.range (0, g.size)             // current integer label for each vertex
    private val ilabel2  = new VectorI (g.size)                  // new integer label for each vertex
    private val pop      = new VectorI (g.size)                  // holder for population/frequency
    private val rng      = Randi0 (g.size)                       // random number generator
    private val partMap  = Map [Int, SET [Int]] ()               // k-way partition of graph g

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group the vertices based on their randomly generated 'ilabel's.
     *  @param k  the number of subgraphs to create
     */
    def group_ran (k: Int)
    {
        for (i <- ilabel.indices) ilabel(i) = rng.igen % k
        if (DEBUG) println (s"group_ran: after grouping nDistnct = ${ilabel.distinct}")
    } // group_ran

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group the vertices based on the order of their 'ilabel's, which are
     *  assigned incrementally.
     *  @param k  the number of subgraphs to create
     */
    def group_ord (k: Int)
    {
        val gsize = ceil (ilabel.dim / k.toDouble).toInt
        for (i <- ilabel.indices) ilabel(i) = i / gsize
        if (DEBUG) println (s"group_ord: after grouping nDistnct = ${ilabel.distinct}")
    } // group_ord

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group the vertices based on their 'ilabel's using label propogation.
     *  @param k  the number of subgraphs to create
     */
    def group_lp (k: Int)
    {
        var nDistnct = 0
        for (it <- 1 to MAX_ITER) {
            for (i <- ilabel.indices) getPopularLabel (i)        // re-determine the popoular labels
            if (DEBUG) println (s"group_lp: ilabel2 = $ilabel2")
            for (i <- ilabel.indices) {
                reassign (i)                                     // reassign vertex labels
                nDistnct = ilabel.distinct
                if (ilabel.distinct <= k) return                 // quit when only k parts
            } // for
            if (DEBUG) println (s"group_lp: after iteration $it nDistnct = $nDistnct")
        } // for
    } // group

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Partition vertices with the same 'ilabel' after grouping.
     */
    def partition ()
    {
        if (DEBUG) println (s"partition: ilabel = $ilabel")
        var part = 0
        for (i <- ilabel.indices) {                              // put vertex into map based on ilabel
            val iset = partMap.getOrElse (ilabel(i), null)
            if (iset == null) partMap += ilabel(i) -> SET (i)
            else              iset += i
        } // for
        if (DEBUG) println (s"partition: partMap = $partMap")
    } // partition

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form subgraphs 'gi's from the orginal graph 'g'
     */
    def formGraphs (): ArrayBuffer [Graph] =
    {
        val parts = ArrayBuffer [Graph] ()
        var i     = 0
        for (sg <- partMap.values) {                             // for each subgraph sg
            if (DEBUG) println (s"formGraph: sg = $sg")
            val ch2  = Array.ofDim [SET [Int]] (sg.size)
            val lab2 = Array.ofDim [TLabel] (sg.size)
            val vid2 = Array.ofDim [Int] (sg.size)

            var j = 0
            for (v <- sg) {
                ch2(j)  = g.ch(v) & sg                           // child vertices gi's for vertex v
                lab2(j) = g.label(v)                             // label for for gi's vertex v 
                vid2(j) = v                                      // id of gi's vertex v
                j = j + 1
            } // for

// FIX - the next line fails when inverse is true
//          val gi = new Graph (ch2, lab2, g.inverse, g.name + "_" + i, vid2)
            val gi = new Graph (ch2, lab2, false, g.name + "_" + i, vid2)
            parts += gi
            i = i + 1
        }  // for
        parts
    } // formGraphs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine and save the the most popular 'ilabel' from vertex 'i's
     *  neighborhood (its children, parents and itself).
     *  @param i  the vertex to consider
     */
    private def getPopularLabel (i: Int)
    {
        val child  = VectorI (g.ch(i).toArray.map (ilabel(_)))   // the labels of the children of vertex i
        val parent = VectorI (g.pa(i).toArray.map (ilabel(_)))   // the labels of the parents of vertex i
        val lab    = child ++ parent ++ VectorI (ilabel(i)) 
        if (DEBUG) println (s"getPopularLabel: for $i lab = $lab")

        pop.set (0)                                              // reset the population counters
        for (i <- lab.indices) pop(lab(i)) = pop(lab(i)) + 1     // increment population count
        ilabel2(i) = pickMostPopular ()                          // assign the index of highest count
    } // getPopularLabel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly pick one of the most popular labels.
     */
    private def pickMostPopular (): Int =
    { 
        val mxList = ArrayBuffer [Int] ()                        // list to hold most popular
        val mx     = pop.max ()                                  // maximum frequency count
        for (i <- pop.indices if pop(i) == mx) mxList += i       // add to most popular list
        println (s"pickMostPopular: mxList = $mxList")
        ilabel (mxList (rng.igen % mxList.size))                 // randomly pick one
    } // pickMostPopular

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reassign the 'ilabel' for vertex 'i' using the the highest frequency
     *  'ilabel' among itself and its children.
     *  @param i  the vertex to reassign  
     */
    private def reassign (i: Int)
    {
        val newLab = ilabel2(i)                                  // get new ilabel
        if (newLab != ilabel(i)) {
            if (DEBUG) println (s"reassign: vertex $i from ${ilabel(i)} to $newLab")
            ilabel(i) = newLab                                   // assign the new ilabel
        } // if
    } // reassign

} // Partition class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PartitionTest` object is used to test the `Partition` class.
 *  This test uses random partitoning.
 *  > run-main scalation.graphalytics.mutable.PartitionTest
 */
object PartitionTest extends App
{
    println ("TEST random partitioning")
    val g  = Graph.g2p
    val dp = new Partition (g)
    dp.group_ran (4)
    dp.partition ()

    var edgeSum = 0
    for (gi <- dp.formGraphs ()) {
        gi.printG ()
        val ne   = gi.nEdges
        edgeSum = edgeSum + ne
        println (s"nEdges = $ne")
    } // for

    println (s"edgeSum  = $edgeSum")
    println (s"g.nEdges = ${g.nEdges}")
    g.printG ()

} // PartitionTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PartitionTest2` object is used to test the `Partition` class.
 *  This test uses ordered partitioning.
 *  > run-main scalation.graphalytics.mutable.PartitionTest2
 */
object PartitionTest2 extends App
{
    println ("TEST ordered partitioning")
    val g  = Graph.g2p
    val dp = new Partition (g)
    dp.group_ord (4)
    dp.partition ()

    var edgeSum = 0
    for (gi <- dp.formGraphs ()) {
        gi.printG ()
        val ne   = gi.nEdges
        edgeSum = edgeSum + ne
        println (s"nEdges = $ne")
    } // for

    println (s"edgeSum  = $edgeSum")
    println (s"g.nEdges = ${g.nEdges}")
    g.printG ()

} // PartitionTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PartitionTest3` object is used to test the `Partition` class.
 *  This test uses label propogation for partitioning.
 *  > run-main scalation.graphalytics.mutable.PartitionTest3
 */
object PartitionTest3 extends App
{
    println ("TEST label propogation for partitioning")
    val g  = Graph.g2p
    val dp = new Partition (g)
    dp.group_lp (4)
    dp.partition ()

    var edgeSum = 0
    for (gi <- dp.formGraphs ()) {
        gi.printG ()
        val ne   = gi.nEdges
        edgeSum = edgeSum + ne
        println (s"nEdges = $ne")
    } // for

    println (s"edgeSum  = $edgeSum")
    println (s"g.nEdges = ${g.nEdges}")
    g.printG ()

} // PartitionTest3 object

