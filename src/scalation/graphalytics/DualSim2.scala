
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.0
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

// import collection.mutable.Set

import GraphTypes._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DualSim2' class provides a second implementation for Dual Graph Simulation.
 *  It differs from DualSim by not using inverse adjacency sets ('par') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim2 (g: Graph, q: Graph)
      extends PatternMatcher (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [ISet] = saltzDualSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Dual Graph Simulation by itself can not produce bijections.
     */
    def bijections (): Set [Array [Int]] =
    {
        throw new UnsupportedOperationException ()
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def saltzDualSim (phi: Array [ISet]): Array [ISet] =
    {
        var alter = true
        while (alter) {
            alter = false

            // loop over query vertices u and u's children u_c

            for (u <- qRange; u_c <- q.adj (u)) {
                var newPhi: ISet = Set [Int] ()                   // subset of phi(u_c) having a parent in phi(u)
                for (v <- phi (u)) {                              // data vertex v matching u's label
                    val phiTemp = g.adj (v) & phi (u_c)           // children of v contained in phi(u_c)
                    if (phiTemp.isEmpty) {
                        phi (u) -= v                              // remove vertex v from phi(u)
                        if (phi (u).isEmpty) return EMPTY         // no match for vertex u => no overall match
                        alter = true
                    } // if
                    // build newPhi to contain only those vertices in phi(u_c) which also have a parent in phi(u)
                    newPhi ++= phiTemp
                } // for

                if (newPhi.isEmpty) return EMPTY                  // empty newPhi => no match
                if (newPhi.size < phi (u_c).size) alter = true    // since newPhi is smaller than phi(u_c)

                if (SELF_LOOPS && u_c == u) phi (u_c) &= newPhi else phi (u_c) = newPhi
            } // for

        } // while
        phi
    } // saltzDualSim

} // DualSim2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DualSim2Test' object is used to test the 'DualSim2' class.
 */
object DualSim2Test extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = GraphGenerator.genRandomGraph (gSize, nLabels, gAvDegree)
    val q = GraphGenerator.genBFSQuery (qSize, qAvDegree, g)

    val matcher = new DualSim2 (g, q)                      // Dual Graph Simulation Pattern Matcher
    val phi     = matcher.time { matcher.mappings () }     // time the matcher
    for (i <- phi.indices) println ("u_" + i + ": " + phi(i)) 

} // DualSim2Test object

