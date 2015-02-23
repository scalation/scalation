
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.1
 *  @date    Wed Nov 13 15:27:19 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.ArrayBuffer

import scalation.util.Timer.time

import Graph2Types._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim3` class provides a second implementation for Dual Graph Simulation.
 *  It differs from DualSim by not using inverse adjacency sets ('par') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim3 (g: Graph2, q: Graph2)
      extends PatternMatcher2 (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [AList] = saltzDualSim (feasibleMates ())

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
    def saltzDualSim (phi: Array [AList]): Array [AList] =
    {
        var alter = true
        while (alter) {
            alter = false

            // loop over query vertices u and u's children u_c

            for (u <- qRange; u_c <- q.adj (u)) {
                var newPhi: AList = ArrayBuffer [Int] ()              // subset of phi(u_c) having a parent in phi(u)
                for (v <- phi (u)) {                                  // data vertex v matching u's label
                    val phiTemp = g.adj (v) intersect phi (u_c)       // children of v contained in phi(u_c)
                    if (phiTemp.isEmpty) {
                        phi (u) -= v                                  // remove vertex v from phi(u)
                        if (phi (u).isEmpty) return Array [AList] ()  // no match for vertex u => no overall match
                        alter = true
                    } // if
                    // build newPhi to contain only those vertices in phi(u_c) which also have a parent in phi(u)
                    newPhi ++= phiTemp
                } // for

                if (newPhi.isEmpty) return Array [AList] ()           // empty newPhi => no match
                if (newPhi.size < phi (u_c).size) alter = true        // since newPhi is smaller than phi(u_c)

                if (SELF_LOOPS && u_c == u) phi (u_c) = (phi (u_c) intersect newPhi) else phi (u_c) = newPhi
            } // for

        } // while
        phi
    } // saltzDualSim

} // DualSim3 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim3Test` object is used to test the 'DualSim3' class.
 */
object DualSim3Test extends App
{
    val q = Graph2 (Array (ArrayBuffer (1),
                           ArrayBuffer (0)),
                    Array (0, 1))

    val g = Graph2 (Array (ArrayBuffer (1),
                           ArrayBuffer (0),
                           ArrayBuffer (0),
                           ArrayBuffer (2)),
                   Array (0, 1, 1, 0))

    q.print
    g.print

    val matcher = new DualSim3 (g, q)                      // Dual Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    for (i <- phi.indices) println ("u_" + i + ": " + phi(i)) 

} // DualSim3Test object

