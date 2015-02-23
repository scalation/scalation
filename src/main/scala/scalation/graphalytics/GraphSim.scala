
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, John Miller
 *  @version 1.1
 *  @date    Mon May  6 10:50:37 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 */

package scalation.graphalytics

// import collection.mutable.Set

import scalation.util.Timer.time

import GraphTypes._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim (g: Graph, q: Graph)
      extends PatternMatcher (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [ISet] = nisarGraphSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Graph Simulation by itself can not produce bijections.
     */
    def bijections (): Set [Array [Int]] =
    {
        throw new UnsupportedOperationException ()
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def nisarGraphSim (phi: Array [ISet]): Array [ISet] =
    {
        var alter = true
        while (alter) {                                     // check for matching children
            alter = false

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c

            for (u <- qRange; v <- phi(u); u_c <- q.adj(u)) {
                if ((g.adj(v) & phi(u_c)).isEmpty) {        // v must have a child in phi(u_c)
                    phi(u) -= v                             // remove v due to lack of child match 
                    alter   = true
                } // if
            } //for           

        } // while
        phi
    } // nisarGraphSim

} // GraphSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest` object is used to test the `GraphSim` class.
 */
object GraphSimTest extends App
{  
    if (args.length != 4) {
        println ("usage: scala -cp classes scalation.graphalytics.GraphSim [DataGraph] [QueryGraph] [Repeats] [DisplayResults(0/1)]")
    } else {
        val g = Graph (args(0), false)                        // data graph
        val q = Graph (args(1), false)                        // query graph
        val matcher = new GraphSim (g, q)                     // Simple Graph Simulation Pattern Matcher   
    
        for (i <- 0 until args(2).toInt) {                    // loop repeats times
//          g.labelMap.clear ()
            System.gc ()                                      // run garbage collector (gc)
            Thread.sleep (2000)                               // give gc time to work
            val sim = time { matcher.mappings () }            // time the matcher
            if (args(3).toInt == 1) for (u <- 0 until q.size) println (u + " -> " + sim(u))
        } // for
    } // if

} // GraphSimTest object

