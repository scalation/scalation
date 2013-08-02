
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, John Miller
 *  @version 1.0
 *  @date    Mon May  6 10:50:37 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 */

package scalation.graphalytics

// import collection.mutable.Set

import GraphTypes._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DualSim' classs provides an implementation for Dual Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim (g: Graph, q: Graph)
      extends PatternMatcher (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [ISet] = nisarDualSim (feasibleMates ())

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
    def nisarDualSim (phi: Array [ISet]): Array [ISet] =
    {
        var alter = true
        while (alter) {                                     // check for matching children and parents
            alter = false

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c

            for (u <- qRange; v <- phi(u); u_c <- q.adj(u)) {
                if ((g.adj(v) & phi(u_c)).isEmpty) {        // v must have a child in phi(u_c)
                    phi(u) -= v                             // remove v due to lack of child match 
                    alter  = true
                } // if
            } //for

            // loop over query vertices u, data vertices v in phi(u), and u's parents u_p

            for (u <- qRange; v <- phi(u); u_p <- q.par(u)) {
                if ((g.par(v) & phi(u_p)).isEmpty) {        // v must have a parent in phi(u_p)
                    phi(u) -= v                             // remove v due to lack of parent match
                    alter   = true
                } // if
            } //for

        } // while
        phi
    } // nisarDualSim

} // DualSim


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DualSimTest' object is used to test the 'DualSim' class.
 */
object DualSimTest extends App
{  
    if (args.length != 4) {
        println ("usage: scala -cp classes scalation.graphalytics.DualSim [DataGraph] [QueryGraph] [Repeats] [DisplayResults(0/1)]")
    } else {
        val g = Graph (args(0), true)                         // data graph
        val q = Graph (args(1), true)                         // query graph
        val matcher = new DualSim (g, q)                      // Dual Graph Simulation Matcher

        for (i <- 0 until args(2).toInt) {                    // loop Repeats times
//          g.labelMap.clear ()
            System.gc ()                                      // run garbage collector (gc)
            Thread.sleep (2000)                               // give gc time to work
            val sim = matcher.time { matcher.mappings () }    // time the matcher
            if (args(3).toInt == 1) for (u <- 0 until q.size) println (u + " -> " + sim(u))
        } // for
    } // if

} // DualSimTest object

