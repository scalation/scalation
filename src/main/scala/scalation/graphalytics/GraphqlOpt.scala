
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Ayushi Jain, John Miller
 *  @version 1.2
 *  @date    Thu Feb 4 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.immutable.{Set => SET}
import collection._
import collection.mutable.{HashMap, MutableList}
import util.control.Breaks._

import scalation.util.time

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphqlOpt' class provides an implementation for Subgraph Isomorphism
 *  of GraphQL's Algorithm.
 *  @see http://cs.ucsb.edu/~dbl/papers/he_sigmod_2008.pdf (GraphQL paper)
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphqlOpt (g: Graph, q: Graph) 
{
     private var phi           = Array.ofDim [SET [Int]] (q.size)     // initialize empty feasible match sets
     private var matches       = mutable.Set [Array [Int]] ()         // initialize empty matches
     private var matchedVertex = Array.fill (q.size) {-1}             // initialize matched vertex for each u to -1
     private val qProfileMap   = HashMap [Int, MutableList [TLabel]] ()  // initialize profile mapping of each query vertex 'u' to empty list
     private val gProfileMap   = HashMap [Int, MutableList [TLabel]] ()  // initialize profile mapping of each data vertex 'v' to empty list
     private val searchOrder   = Array.ofDim [Int] (q.size)           // initialize empty search order
     private val searchOrderEnabled = true                            // by default search order is enabled
     private val LIMIT = 1E7                                          // quit after too many matches
   
     for (u <- 0 until q.size) qProfileMap += u -> getNeighborProfile (q, u)   // get Neighborhood profile of query vertex 'u', Figure 13
     for (v <- 0 until g.size) gProfileMap += v -> getNeighborProfile (g, v)   // get Neighborhood profile of data vertex 'v', Figure 13
     println ("Neighbor, Profile processing done")
   
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Create an initial array of feasible mappings 'phi' from each query
      *  vertex 'u' to the corresponding set of data graph vertices '{v}' whose
      *  label matches 'u's.
      */
     def feasibleMates (): Array [SET [Int]] =
     {
         q.label.map (u_label => g.getVerticesWithLabel (u_label))
     } // feasibleMates

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Apply the GraphQL Subgraph Isomorphism algorithm to find subgraphs of data
      *  graph 'g' that isomorphically match query graph 'q'.  
      *  This method represents the Algorithm 1: Graph Pattern Matching of the GraphQL paper
      */
     def bijections () 
     {
         matches = mutable.Set [Array [Int]] ()
         phi     = feasibleMates ()                               // initial mappings from label match
         println ("Neighbor Local Pruning started")
         neighborProfilePruning ()                                // neighborhood local pruning, Section 4.2 of the GraphQL paper
         println ("Neighbor Local Pruning completed")
         println ("Global Pruning started")
//       refineSearchSpace (q.size)                               // reduce global search space, Section 4.3 of the GraphQL paper
         println ("Global search space pruning completed")
         getSearchOrder ()                                        // optimize search order, Section 4.4 of the GraphQL paper
         search (0)
     } // bijections
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Sorts according to the feasible matches size in ascending order.
      *  This method represents the Section 4.4.2 : Optimization of search order based
      *  on greedy approach of the GraphQL paper.
      *  i.e., at join i, choose a leaf node that minimizes the estimated cost of the join.
      */
     def getSearchOrder ()
     {
         val phiSortMap = new HashMap [Int, Int] ()
         for (u <- 0 until phi.size) phiSortMap += u -> phi (u).size
         phiSortMap.toSeq.sortBy (_._2).map (x => x._1).copyToArray (searchOrder)
         println ("Search Order decided")
     } // getSearchOrder
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Iterates on the u'th Node to find feasible mapping for that node.
      *  This is the part of Algorithm 1, Line 8 to 18, of the GraphQL paper
      *  @param depth  the depth of recursion
      */
     def search (depth: Int) 
     {
         val u = if (searchOrderEnabled) searchOrder (depth) else depth
         for (v <- phi (u)) {
             if (! contains (matchedVertex, v) && check (u, v)) {
                 matchedVertex (u) = v                                  // v is the matched vertex for u    
                 if (depth < q.size - 1) {
                     if (matches.size >= LIMIT) return                  // quit if at LIMIT
                     search (depth + 1)                                 // search at next depth
                 } else {
                     matches += matchedVertex.clone                     // Match Found
                     if (matches.size >= LIMIT) return
                 } // if
             } // if
             matchedVertex (u) = -1
         } // for
     } // search
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Is vertex v contained in any matched vertex?
      *  @param matVert   array of matched vertex from a query vertex u to graph vertex v
      *  @param v         the vertex v to check
      */
     def contains (matVert: Array [Int], v: Int): Boolean =
     {
         for (i <- matVert if i == v) return true
         false
     } // contains
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Examines, if u can be mapped to v, by considering their edges.
      *  This method is the part of Algorithm 1, line 19 to 26, of the GraphQL paper.
      *  @param u  vertex u can be mapped to v
      *  @param v  vertex v can be mapped to u
      */
     def check (u: Int, v: Int): Boolean = 
     {
         for (u_c <- q.ch (u)) {                                           // directed graph, so child check
             breakable { 
                 if (matchedVertex (u_c) != -1) {                          // vertex u_c is already evaluated, so v_c is selected for u_c
                     if (g.ch (v) contains matchedVertex (u_c)) break      // if adjacency vertices of v does not contain v_c, return false  
                     else return false
                 } // if 
                 for (v_c <- g.ch (v) if phi (u_c) contains v_c)  break    // vertex u_c is not evaluated, so phi(u_c), should contain v_c of v
                 return false
             } // breakable 
         } // for

         for(u_p <- q.pa (u)) {                                            // directed graph, so parent check
             breakable { 
                 if (matchedVertex (u_p) != -1) {                          // vertex u_p is already evaluated, so v_p is selected for u_p
                     if (g.pa (v) contains matchedVertex (u_p)) break      // if adjacency vertices of v does not contain v_p, return false
                     else return false
                 } // if 
                 for (v_p <- g.pa (v) if phi (u_p) contains v_p) break     // vertex u_p is not evaluated, so phi(u_p) should contain v_p of v
                 return false
             } // breakable
         } // for
         true
     } // check
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Examines neighborhood profile(sorted neighborhood labels) of query vertex u
      *  with feasible vertex v profile of v must contain profile of u.
      *  This method is the part of Algorithm 1: line 3 and the section 4.2 of the GraphQL paper.
      */
     def neighborProfilePruning ()
     {
         for (u <- 0 until q.size) {
             for (v <- phi (u)) {
                 if (! (qProfileMap (u) diff gProfileMap (v)).isEmpty) phi (u) -= v
             } // for
         } // if
      } // neighborProfilePruning
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Checks whether at level l, subtree of u is sub-isomorphic to v only if
      *  Bipartite graph of B(u,v) has semi-perfect matching.  This method is the
      *  implementation of Algorithm 2: Refine Search Space, of the GraphQL paper.
      *  @param level   to what level we want to refine.
      *
     def refineSearchSpace (level: Int) 
     {
         val mark = HashMap [String, Boolean] ()
         for (u <- 0 until q.size; v <- phi (u)) mark.put ("(" +u + "," +v + ")", true) {             // initialize all pair of u and v to true
             breakable { 
                 for (i <- 1 to level) {
                     for (u <- 0 until q.size; v <- phi (u) if (mark("(" +u + "," +v + ")"))) {
                         val bipartite = Array.fill (q.size, g.size) (false)                          // start constructing bipartite graph B(u,v)
                         val uNeighbors = getNeighbors (q, u)
                         val vNeighbors = getNeighbors (g, v)
                         for (u0 <- uNeighbors; v0 <- vNeighbors if (phi (u0) contains v0)) bipartite (u0)(v0) = true    // construct bipartite graph
                         val maxBipSize = new BipartiteMatching (bipartite).maxBPM                    // find max bipartite match all u to v, unique pair
                         if (maxBipSize == uNeighbors.size) mark.put ("(" +u + "," +v + ")", false)   // max bipartite is equal to u's neighbor size,
                                                                                                      // that means a semi-perfect match is found    
                         else {
                             phi(u) -= v                                                              // if not semi-perfect match, remove v from phi(u)
                             for (u0 <- uNeighbors ; v0 <- vNeighbors if (phi (u0) contains v0)) mark.put ("(" +u0 + "," +v0 + ")", true )
                         } // if
                      } // for
                      if (! mark.values.toList.contains (true)) break                                 // no pair (u,v) in mark is true, then no further pruning
                  } // for
             } // breakable
         } // for
     } // refineSearchSpace
      */
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Finds neighborhood profile of vertex u in a graph. The profile is constructed
      *  in lexicographic order, refer figure 13 of the GraphQL paper for example.
      *  @param graph   For which graph, we are finding neighbor profile of u
      *  @param u       Vertex u, for which neighborhood profile is created
      */
     def getNeighborProfile (graph: Graph, u: Int) : MutableList [TLabel] = 
     {
         var profileList = scala.collection.mutable.MutableList [TLabel] ()
         for (vertex <- graph.ch (u)) profileList += graph.label (vertex)
         for (vertex <- graph.pa (u)) profileList += graph.label (vertex)
         profileList += graph.label (u)
//       profileList.sorted                        // FIX: check if sorting is required
         profileList
     } // getNeighborProfile
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Finds all the neighbors of vertex u in a graph.
      *  @param graph   For which graph, we are finding neighbors of u
      *  @param u       Vertex u, for which all neighbors are found
      */
     def getNeighbors (graph: Graph, u : Int): SET [Int] = 
     {
         var uNeighbors = SET [Int] ()
         uNeighbors ++=  graph.ch (u) 
         uNeighbors ++=  graph.pa (u)
         uNeighbors += u
         uNeighbors
     } // getNeighbors
    
     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Returns all subgraph isomorphic matches.
      */
     def getMatches (): Set [Array [Int]] = matches
     
} // GraphqlOpt class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphqlOptTest` object randomly generates the data graph and corresponding
 *  query graph to test `GraphqlOpt` class.
 */
object GraphqlOptTest extends App
{
    val gSize     = 5000            // size of the data graph
    val qSize     = 20              // size of the query graph
    val nLabels   = 20              // number of distinct labels
    val gAvDegree = 20              // average vertex out degree for data graph
    val qAvDegree = 8               // average vertex out degree for query graph

    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree, true, "g")
    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g, true, "q")

    g.print ()                                                // print data graph
    q.print ()                                                // print query graph

    val matcher = new GraphqlOpt (g, q)                       // GraphqlOpt Subgraph Isomorphism Pattern Matcher
    
    for (i <- 0 until 5) { 
        time { matcher.bijections () }                        // time the matcher
        println("Match Count: " + matcher.getMatches.size)
        for (mat <- matcher.getMatches ) println (mat.deep)   // print Matches
    } // for 

} // GraphqlOptTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphqlOptTest2` object test the `GraphqlOpt` class by feeding the value
 *  of data graph and query graph.
 */
object GraphqlOptTest2 extends App
{
    val g = new Graph (Array (SET [Int] (),
                              SET [Int] (),
                              SET (1, 4),
                              SET (0, 1, 2),
                              SET (0)),
                       Array (0, 2, 2, 1, 2),
                       true, "g")
    
    val q = new Graph (Array (SET (1),
                              SET [Int] ()),
                       Array (2, 0),
                       true, "q")
    g.print ()                                                // print data graph
    q.print ()                                                // print query graph

    val matcher = new GraphqlOpt (g, q)                       // GraphqlOpt Subgraph Isomorphism Pattern Matcher

    for (i <- 0 until 5) {
        time { matcher.bijections () }                        // time the matcher
        println ("Match Count: " + matcher.getMatches.size)
        for (mat <- matcher.getMatches ) println (mat.deep)
    } // for

 } // GraphqlOptTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ::::::::::::
/** The `GraphqlOptTest3` object test the `GraphqlOpt` class by feeding data graph
 *  and query graph relative file path.
 */
object GraphqlOptTest3 extends App
{
    val g = GraphIO ("gFile")                                 // read data graph
    val q = GraphIO ("qFile")                                 // read query graph

    g.print ()                                                // print data graph
    q.print ()                                                // print query graph

    val matcher = new GraphqlOpt (g, q)                       // GraphqlOpt Subgraph Isomorphism Pattern Matcher

    for (i <- 0 until 5) {
        time { matcher.bijections () }                        // time the matcher
        println ("Match Count: " + matcher.getMatches.size)
        for (mat <- matcher.getMatches) println (mat.deep)
    } // for

} // GraphqlOptTest3

