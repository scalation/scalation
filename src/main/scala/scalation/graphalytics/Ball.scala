//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Arash Fard, Usman Nisar, Ayushi Jain
 *  @version 1.1
 *  @date    Thu Oct 20 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.{Queue, Set, Map, BitSet}
import collection.mutable
import math.max

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Ball` class provides an implementation for ball construction.
 *  A ball consists of all vertices within a given radius of a given center.
 *  @see http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=6691601&tag=1&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D6691601%26tag%3D1
 *  @param parentGraph  thea graph from which the ball will be created based upon center vertex and radius
 *  @param center       the vertex picked as center
 *  @param radius       the radius of the ball
 */
class Ball (parentGraph: Graph, center: Int, radius: Int) 
{
    var nodesInBall = mutable.Set [Int] ()                 // vertices in a ball
    var borderNodes = mutable.Set [Int] ()                 // border vertices of a ball
    var q           = Queue [(Int, Int)] ()                // vertex, depth pair
    var adjSet      = Map [Int, mutable.Set [Int]] ()      // child vertex mapping
    var parList     = Map [Int, mutable.Set [Int]] ()      // parent vertex mapping
    var ballcenter  = center                               // center vertex
    nodesInBall    += center
    var counter     = 0
    q.enqueue ((center, 0))
    // assumes diameter always > 0
    adjSet  += (center -> mutable.Set (parentGraph.adj(center).toArray:_* ))
    parList += (center -> mutable.Set (parentGraph.par(center).toArray:_* ))

    while (! q.isEmpty) {                        // fetching all the nodes or vertices which will be part of a ball
        val (nextV, depth) = q.dequeue
        if (depth == radius) borderNodes += nextV
        else {
            val children = parentGraph.adj(nextV)
            val parents  = parentGraph.par(nextV)
            for (child <- children) {            // get all the children till depth == radius starting from the center
                if (! nodesInBall.contains (child)) {
                    nodesInBall += child
                    q.enqueue ((child, depth + 1))
                } // if
            } // for
            for (parent <- parents) {            // get all the parents till depth == radius starting from the center
                if (! nodesInBall.contains (parent)) {
                    nodesInBall += parent
                    q.enqueue ((parent, depth + 1))
                } // if
            } // for
        } // else
    } // while

    for (n <- nodesInBall) {                     // prepare adj and parent set for the nodes of a ball
        val children = parentGraph.adj(n)
        val parents  = parentGraph.par(n)
        adjSet.getOrElseUpdate (n, Set ())
        parList.getOrElseUpdate (n, Set ())
        for (child <- children if (nodesInBall contains child))  parList.getOrElseUpdate (child, Set ()) += n
        for (parent <- parents if (nodesInBall contains parent)) adjSet.getOrElseUpdate (parent, Set ()) += n
    } // for

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the set of children of vertex 'v'.
    * @param v  the identifier of the input vertex
    */
  def post (v: Int): Set [Int] = adjSet.getOrElse (v, Set [Int] ())
  
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the set of parents of vertex 'v'.
    * @param v the identifier of the input vertex
    */
  def pre (v: Int): Set [Int] = parList.getOrElse (v, Set [Int] ())
  
 //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Get the ball in a string format.
   */
  def getBallAsString (): String = 
  {
      var s = new StringBuilder ()
      if (adjSet != null) {
           for (i <- adjSet.keySet.toList.sorted) {
               s.append (i + "->[");
               for (j <- adjSet.get (i).get.toList.sorted) s.append (j+",")
               s.append("],")
           } // for
       } // if
       s.toString ()
  } // getBallAsString

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Print the ball.
   */
  def printball ()
  {
      println ("Center: " + ballcenter)
      println ("Nodes in ball: " + nodesInBall)
      for ((u, children) <- adjSet) println (u + "\t" + children)
      println ("Border nodes: ")
      for (b <- borderNodes) println (b)
      println ("-----------")
  } // printball
  
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the diameter of the ball.
   */
  def getBallDiameter: Int = 
  {
      val n    = nodesInBall.max + 1
      val path = Array.ofDim [Int] (n, n)
      for (u <- nodesInBall; v <- nodesInBall if u != v) {
          if (post (u).contains (v)) {
              path(v)(u) = 1
              path(u)(v) = 1
          } // if
      } // for       
      var diameter = 0
      for (k <- nodesInBall; i <- nodesInBall; j <- nodesInBall if i != j) {
          if (path(i)(k) * path(k)(j) != 0)
          {
              if ((path(i)(k) + path(k)(j) < path(i)(j)) || path(i)(j) == 0) path(i)(j) = path(i)(k) + path(k)(j)
          } // if
          diameter = max (path(i)(j), diameter)
      } // for
      diameter    
  } // getBallDiameter
  
} // Ball class

