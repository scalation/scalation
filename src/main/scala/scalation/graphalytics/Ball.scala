//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Arash Fard, Usman Nisar, Ayushi Jain
 *  @version 1.2
 *  @date    Thu Oct 20 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.{Map, Queue, Set => SET}
import collection.mutable
import math.max

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Ball` class provides an implementation for ball construction.
 *  A ball consists of all vertices within a given radius of a given center.
 *  @see ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=6691601&tag=1&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D6691601%26tag%3D1
 *  @param srcGraph  the graph from which the ball will be created based upon center vertex and radius
 *  @param center    the vertex picked as the center of the ball
 *  @param radius    the radius of the ball
 */
class Ball (srcGraph: Graph, val center: Int, radius: Int) 
{
    val nodesInBall = SET [Int] ()                            // vertices in this ball
    val borderNodes = SET [Int] ()                            // border vertices of this ball
    var chMap       = Map [Int, SET [Int]] ()                 // child vertex mapping
    var paMap       = Map [Int, SET [Int]] ()                 // parent vertex mapping

    private val qu      = Queue [(Int, Int)] ()               // vertex, depth pair
    private var counter = 0

    nodesInBall += center
    qu.enqueue ((center, 0))
    chMap += (center -> SET (srcGraph.ch(center).toArray: _*))
    paMap += (center -> SET (srcGraph.pa(center).toArray: _*))

    while (! qu.isEmpty) {                // fetching all the vertices which will be part of a ball
        val (nextV, depth) = qu.dequeue ()
        if (depth == radius) borderNodes += nextV
        else {
            // get all the children till depth == radius starting from the center
            for (c <- srcGraph.ch(nextV) if ! (nodesInBall contains c)) {
                nodesInBall += c
                qu.enqueue ((c, depth + 1))
            } // for
            // get all the parents till depth == radius starting from the center
            for (p <- srcGraph.pa(nextV) if ! (nodesInBall contains p)) {
                nodesInBall += p
                qu.enqueue ((p, depth + 1))
            } // for
        } // if
    } // while

    for (v <- nodesInBall) {                     // prepare ch and parent set for the nodes of a ball
        chMap.getOrElseUpdate (v, SET ())
        paMap.getOrElseUpdate (v, SET ())
        for (c <- srcGraph.ch(v) if (nodesInBall contains c)) paMap.getOrElseUpdate (c, SET ()) += v
        for (p <- srcGraph.pa(v) if (nodesInBall contains p)) chMap.getOrElseUpdate (p, SET ()) += v
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the set of children of vertex 'v'.
     *  @param v  the identifier of the input vertex
     */
    def post (v: Int): SET [Int] = chMap.getOrElse (v, SET [Int] ())
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the set of parents of vertex 'v'.
     *  @param v the identifier of the input vertex
     */
    def pre (v: Int): SET [Int] = paMap.getOrElse (v, SET [Int] ())
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the ball into a string.
     */
    override def toString: String = 
    {
      var s = new StringBuilder ()
      for (i <- chMap.keySet.toList.sorted) {
           s.append (i + "->[");
           for (j <- chMap.get (i).get.toList.sorted) s.append (j + ",")
           s.append("],")
       } // for
       s.toString
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the ball.
     */
    def printBall
    {
        println ("Center: " + center)
        println ("Nodes in ball: " + nodesInBall)
        for ((u, c) <- chMap) println (u + "\t" + c)
        println ("Border nodes: ")
        for (b <- borderNodes) println (b)
        println ("----------------------")
    } // printBall
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the diameter of the ball.
     */
    def getBallDiameter: Int = 
    {
        val n    = nodesInBall.max + 1
        val path = Array.ofDim [Int] (n, n)
        for (u <- nodesInBall; v <- nodesInBall if u != v) {
            if (post (u) contains v) { path(v)(u) = 1; path(u)(v) = 1 }
        } // for       
        var diameter = 0
        for (k <- nodesInBall; i <- nodesInBall; j <- nodesInBall if i != j) {
            if (path(i)(k) * path(k)(j) != 0) {
                if ((path(i)(k) + path(k)(j) < path(i)(j)) || path(i)(j) == 0) path(i)(j) = path(i)(k) + path(k)(j)
            } // if
            diameter = max (path(i)(j), diameter)
        } // for
        diameter    
    } // getBallDiameter
  
} // Ball class


object BallTest extends App
{
} // BallTest object

