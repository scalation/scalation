
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Tue Nov 29 15:46:51 EST 2011
 *  @see     LICENSE (MIT style license file).
 *  @see     www.ece.virginia.edu/~mv/edu/715/lectures/QNet.pdf
 *  @see     hspm.sph.sc.edu/courses/716-8%20Queuing%20Theory%20Cookbook.pdf
 */

package scalation.queueingnet

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.Combinatorics.fac
import scalation.math.DoubleWithExp._
import scalation.math.IntWithExp._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to solve Jackson Queueing Network problems.  Each node in
 *  the network represents a service station consisting of one queue and k_i servers.
 *  It is currently limited to networks of M/M/k queues.
 *  @param p   the routing probabilities from node to node
 *  @param r   the external arrival rates for each node
 *  @param mu  the service rates for each node
 *  @param k   the number of servers for each node
 */
class JacksonNet (p: MatrixD, r: VectorD, mu: VectorD, private var k: Array [Int] = null)
{
     /** Size of the Jackson network (number of nodes)
      */
     private val m = mu.dim

     /** Identity matrix
      */
     private val ident = new MatrixD (m, 1., 0.)

     /** Effective arrival rates at each node
      */
     private val lambda = r * (ident - p).inverse       // with routing
//   private val lambda = r                             // no routing

     if (k == null) k = Array.fill [Int] (m)(1)         // default to M/M/1 queues

     /** Utilization factor for each node
      */
     private val rho = new VectorD (m)
     for (i <- 0 until m) rho(i) = lambda(i) / (mu(i) * k(i).toDouble)

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Check intermediate results.
      */
     def check
     {
         println ("check: queueing network parameters:")
         println ("p      = " + p)                      // routing probability matrix
         println ("r      = " + r)                      // external rate vector
         println ("mu     = " + mu)                     // service rate vector
         println ("k      = " + k)                      // number of servers vector
         println ("lambda = " + lambda)                 // effective arrival rate vector
         println ("rho    = " + rho)                    // utilization factor vector
     } // check

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Calculate the probability of a node being empty, based on its utilization
      *  factor and number of servers.
      *  @param ro  the utilization factor
      *  @param kk  the number of servers
      */
     def pi_0 (ro: Double, kk: Int): Double =
     {
         val rok = ro * kk
         val sum = (for (i <- 0 until kk) yield rok~^i / fac (i)).sum
         1. / (sum + rok~^kk / (fac (kk) * (1. - ro)))
     } // pi_0

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Calculate the expected number in the queue at the j-th node.
      *  @param j  the j-th node
      */
     def nQueue (j: Int): Double =
     {
        val ro = rho(j)
        val kk = k(j)
        pi_0 (ro, kk) * kk~^kk * ro~^(kk+1) / (fac (kk) * (1. - ro)~^2)
     } // nQueue

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Calculate the expected number in the queue at the j-th node for an M/M/1 queue.
      *  @param j  the j-th node
      */
     def nQueue1 (j: Int): Double = 
     {
        val ro = rho(j)
        ro~^2 / (1. - ro)
     } // nQueue1

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Report the results.
      */
     def report
     {
         for (j <- 0 until m) {
             val nQ = nQueue (j)        // expected number waiting in the queue at node j
             val nS = rho(j) * k(j)     // expected number in service at node j
             val nT = nQ + nS           // expected number at node j
             val lamb_j = lambda(j)     // effective arrival rate at node j

             println ("\nResults for node " + j + ":")
             println ("nQ = %9.3f".format (nQ) +
                    "\twQ = %9.3f".format (nQ / lamb_j))
             println ("nS = %9.3f".format (nS) +
                    "\twS = %9.3f".format (nS / lamb_j))
             println ("nT = %9.3f".format (nT) +
                    "\twT = %9.3f".format (nT / lamb_j))
         } // for
     } // report

} // JacksonNet


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the JacksonNet class.
 */
object JacksonNetTest extends App
{
    val p  = new MatrixD ((2, 2), 0., 1.,
                                  0., 0.)
    val r  = new VectorD (5., 0.)
    val mu = new VectorD (8., 10.)

    val jqn = new JacksonNet (p, r, mu)
    jqn.check
    jqn.report

} // JacksonNetTest object

