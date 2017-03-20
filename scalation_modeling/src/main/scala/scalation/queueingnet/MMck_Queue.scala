
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Dec 29 21:28:40 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://irh.inf.unideb.hu/~jsztrik/education/16/SOR_Main_Angol.pdf
 */

package scalation.queueingnet

import scalation.math.Combinatorics.fac
import scalation.math.double_exp
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MMck_Queue` class is used to solve single node Markovian Queueing problems.
 *  It models a service station consisting of one queue, 'c' servers and a capacity
 *  to hold at most 'k' entities, i.e., an M/M/c/k queue.
 *------------------------------------------------------------------------------
 *  @see also `MMc_Queue` to model infinite capacity Markovian queues.
 *  @see also `MGc_Queue` to model queues with general service time distributions.
 *------------------------------------------------------------------------------
 *  @param lambda  the arrival rate
 *  @param mu      the service rate
 *  @param c       the number of servers
 *  @param k       the capacity of the queue
 */
class MMck_Queue (lambda: Double, mu: Double, c: Int = 1, k: Int = 1)
      extends Error
{
     if (c < 1) flaw ("constructor", "must have at least on server")
     if (k < c) flaw ("constructor", "not enough capacity")

     private val rho    = lambda / mu              // traffic intensity
     private val a      = rho / c.toDouble         // server utilization factor
     private val k_c    = k - c                    // waiting capacity
     private val c_fac  = fac (c)                  // c! (factorial)
     private val rhoc   = rho~^c / c_fac           // all servers busy probability factor
     private val _1_a   = 1.0 - a                  // one minus a
     private val pr_0   = prob_0                   // probability system is empty
     private val pr_k   = prob_k                   // probability system is full
     private val lambde = lambda * (1.0 - pr_k)    // lambda effective

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Probability system is empty
      */
     def prob_0: Double =
     {
         var sum = (for (i <- 0 until c) yield rho~^i / fac (i)).sum
         sum += ( if (a == 1) rhoc * (k_c+1) else rhoc * (1.0 - a~^(k_c+1)) / _1_a )
         1.0 / sum
     } // prob_0

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Probability system is full.
      */
     def prob_k: Double = pr_0 * rho~^k / (c~^k_c * c_fac)

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length of the waiting queue.
      */
     val l_q = pr_0 * rhoc * a * ( 1.0 - a~^(k_c+1) - _1_a * (k_c+1) * a~^k_c ) / _1_a~^2

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length/number in Service.
      */
     val l_s = lambde / mu

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length/number in sYstem.
      */
     val l_y = l_q + l_s

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected time in the waiting Queue (using Little's Law).
      */
     val t_q = l_q / lambde

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected time in Service.
      */
     val t_s = 1.0 / mu

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected time in the sYstem.
      */
     val t_y = t_q + t_s

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** View/check intermediate results.
      */
     def view ()
     {
         println ("Check queueing parameters:")
         println ("lambda = %g".format (lambda))                   // arrival rate
         println ("mu     = %g".format (mu))                       // service rate
         println ("c      = %d".format (c))                        // number of servers
         println ("k      = %d".format (k))                        // holding capacity
         println ("rho    = %g".format (rho))                      // traffic intensity
         println ("a      = %g".format (a))                        // server utilization factor
         println ("lambde = %g".format (lambde))                   // effective arrival rate
     } // view

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Report the results.
      */
     def report ()
     {
         println ("Results for queue:")
         println ("---------------------------------------------------")
         println ("|  Queue    |  l_q = %8.4g".format (l_q) + "  |  t_q = %8.4g".format (t_q) + "  |")
         println ("|  Service  |  l_s = %8.4g".format (l_s) + "  |  t_s = %8.4g".format (t_s) + "  |")
         println ("|  sYstem   |  l_y = %8.4g".format (l_y) + "  |  t_y = %8.4g".format (t_y) + "  |")
         println ("---------------------------------------------------")
     } // report

} // MMck_Queue class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MMck_QueueTest` object is used to test the `MMck_Queue` class.
 *  > run-main scalation.queueingnet.MMck_QueueTest
 */
object MMck_QueueTest extends App
{
    val lambda = 6.0                                   // customer arrival rate (per hour)
    val mu     = 7.5                                   // customer service rate (per hour)

    println("\nM/M/1/1 Queue Results:")
    val mm1_1 = new MMck_Queue (lambda, mu, 1, 1)      // M/M/c/k Queue
    mm1_1.view ()
    mm1_1.report ()

    println("\nM/M/2/4 Queue Results:")
    val mm2_4 = new MMck_Queue (lambda, mu, 2, 4)      // M/M/c/k Queue
    mm2_4.view ()
    mm2_4.report ()

} // MMck_QueueTest object

