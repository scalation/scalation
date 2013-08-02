
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Dec 12 13:11:30 EST 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.state

import math.{abs, cos, Pi, sin}

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.random.Discrete
import scalation.scala2d.{Ellipse, QArrow}
import scalation.scala2d.Colors._
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class supports the creation and use of Discrete-Time Markov Chains (DTMC).
 * Transient solution: compute the next state p' = p * tr where 'p' is the current
 * state probability vector and 'tr' is the transition probability matrix.
 * Equilibrium solution (steady-state): solve for p in p = p * tr.
 * @param tr  the transition probability matrix
 */
class Markov (tr: MatrixD) extends Error
{
    {
        if ( ! isStochastic) flaw ("constructor", "transition matrices must be stochastic")
    } // primary constructor

    /** A number close to zero
     */
    private val EPSILON = 1E-7

    /** The radius of the circle that the nodes are displayed on
     */
    private val radius = 200

    /** The x-coordinate of the center of the circle
     */
    private val xCenter = radius + 100

    /** The y-coordinate of the center of the circle
     */
    private val yCenter = radius + 100

    /** The size/diameter of a node
     */
    private val size = 30

    /** Animation flag (set to false to turn off animation)
     */
    private var animating = true

    /** The animation engine
     */
    private val dgAni = new DgAnimator ("Markov Chain Animator", black, white)

    /** The animation engine's command queue
     */
    private val aniQ = dgAni.getCommandQueue

    /** Amount of bend in the QArrow
     */
    private val bend = .25

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the kth next probabilistic state (p * tr^k).
     *  @param p  the current state probability vector
     *  @param k  compute for the kth step/epoch
     */
    def next (p: VectorD, k: Int = 1): VectorD =
    {
        var p2 = new VectorD (p)
        for (i <- 1 to k) p2 = p2 * tr
        p2
    } // next

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the limiting probabilistic state (p * tr^k) as k -> infinity, by
     *  solving a left eigenvalue problem: p = p * tr => p * (tr - I) = 0, where the
     *  eigenvalue is 1.  Solve for p by computing the left nullspace of the tr - I
     *  matrix (appropriately sliced) and then normalize p so ||p|| = 1.
     */
    def limit: VectorD =
    {
        val ident = new MatrixD (tr.dim1, 1., 0.)
        (tr - ident).t.slice (0, tr.dim1 - 1).nullspace.normalize
    } // limit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Simulate the discrete-time Markov chain, by starting in state i0 and after
     *  the state's holding, making a transition to the next state according to the
     *  jump matrix.
     *  @param i0       the initial/start state
     *  @param endTime  the end time for the simulation
     */
    def simulate (i0: Int, endTime: Int)
    {
        var clock      = 0             // current discrete time
        var i          = i0            // current state = start state
        var absorbed   = false         // whether it has entered an absorbing state
        val tk_id      = tr.dim1       // the identifier for the token
        val ms_per_sec = 1000.         // 1000 milliseconds per second (animate using seconds)

        animate ()
        aniQ += AnimateCommand (CreateToken, tk_id, Ellipse (), "tk" + tk_id, false, black, null, 0., i0)

        println ("simulate: start simulation of Discrete-Time Markov Chain at time " + clock)
        println ("simulate: at time " + clock + " the state is " + i)

        while (clock < endTime && ! absorbed) {
            if (tr(i, i) == 1.) {
                absorbed = true
                println ("simulate: entered absorbing state " + i)
            } else {
                clock    += 1
                val rowi  = tr(i)
                println ("rowi = " + rowi)
                val disRV = Discrete (rowi)
                i         = disRV.igen             // advance to the next state
            } // if
            aniQ += AnimateCommand (MoveToken2Node, tk_id, null, null, false, null, null, ms_per_sec * clock, i)
            println ("simulate: at time " + clock + " the state is " + i)
        } // while

        dgAni.animate (0, ms_per_sec * endTime)
        println ("simulate: end simulation of Discrete-Time Markov Chain at time " + clock)

    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animate this Markov Chain.  Place the nodes around a circle and connect them
     *  if there is a such a transition.
     */
    def animate ()
    {
        if (animating) {
            val n = tr.dim1    // number of nodes to create

            //:: Display the nodes for the Markov Chain

            for (i <- 0 until n) {
                val theta = -Pi + 2. * Pi * (i / n.asInstanceOf [Double])
                val shape = Ellipse ()
                val label = "n" + i
                val color = lightblue
                val at    = Array (xCenter + radius * cos (theta),
                                   yCenter + radius * sin (theta), size, size)
                println ("Markov.animate: " + label + "." + i + " " + CreateNode + " " + color +
                         " " + shape + " " + at.deep)
                aniQ += AnimateCommand (CreateNode, i, shape, label, true, color, at, 0.)
            } // for

            //:: Display the edges for the Markov Chain

            for (i <- 0 until n) {
               for (j <- 0 until n if tr(i, j) > EPSILON) {
                   val eid   = n * (i + 1) + j
                   val shape = QArrow ()
                   val label = "" + tr(i, j)
                   val color = red
                   println ("Markov.animate: " + label + "." + eid + " " + CreateEdge +
                            " " + color + " " + shape + " " + i + " " + j)
                   if (i == j) {
                       aniQ += AnimateCommand (CreateEdge, eid, shape, label, true, color,
                                               Array (16. * bend), 0., i, j)
                   } else {
                       aniQ += AnimateCommand (CreateEdge, eid, shape, label, true, color,
                                               Array (bend), 0., i, j)
                   } // if
                } // for
            } // for

        } // if
    } // animate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the transition matrix is stochastic.
     */
    def isStochastic: Boolean =
    {
        if ( ! (tr.isSquare && tr.isNonnegative)) return false
        for (row <- tr) if (abs (row.sum - 1.) > EPSILON) return false
        true
    } // isStochastic

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this discrete-time Markov Chain to s string.
     */ 
    override def toString: String = "Markov(" + tr + ")"
   
} // Markov class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object tests the Markov class (Discrete-Time Markov Chains).
 */
object MarkovTest extends App
{
    val endTime = 20   // number of epochs (milliseconds), but may represent any time unit

    val mc = new Markov (new MatrixD ((4, 4), .4, .6, .0, .0,    // 4-by-4 matrix
                                              .0, .2, .8, .0,
                                              .3, .0, .5, .2,
                                              .1, .0, .7, .2))
    var p = VectorD (1., 0., 0., 0.)

    println ("\nDiscrete-Time Markov Chain mc = " + mc + "\n")
    println ("\nDiscrete-Time Markov Chain: transient solution:")
    println ("\nON epoch 2,\tp = " + mc.next (p, 2))
    println ("\non epoch 0,\tp = " + p)
    for (k <- 1 to endTime) {
        p = mc.next (p)
        println ("on epoch " + k + ",\tp = " + p)
    } // for

    println ("\nDiscrete-Time Markov Chain: steady-state solution:")
    println ("\nsteady-state \tp = " + mc.limit)

    println ("\nDiscrete-Time Markov Chain mc = " + mc + "\n")
    println ("\nDiscrete-Time Markov Chain: simulation:")
    mc.simulate (0, endTime)
   
} // MarkovTest object

