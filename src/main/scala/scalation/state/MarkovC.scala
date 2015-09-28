
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Dec 12 13:11:30 EST 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.state

import math.{abs, cos, Pi, sin}

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.double_exp
import scalation.random.{Discrete, Exponential}
import scalation.scala2d.{Ellipse, QArrow}
import scalation.scala2d.Colors._
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class supports the creation and use of Continuous-Time Markov Chains
 *  (CTMC).  Note: the transition matrix tr gives the state transition rates
 *  off-diagonal.  The diagonal elements must equal minus the sum of the rest
 *  of their row.  Transient solution: Solve the Chapman-Kolmogorov differemtial
 *  equations.  Equilibrium solution (steady-state): solve for p in p * tr = 0.
 *  See: www.math.wustl.edu/~feres/Math450Lect05.pdf
 *  @param tr  the transition rate matrix
 */
class MarkovC (tr: MatrixD) extends Error
{
    /** A number close to zero
     */
    private val EPSILON = 1E-7

    /** The jump matrix derived from the transition rate matrix (tr)
     */
    val jump = new MatrixD (tr.dim1, tr.dim2)
   
    {
        if ( ! tr.isSquare) flaw ("constructor", "transition rate matrices must be square")
        for (i <- 0 until jump.dim1) {
            val s = tr(i).sumNE (i)                     // sum the ith row of tr skipping i
            for (j <- 0 until jump.dim2) {
                if (i != j) {                                               // off-diagonal
                    jump(i, j) = if (s =~ 0.0) 0.0 else tr(i, j) / s
                } else {                                                    // on-diagonal
                    jump(i, i) = if (s =~ 0.0) 1.0 else 0.0
                } // if
            } // for
        } // for
    } // primary constructor

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
    private val dgAni = new DgAnimator ("Continuous-Time Markov Chain Animator", black, white)

    /** The animation engine's command queue
     */
    private val aniQ = dgAni.getCommandQueue

    /** Amount of bend in the QArrow
     */
    private val bend = .25

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the next probabilistic state at t time units in the future.
     *  @param p  the current state probability vector
     *  @param t  compute for time t
     */
    def next (p: VectorD, t: Double = 1.0): VectorD =
    {
        null     // FIX, not implemented yet
    } // next

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the limiting probabilistic state as t -> infinity, by finding the
     *  left nullspace of the tr matrix: solve for p such that p * tr = 0 and
     *  normalize p, i.e.0, ||p|| = 1.
     */
    def limit: VectorD =
    {
        tr.t.slice (0, tr.dim1 - 1).nullspace.normalize
    } // limit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Simulate the continuous-time Markov chain, by starting in state i0 and after
     *  the state's holding, making a transition to the next state according to the
     *  jump matrix.
     *  @param i0       the initial/start state
     *  @param endTime  the end time for the simulation
     */
    def simulate (i0: Int, endTime: Double)
    {
        var clock      = 0.0              // current continuous time
        var i          = i0               // current state = start state
        var absorbed   = false            // whether it has entered an absorbing state
        val tk_id      = tr.dim1          // the identifier for the token
        val ms_per_sec = 1000.0           // 1000 milliseconds per second (animate using seconds)

        animate ()
        aniQ.add (AnimateCommand (CreateToken, tk_id, Ellipse (), "tk" + tk_id, false, black, null, 0.0, i0))

        println ("simulate: start simulation of Continuous-Time Markov Chain at time " + clock)
        println ("simulate: at time " + clock + " the state is " + i)

        while (clock < endTime && ! absorbed) {
            val tr_i = - tr(i, i)                  // holding rate for state i
            if (tr_i =~ 0.0) {
                absorbed = true
                println ("simulate: entered absorbing state " + i)
            } else {
                val expRV = Exponential (tr_i)
                clock    += expRV.gen              // add holding time for state i
                val rowi  = jump(i)
                println ("rowi = " + rowi)
                val disRV = Discrete (rowi)
                i         = disRV.igen             // advance to the next state
            } // if
            aniQ.add (AnimateCommand (MoveToken2Node, tk_id, null, null, false, null, null, ms_per_sec * clock, i))
            println ("simulate: at time " + clock + " the state is " + i)
        } // while

        dgAni.animate (0, ms_per_sec * endTime)
        println ("simulate: end simulation of Continuous-Time Markov Chain at time " + clock)

    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animate this continuous-time Markov Chain.  Place the nodes around a circle
     *  and connect them if there is a such a transition.
     */
    def animate ()
    {
        if (animating) {
            val n = tr.dim1    // number of nodes to create

            //:: Display the nodes for the continuous-time Markov Chain

            for (i <- 0 until n) {
                val theta = -Pi + 2.0 * Pi * (i / n.asInstanceOf [Double])
                val shape = Ellipse ()
                val label = "n" + i
                val color = lightblue
                val at    = Array (xCenter + radius * cos (theta),
                                   yCenter + radius * sin (theta), size, size)
                println ("MarkovC.animate: " + label + "." + i + " " + CreateNode + " " + color +
                         " " + shape + " " + at.deep)
                aniQ.add (AnimateCommand (CreateNode, i, shape, label, true, color, at, 0.0))
            } // for

            //:: Display the edges for the continuous-time Markov Chain

            for (i <- 0 until n) {
               for (j <- 0 until n if i != j && tr(i, j) > EPSILON) {
                   val eid   = n * (i + 1) + j
                   val shape = QArrow ()
                   val label = "" + tr(i, j)
                   val color = red
                   println ("MarkovC.animate: " + label + "." + eid + " " + CreateEdge +
                            " " + color + " " + shape + " " + i + " " + j)
                   aniQ.add (AnimateCommand (CreateEdge, eid, shape, label, true, color,
                                             Array (bend), 0.0, i, j))
                } // for
            } // for

        } // if
    } // animate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this continuous-time Markov Chain to s string.
     */
    override def toString: String = "MarkovC(" + tr + ")"

} // MarkovC class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object tests the MarkovC class (Continuous-Time Markov Chains).
 */
object MarkovCTest extends App
{
    val endTime = 200.0       // number of time units (e.g.0, milliseconds)

    val mc = new MarkovC (new MatrixD ((2, 2), -4.0,  4.0,        // 2-by-2 matrix
                                                5.0, -5.0))

    println ("\nContinuous-Time Markov Chain mc = " + mc + "\n")
    println ("\nContinuous-Time Markov Chain: transient solution:")
//  println ("\nAT STEP 1,\tp = " + mc.next (p, 1))

    println ("\nContinuous-Time Markov Chain: steady-state solution:")
    println ("\njump matrix  \tj = " + mc.jump)
    println ("\nsteady-state \tp = " + mc.limit)

    val mc2 = new MarkovC (new MatrixD ((6, 6), -2.0, 1.0,  0.0,  1.0,  0.0, 0.0,   // 6-by-6 matrix
                                                 0.0, 0.0,  0.0,  0.0,  0.0, 0.0,
                                                 0.0, 1.0, -4.0,  0.0,  0.0, 3.0,
                                                 2.0, 0.0,  0.0, -4.0,  2.0, 0.0,
                                                 0.0, 3.0,  1.0,  0.0, -5.0, 1.0,
                                                 0.0, 0.0,  0.0,  0.0,  0.0, 0.0))

    println ("\nContinuous-Time Markov Chain mc2 = " + mc2 + "\n")
    println ("\nContinuous-Time Markov Chain: simulation:")
    mc2.simulate (0, endTime)

} // MarkovCTest object

