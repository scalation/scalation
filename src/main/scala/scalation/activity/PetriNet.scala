
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Oct  16 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.activity

import java.util.concurrent.ConcurrentLinkedQueue

import collection.mutable.PriorityQueue

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.dynamics.Derivatives.Derivative
import scalation.linalgebra.{VectorD, VectorI}
import scalation.random.{Uniform, Variate}
import scalation.scala2d.{Ellipse, QCurve, Rectangle}
import scalation.scala2d.Colors._
import scalation.util.{Error, Identifiable, PQItem}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PlaceI` class represents a discrete place (can hold tokens).
 *  @param x       the place's x-coordinate
 *  @param y       the place's y-coordinate
 *  @param tokens  the number of tokens per color
 *  @param stays   whether the tokens stay (test arc)
 */
class PlaceI (val x: Double, val y: Double, var tokens: VectorI, stays: Boolean = false)
      extends Identifiable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add tokens to 'this' discrete place.
     *  @param _token  the token vector to add
     */
    def add (_tokens: VectorI) { tokens += _tokens }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take tokens from 'this' discrete place.
     *  @param _token  the token vector to take away
     */
    def take (_tokens: VectorI) { tokens -= _tokens }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' place holds at least the token vector (i.e.,
     *  the requisite number of tokens of each color). Alternative: use
     *  threshold predicate in `PetriNetRules`.
     *  @param _token  the token vector
     */
    def holds (_tokens: VectorI): Boolean = tokens >= _tokens

} // PlaceI class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PlaceD` class represents a continuous place (can hold fluids).
 *  @param x       the place's x-coordinate
 *  @param y       the place's y-coordinate
 *  @param fluids  the amount of fluid per color
 *  @param stays   whether the fluids stay (test arc)
 */
class PlaceD (val x: Double, val y: Double, var fluids: VectorD, stays: Boolean = false)
      extends Identifiable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add fluids to 'this' continuous place.
     *  @param _fluids  the fluid vector to add
     */
    def add (_fluids: VectorD) { fluids += _fluids }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take fluids from 'this' continuous place.
     *  @param _fluids  the fluid vector to take away
     */
    def take (_fluids: VectorD) { fluids -= _fluids }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' place holds at least the fluid vector (i.e.,
     *  the requisite amount of fluid of each color). Alternative: use
     *  threshold predicate in `PetriNetRules`.
     *  @param _fluids  the fluid vector
     */
    def holds (_fluids: VectorD): Boolean = fluids >= _fluids

} // PlaceD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Transition` class represents a timed transition.
 *  @param x           the x-coordinate for 'this' transition
 *  @param y           the y-coordinate for 'this' transition
 *  @param firingDist  the random variate for the firing distribution
 *  @param colors      the colors of the tokens (needed for firing rules)
 */
class Transition (val x: Double, val y: Double, firingDist: Variate, colors: Array [Color])
      extends PQItem with Ordered [Transition] with PetriNetRules with Identifiable
{
    /** The containing Petri net
     */
    var pnet: PetriNet = null

    /** The animation command queue
     */
    var cqueue: ConcurrentLinkedQueue [AnimateCommand] = null

    /** Arcs incoming from discrete places
     */
    var inI: Array [ArcI] = null

    /** Arcs incoming from continuous places
     */
    var inD: Array [ArcD] = null

    /** Arcs outgoing to discrete places
     */
    var outI: Array [ArcI] = null

    /** Arcs outgoing to continuous places
     */
    var outD: Array [ArcD] = null

    /** The firing delay for 'this' transition
     */
    var firingDelay: Double = 0.0

    /** Token vector for transition
     */
    var tokens: VectorI = new VectorI (colors.length)

    /** Fluid vector for transition
     */
    var fluids: VectorD = new VectorD (colors.length)

    /** A transition is locked from the time it is enabled until it fires
     */
    var locked: Boolean = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Connect 'this' transition to all the incoming and outgoing discrete arcs
     *  as well as the containing Petri net.
     *  @param _pnet  the containing Petri net
     *  @param _in    the incoming arcs from discrete/`Int` places
     *  @param _out   the outgoing arcs to discrete/`Int` places
     */
    def connect (_pnet: PetriNet, _in: Array [ArcI], _out: Array [ArcI])
    {
        pnet   = _pnet
        cqueue = pnet.getCommandQueue
        inI    = _in
        inD    = Array [ArcD] ()
        outI   = _out
        outD   = Array [ArcD] ()
    } // connect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Connect 'this' transition to all the incoming and outgoing continuous arcs
     *  as well as the containing Petri net.
     *  @param _pnet  the containing Petri net
     *  @param _in    the incoming arcs from continuous/`Double` places
     *  @param _out   the outgoing arcs to continuous/`Double` places
     */
    def connect (_pnet: PetriNet, _in: Array [ArcD], _out: Array [ArcD])
    {
        pnet   = _pnet
        cqueue = pnet.getCommandQueue
        inI    = Array [ArcI] ()
        inD    = _in
        outI   = Array [ArcI] ()
        outD   = _out
    } // connect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Connect 'this' transition to all the incoming and outgoing arcs as well as
     *  the containing Petri net.
     *  @param _pnet  the containing Petri net
     *  @param _inI   the incoming arcs from discrete/`Int` places
     *  @param _inD   the incoming arcs from continuous/`Double` places
     *  @param _outI  the outgoing arcs to discrete/`Int` places
     *  @param _outD  the outgoing arcs to continuous/`Double` places
     */
    def connect (_pnet: PetriNet, _inI: Array [ArcI], _inD: Array [ArcD], _outI: Array [ArcI], _outD: Array [ArcD])
    {
        pnet   = _pnet
        cqueue = pnet.getCommandQueue
        inI    = _inI
        inD    = _inD
        outI   = _outI
        outD   = _outD
    } // connect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add tokens to 'this' transition.
     *  @param _token  the token vector to add
     */
    def addTokens (_tokens: VectorI) { tokens += _tokens }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take tokens from 'this' transition.
     *  @param _token  the token vector to take away
     */
    def takeTokens (_tokens: VectorI) { tokens -= _tokens }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add fluids to 'this' transition.
     *  @param _fluids  the fluid vector to add
     */
    def addFluids (_fluids: VectorD) { fluids += _fluids }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take fluids from 'this' transition.
     *  @param _fluids  the fluid vector to take away
     */
    def takeFluids (_fluids: VectorD) { fluids -= _fluids }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the incoming arcs from discrete place for enough tokens of the
     *  right colors and the incoming arcs from continuous places for enough
     *  fluid of the right colors.
     */
    def checkGuard: Boolean =
    {
        if (locked) return false
        for (iI <- inI if ! iI.place.holds (iI.minTokens)) return false
        for (iD <- inD if ! iD.place.holds (iD.minFluids)) return false
        true
    } // checkGuard

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Enable 'this' transition by computing the firing delay.  Should immediately
     *  place it on the time ordered firing list.  Also, move tokens/fluids from
     *  input places to 'this' transition.
     */
    def enable (): Double =
    {
        locked = true    // this transition is now in progress, so it's locked

        //:: Calculate the firing delay = firing-time - enablement-time

        firingDelay = calcFiringDelay (firingDist, null, null, null, null)
        println ("Transition.enable: firingDelay = " + firingDelay)

        //:: Pull tokens from incoming discrete places (move to transition).

        for (iI <- inI) {                                          // for each incoming discrete arc
            val place   = iI.place                                 // discrete place token source
            val _tokens = iI._tokenFlow (place.tokens, pnet.clock, firingDelay)  // how many tokens
            place.take (_tokens)                                   // take tokens from place
            addTokens (_tokens)                                    // add these tokens to transition

            println ("Transition.enable: move " + _tokens + " tokens to transition " + id + " at " + pnet.clock)

            //:: Move these tokens from their discrete place (place.id) to the transition (this.id).
            //:: For each color, move number = 'tokens(i)' tokens

            for (i <- 0 until _tokens.dim) {
                val number = _tokens(i)
                if (number > 0) {
                    cqueue.add (AnimateCommand (MoveTokens2Node, -1, null, null, false,
                                                colors(i), Array (number), pnet.clock, place.id, id))
                } // if
            } // for
        } // for

        //:: Pull fluids from incoming continuous places (move to transition).

        for (iD <- inD) {                                          // for each incoming continuous arc
            val place   = iD.place                                 // continuous place fluid source
            val _fluids = iD._fluidFlow (place.fluids, pnet.clock, firingDelay)  // how much fluid
            place.take (_fluids)                                   // take fluids from place
            addFluids (_fluids)                                    // add these fluids to transition

            println ("Transition.enable: move " + _fluids + " fluids to transition " + id + " at " + pnet.clock)

            //:: Move these fluids from their continuous place (place.id) to the transition (this.id).
            //:: For each color, move amount = 'fluids(i)' fluids

            for (i <- 0 until _fluids.dim) {
                val amount = _fluids(i)
                if (amount > 0) {
                    // Adjust the sizes of tokens at both nodes by the amount
                    cqueue.add (AnimateCommand (ScaleTokensAt, -1, null, null, false,
                                                colors(i), Array (amount), pnet.clock, place.id, id))
                } // if
            } // for
        } // for

        firingDelay
    } // enable

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fire 'this' transition by moving the requisite number and color of tokens
     *  from 'this' transition to each outgoing discrete place and the requisite
     *  amount and color of fluid to each outgoing continuous place.
     */
    def fire ()
    {
        //:: Push tokens to outgoing discrete places (move to outgoing place).

        for (oI <- outI) {                                                 // for each outgoing discrete arc
            val place   = oI.place                                         // discrete place token target
            val _tokens = oI._tokenFlow (tokens, pnet.clock, firingDelay)  // how many tokens to move
            takeTokens (_tokens)                                           // take tokens from transition
            place.add (_tokens)                                            // add these tokens to place

            println ("Transition.fire: move " + _tokens + " tokens to place " + place.id + " at " + pnet.clock)

            //:: Move these tokens from the transition (this.id) to their discrete place (place.id)
            //:: For each color, move number = 'tokens(i)' tokens

            for (i <- 0 until _tokens.dim) {
                val number = _tokens(i)
                if (number > 0) {
                    cqueue.add (AnimateCommand (MoveTokens2Node, -1, null, null, false,
                                                colors(i), Array (number), pnet.clock, id, place.id))
                } // if
            } // for
        } // for

        //:: Push fluids to outgoing continuous places (move to outgoing place).

        for (oD <- outD) {                                                 // for each outgoing continuous arc
            val place   = oD.place                                         // continuous place token target
            val _fluids = oD._fluidFlow (fluids, pnet.clock, firingDelay)  // how much fluid to move
            takeFluids (_fluids)                                           // take fluids from transition
            place.add (_fluids)                                            // add these fluids to place

            println ("Transition.fire: move " + _fluids + " fluids to place " + place.id + " at " + pnet.clock)

            //:: Move these fluids from the transition (this.id) to their continuous place (place.id)
            //:: For each color, move amount = 'fluids(i)' tokens

            for (i <- 0 until _fluids.dim) {
                val amount = _fluids(i)
                if (amount > 0) {
                    // Adjust the sizes of tokens at both nodes by the amount
                    cqueue.add (AnimateCommand (ScaleTokensAt, -1, null, null, false,
                                                colors(i), Array (amount), pnet.clock, id, place.id))
                } // if
            } // for
        } // for

        locked = false     // this transition is now in complete, so it's unlocked
    } // fire

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' transition to 'tr2' based on firing time.
     * @param tr2  the other transition
     */
    def compare (tr2: Transition) = actTime compare tr2.actTime

} // Transition class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ArcI` class represents an arc connecting discrete place with a
 *  transition. If incoming is true the arc is from the place to transition,
 *  otherwise it is from the transition to the place (outgoing).
 *  @param place        the discrete place at one end of the arc
 *  @param transition   the transition at the other end of the arc
 *  @param incoming     whether the arc goes into a transition
 *  @param minTokens    minimum number of tokens to transport over the arc
 *  @param rates        the rate vector for the linear flow model
 *  @param testArc      whether the arc is a test arc meaning the tokens/fluids stay
 *  @param scaleFactor  the scale factor for the firing delay
 */
class ArcI (val place: PlaceI, val transition: Transition, incoming: Boolean, val minTokens: VectorI,
            rates: VectorI = null, testArc: Boolean = false, scaleFactor: Double = 1.0)
      extends PetriNetRules with Identifiable
{
    {
        if (place == null)          flaw ("constructor", "discrete place must not be null")
        if (transition == null)     flaw ("constructor", "transition must not be null")
        if ( ! incoming && testArc) flaw ("constructor", "test arcs must be incoming")
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the number of tokens of each color to flow over 'this' arc.
     *  @param tokens       the number of tokens available
     *  @param time         the current time
     *  @param firingDelay  the time it takes for the transition to fire
     */
    def _tokenFlow (tokens: VectorI, time: Double, firingDelay: Double): VectorI =
    {
        tokenFlow (tokens, minTokens, rates, firingDelay / scaleFactor)
    } // _tokenFlow

} // ArcI class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ArcD` class represents an arc connecting continuous place with a
 *  transition. If incoming is true the arc is from the place to transition,
 *  otherwise it is from the transition to the place (outgoing).
 *  @param place        the continuous place at one end of the arc
 *  @param transition   the transition the other end of the arc
 *  @param incoming     whether the arc goes into a transition
 *  @param minFluids    minimum amount of fluid to transport over the arc
 *  @param rates        the rate vector for the linear flow model
 *  @param derv         the array of derivative functions for ODE's
 *  @param testArc      whether the arc is a test arc meaning the tokens/fluids stay
 *  @param scaleFactor  the scale factor for the firing delay
 */
class ArcD (val place: PlaceD, val transition: Transition, incoming: Boolean, val minFluids: VectorD,
            rates: VectorD = null, derv: Array [Derivative] = null, testArc: Boolean = false,
            scaleFactor: Double = 1.0)
      extends PetriNetRules with Identifiable
{
    {
        if (place == null) flaw ("constructor", "continuous place must not be null")
        if (transition == null) flaw ("constructor", "transition must not be null")
        if ( ! incoming && testArc) flaw ("constructor", "test arcs must be incoming")
        if ( ! incoming && derv != null) flaw ("constructor", "only incoming arcs may have ODE's")
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the amount of fluid of each color to flow over 'this' arc.
     *  @param fluids       the amount of fluid available
     *  @param time         the current time
     *  @param firingDelay  the time it takes for the transition to fire
     */
    def _fluidFlow(fluids: VectorD, time: Double, firingDelay: Double): VectorD =
    {
        if (derv == null) {                // use a linear or constant flow model
            fluidFlow (fluids, minFluids, rates, firingDelay / scaleFactor)
        } else {                           // use an ODE based flow model
            fluidFlow (fluids, derv, time, firingDelay / scaleFactor)
        } // if
    } // _fluidFlow

} // ArcD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PetriNet` class provides a simulation engine for Hybrid Colored Petri Nets.
 *  Reference: "Discrete-event simulation of fluid stochastic Petri Nets"
 *  @param colors      array of colors for tokens/fluids
 *  @param placeI      array of discrete places
 *  @param placeD      array of continuous places
 *  @param transition  array of timed transitions
 */
class PetriNet (colors: Array [Color], placeI: Array [PlaceI], placeD: Array [PlaceD],
                transition: Array [Transition])
      extends PetriNetRules with Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a discrete Petri net (tokens, but no fluids).
     *  @param colors      array of colors for tokens
     *  @param placeI      array of discrete places
     *  @param transition  array of timed transitions
     */
    def this (colors: Array [Color], placeI: Array [PlaceI], transition: Array [Transition])
    {
        this (colors, placeI, Array [PlaceD] (), transition)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a continuous Petri net (fluids, but no tokens).
     *  @param colors      array of colors for fluids
     *  @param placeD      array of continuous places
     *  @param transition  array of timed transitions
     */
    def this (colors: Array [Color], placeD: Array [PlaceD], transition: Array [Transition])
    {
        this (colors, Array [PlaceI] (), placeD, transition)
    } // constructor

    /** The current time
     */
    private var _clock = 0.0

    /** The Petri net directed graph animator
     */
    private val pna = new DgAnimator ("PetriNetAnimator", white, black)

    /** The animation command queue
     */
    private val cqueue = pna.getCommandQueue

    /** Number of colors (need at least 1)
     */
    private val ncolors = colors.length

    /** Number of discrete places (which hold entities)
     */
    private val ndplaces = if (placeI == null) 0 else placeI.length

    /** Number of continuous places (which hold fluids)
     */
    private val ncplaces = if (placeD == null) 0 else placeD.length

    /** Number of timed transitions (need at least 1)
     */
    private val ntransitions = transition.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the current time.
     */
    def clock = _clock

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the animation command queue.
     */
    def getCommandQueue = cqueue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the Petri net to the string representation.
     */
    override def toString =
    {
        var s = "PetriNet (\n"
        println ("placeI = " + placeI)
        for (pI <- placeI) s += "\tPlaceI [ " + pI.id + " ]\n"
        println ("placeD = " + placeD)
        for (pD <- placeD) s += "\tPlaceD [ " + pD.id + " ]\n"
        for (tr <- transition) {
            s += " \tTransition [ " + tr.id + " ]\n"
            for (aI <- tr.inI)  s += " \t\tArcI [ " + aI.place.id + " , " + aI.transition.id + " ]\n"
            for (aD <- tr.inD)  s += " \t\tArcD [ " + aD.place.id + " , " + aD.transition.id + " ]\n"
            for (aI <- tr.outI) s += " \t\tArcI [ " + aI.transition.id + " , " + aI.place.id + " ]\n"
            for (aD <- tr.outD) s += " \t\tArcD [ " + aD.transition.id + " , " + aD.place.id + " ]\n"
        } // for
        s += ")"
        s
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize the animation by drawing the Petri net components onto the
     *  animation drawing panel using animation commands.
     *  @param timeDilationFactor  time dilation is used to speed up/slow down animation
     *  @param gColors             the colors for nodes and edges in the graph
     *                             i.e., discrete-places, continuous-places, transitions and arcs
     */
    def initAnimation (gColors: Array [Color] = Array (yellow, gold, silver, lightyellow),
                       timeDilationFactor: Double = 1000.0)
    {
        println ("PetriNet.initAnimation: begin drawing the Petri net graph")

        //:: Draw the discrete places along with their initial tokens.

        cqueue.add (AnimateCommand (TimeDilation, -1, null, null, true, null,
                                    Array [Double] (timeDilationFactor), 0.0))

        for (pI <- placeI) {
            cqueue.add (AnimateCommand (CreateNode, pI.id, Ellipse (), "pI" + pI.id, false, gColors(0),
                                        Array [Double] (pI.x, pI.y, 30, 30), 0))

            val tokens = pI.tokens
            for (i <- 0 until tokens.dim) {          // number of tokens by color at this place
                for (j <- 0 until tokens(i)) {
                    val tk_id = Counter.next ()
                    println ("PetriNet.initAnimation: token " + tk_id + " for place " + pI.id)
                    cqueue.add (AnimateCommand (CreateToken, tk_id, Ellipse (), "tk" + tk_id, false,
                                                colors(i), null, 0, pI.id))
                } // for
            } // for
        } // for

        //:: Draw the continuous places along with their initial fluid levels.

        for (pD <- placeD) {
            cqueue.add (AnimateCommand (CreateNode, pD.id, Ellipse (), "pD" + pD.id, false, gColors(1),
                                        Array [Double] (pD.x, pD.y, 30, 40), 0))

            val fluids = pD.fluids
            for (i <- 0 until fluids.dim) {          // amount of fluids by color at this place
                val fl_id = Counter.next ()
                val amount = fluids(i)
                if (amount > 0) {
                    println ("PetriNet.initAnimation: fluid " + fl_id + " with amount " + amount + " for place " + pD.id)
                    cqueue.add (AnimateCommand (CreateToken, fl_id, Ellipse (), "fl" + fl_id, false, colors(i),
                                                Array [Double] (amount, amount), 0, pD.id))
                } // if
            } // for
        } // for

        //:: Draw the transitions along with their incoming and outgoing arcs.

        for (tr <- transition) {
            cqueue.add (AnimateCommand (CreateNode, tr.id, Rectangle (), "tr" + tr.id, true, gColors(2),
                                        Array [Double] (tr.x, tr.y, 30, 60), 0))
            for (aI <- tr.inI) {
                cqueue.add (AnimateCommand (CreateEdge, aI.id, QCurve (), "aI" + aI.id, true, gColors(3),
                                            null, 0, aI.place.id, aI.transition.id))
            } // for
            for (aD <- tr.inD) {
                cqueue.add (AnimateCommand (CreateEdge, aD.id, QCurve (), "aD" + aD.id, true, gColors(3),
                            null, 0, aD.place.id, aD.transition.id))
            } // for
            for (aI <- tr.outI) {
                cqueue.add (AnimateCommand (CreateEdge, aI.id, QCurve (), "aI" + aI.id, true, gColors(3),
                                            null, 0, aI.transition.id, aI.place.id))
            } // for
            for (aD <- tr.outD) {
                cqueue.add (AnimateCommand (CreateEdge, aD.id, QCurve (), "aD" + aD.id, true, gColors(3),
                                            null, 0, aD.transition.id, aD.place.id))
            } // for
        } // for

        println ("PetriNet.initAnimation: end drawing the Petri net graph")
    } // initAnimation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Simulate the execution of the Petri Net.
     *  @param tStart  the starting time for the simulation
     *  @param tStop   the stopping time for the simulation
     */
    def simulate (tStart: Double, tStop: Double)
    {
        // The list of transitions to be fired (time-ordered list of transitions)
        val firingList = PriorityQueue.empty [Transition]
//      val firingList = new PQueue [Transition] ()

        _clock       = tStart
        var continue = true

        println ("PetriNet.simulate: initialize animation of the Petri net at " + 0.0)
        initAnimation ()

        println ("PetriNet.simulate: start simulation at " + _clock)

        while (_clock < tStop && continue) {

            //:: Enable other transitions whose guards are true.

            for (tran <- transition) {
                println ("PetriNet.simulate: check guard for transition " + tran.id + " at " + _clock)
                if (tran.checkGuard) {
                    println ("PetriNet.simulate: enable transition " + tran.id + " at " + _clock)
                    tran.actTime = _clock + tran.enable ()   // enable returns firing delay
                    firingList += tran
                } // if
            } // for

            continue = ! firingList.isEmpty

            //:: Fire the next (in time order) enabled transition.

            if (continue) {
                val nextTran = firingList.dequeue ()             // remove from firing list
                _clock = nextTran.actTime                        // advance time
                println ("PetriNet.simulate: fire transition " + nextTran.id + " at " + _clock)
                nextTran.fire ()                                 // fire the next transition
            } // if
        } // while

        println ("PetriNet.simulate: stop simulation at " + _clock +
                 " with firing list = " + firingList)

        println ("PetriNet.simulate: start animation")
        pna.animate (0.0, tStop)
    } // simulate


} // PetriNet class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Counter` object is used to provide unique identifiers for tokens/fluids.
 */
object Counter
{
    private var count = 1000    // nodes (places, transitions), edges <= 1000,
                                // tokens/fluids > 1000

    def next (): Int = { count += 1; count }

} // Counter object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PetriNetTest` object is used to test the `PetriNet` class.
 */
object PetriNetTest extends App
{
    //:: Set up the colors for tokens and fluids (note: colors for tokens and fluids must be disjoint).

    val colors = Array [Color] (green, blue, purple)

    //:: Define the places along with their initial markings by color.

    val placeI = Array [PlaceI] (new PlaceI (100, 100, VectorI (2, 2, 0)),
                                 new PlaceI (500, 100, VectorI (0, 0, 0)))

    val placeD = Array [PlaceD] (new PlaceD (100, 400, VectorD (0.0, 0.0, 10.5)),
                                 new PlaceD (500, 400, VectorD (0.0, 0.0,  0.0)))

    //:: Define the transitions.

    val transt = Array [Transition] (new Transition (300, 250, new Uniform (4, 6), colors))

    //:: Define the overall Petri net.

    val pnet = new PetriNet (colors, placeI, placeD, transt)

    //:: For each transition, link to all of the incoming/outgoing places via true/false arcs.
    //:: Also, establish a back link to the containing Petri net.

    transt(0).connect (pnet,
        Array [ArcI] (new ArcI (placeI(0), transt(0), true,  VectorI (1, 1, 0))),
        Array [ArcD] (new ArcD (placeD(0), transt(0), true,  VectorD (0.0, 0.0, 5.5))),
        Array [ArcI] (new ArcI (placeI(1), transt(0), false, VectorI (1, 1, 0))),
        Array [ArcD] (new ArcD (placeD(1), transt(0), false, VectorD (0.0, 0.0, 5.5))))

    println (pnet)
    pnet.simulate (2, 10)

} // PetriNetTest object

