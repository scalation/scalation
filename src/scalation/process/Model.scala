
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import actors.Actor
import collection.mutable.{ListBuffer, PriorityQueue}

import scalation.model.ModelT
import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.random.{Uniform, Variate}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.Shape
import scalation.stat.Statistic
import scalation.util.{Identity, Monitor}
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Model class maintains a list of components making up the model and
 *  controls the flow of entities (SimActors) through the model, following the
 *  process-interaction world-view.  It maintains a time-ordered priority queue
 *  to activate/re-activate each of the entities.  Each entity (SimActor) is
 *  implemeneted as a Scala Actor and therefore runs in its own thread. 
 *  @param name       the name of the model
 *  @param animation  whether to animate the model
 */
class Model (name: String, animation: Boolean = true)
      extends Actor with Signals with ModelT with Component
{
    initComponent (name, Array ())

    private val DEBUG = true

    /** The agenda of things to be done (time-ordered activation list)
     */
    private val agenda = PriorityQueue.empty [SimActor]
//  private val agenda = new PQueue [SimActor] ()

    /** List of Components making up the model
     */
    private var parts: List [Component] = null

    /** The currently acting actor (act one at a time)
     */
    private var _theActor: SimActor = null

    /** The animation engine
     */
    private val dgAni = if (animation) new DgAnimator ("Process Animator", black, white)
                        else null

    /** The animation engine's command queue
     */
    private val aniQ = if (animation) dgAni.getCommandQueue
                       else null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the component parts to the model.
     *  @param _parts  the component parts
     */
    def addComponents (_parts: List [Component])
    {
        parts = _parts
    } // addComponents

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current acting actor.
     */
    def theActor = _theActor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the order of actors based on their actTime.
     *  @param actor1  the first actor in comparison
     */
    def orderedActor (actor1: SimActor): Ordered [SimActor] =
    {
        new Ordered [SimActor]
            { def compare (actor2: SimActor) = actor1.actTime compare actor2.actTime }
    } // orderedActor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the simulation (includes scheduling all Sources) returning summary
     *  statistics.
     *  @param startTime the time at which the simulation is to begin
     */
    def simulate (startTime: Double = 0.): ListBuffer [Statistic] =
    {
        _clock = startTime
        trace (this, "starts", this, _clock)
        for (p <- parts) {
            trace (this, "establish x = " + p.at(0) + " y = " + p.at(1), p, _clock)
            p.setDirector (this)
            if (p.isInstanceOf [Source]) reschedule (p.asInstanceOf [Source]) 
        } // for

        if (animation) display ()             // turn animation on (true) off (false)
        simulating = true
        start ()                              // start the director thread/actor
        val future = this !! RETURN_RESULTS   // results returned in a future reply
        val results = future ()
        println ("<<<<<<<<<<<<<<<<<<<<<<< future returned - simulation finished >>>>>>>>>>>>>>>>>>>>>>")
        report
        getStatistics
    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Schedule (first time) or reschedule (subsequent times) an actor to act.
     *  @param actor  the actor to be scheduled
     */
    def reschedule (actor: SimActor) { agenda += actor }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The model itself is an Actor (not an ordinary SimActor) and may be
     *  thought of as the director.  The director iteratively manages the clock
     *  and the agenda of actors until the simulation flag becomes false
     *  or the agenda becomes empty.
     */
    def act ()
    {
        trace (this, "starts", this, _clock)

        while (simulating && ! agenda.isEmpty) {
            _theActor = agenda.dequeue ()
            _clock    = _theActor.actTime
            trace (this, "resumes", _theActor, _clock)
            if (_theActor.yetToAct) {
                _theActor.nowActing ()
                _theActor.start ()
            } else {
                _theActor ! RESUME_ACTOR
            } // if
            receive { case RESUME_DIRECTOR => trace (this, "receives " + RESUME_DIRECTOR, this, _clock) }
        } // while

        if (animation) dgAni.animate (0, 100000)
        trace (this, "terminates", this, _clock)
        receive { case RETURN_RESULTS => trace (this, "receives " + RETURN_RESULTS, this, _clock)
                                         reply (RETURN_RESULTS)
        } // receive
    } // act

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation.
     */
    def report
    {
        println ("____________________________________________________________________________________")
        println ("statistics:\t" + durationStat.labels ())
        for (p <- parts) {
            val space = if (p.name.length > 6) ":\t" else ":\t\t"
            println (p.name + space + p.durationStat) 
        } // for
        println ("____________________________________________________________________________________")
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the statistical results of the simulation (statistics for each part).
     */
    def getStatistics: ListBuffer [Statistic] =
    {
        val stat = new ListBuffer [Statistic] ()
        for (p <- parts) stat += p.durationStat
        stat       
    } // getStatistics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the components on the animation engine's queue.
     */
    def display ()
    {
        animating = true
        for (p <- parts) p.display ()
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put a node/token command on the animation queue.
     *  @param who    who is being animated
     *  @param what   what animation command
     *  @param color  the color the node/token
     *  @param shape  the shape of the node/token
     *  @param at     the location of the node/token
     */
    def animate (who: Identity, what: Value, color: Color,
                 shape: Shape, at: Array [Double])
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            if (DEBUG) {
                println (">< Model.animate: " + label + "." + eid + " " + what + " " + color +
                         " " + shape + " " + at.deep)
            } // if
            aniQ += AnimateCommand (what, eid, shape, label, true, color, at, _clock)
        } // if
    } // animate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put a edge command on the animation queue.
     *  @param who    who is being animated
     *  @param what   what animation command
     *  @param color  the color the edge
     *  @param shape  the shape of the edge
     *  @param from   the location of the origination node
     *  @param to     the location of the destination node
     *  @param at     the location of the edge (empty array => implicitly determined)
     */
    def animate (who: Identity, what: Value, color: Color,
                 shape: Shape, from: Component, to: Component, at: Array [Double] = Array ())
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            if (DEBUG) {
                println (">< Model.animate: " + label + "." + eid + " " + what + " " + color +
                         " " + shape + " " + from.me + " " + to.me + " " + at.deep)
            } // if
            aniQ += AnimateCommand (what, eid, shape, label, true, color, at, _clock, from.id, to.id)
        } // if
    } // animate

} // Model class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The ModelTest object is used to test the Model class.
 */
object ModelTest extends App
{
    val rm = new RoadModel ("road", 10, Uniform (4000, 6000), Uniform (2900, 3100))
    println ("results = " + rm.simulate ())

    class RoadModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name, false)
    {
        val onRamp  = new Source ("onRamp", this, Car, nArrivals, iArrivalRV, Array (100., 200., 30., 30.))
        val offRamp = new Sink ("offRamp", Array (400., 200., 30., 30.))
        val road    = new Transport ("road", moveRV, onRamp, offRamp, .25)
  
        addComponents (List (onRamp, road, offRamp))

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                road.move ()
                offRamp.leave ()
            } // act

        } // Customer

    } // RoadModel class

} // ModelTest object

