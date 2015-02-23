
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import collection.mutable.{ListBuffer, PriorityQueue}

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.model.Modelable
import scalation.random.{Uniform, Variate}
import scalation.scala2d.Colors._
import scalation.scala2d.R2
import scalation.scala2d.Shapes.Shape
import scalation.stat.{Statistic, StatTable}
import scalation.util.Identifiable
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` class maintains a list of components making up the model and
 *  controls the flow of entities (SimActors) through the model, following the
 *  process-interaction world-view.  It maintains a time-ordered priority queue
 *  to activate/re-activate each of the entities.  Each entity (SimActor) is
 *  implemeneted as a Scala Actor and may be thought of as running in its own thread.
 *  @param name       the name of the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param animating  whether to animate the model
 *  @param full       generate a full report with both sample and time-persistent statistics
 */
class Model (name: String, aniRatio: Double = 1.0, animating: Boolean = true, val full: Boolean = true)
      extends Coroutine with Modelable with Component
{
    initComponent (name, Array ())

    private val DEBUG = true

    /** The agenda of things to be done (time-ordered activation list)
     */
    private val agenda = PriorityQueue.empty [SimActor]
//  private val agenda = new PQueue [SimActor] ()

    /** List of Components making up the model
     */
    private val parts = ListBuffer [Component] ()

    /** The currently acting actor (act one at a time)
     */
    private var _theActor: SimActor = null

    /** The animation engine
     */
    private val dgAni = if (animating) new DgAnimator ("Process Animator", black, white, aniRatio)
                        else null

    /** The animation engine's command queue
     */
    private val aniQ = if (animating) dgAni.getCommandQueue
                       else null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add component parts to the model.
     *  @param _parts  the component parts
     */
    def addComponent (_parts: Component*) { for (p <- _parts) parts += p }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add lists of component parts to the model.
     *  @param _parts  the lists of component parts
     */
    def addComponents (_parts: List [Component]*) { for (p <- _parts; q <- p) parts += q }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the current acting actor.
     */
    def theActor = _theActor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the order of actors based on their actTime.
     *  @param actor1  the first actor in comparison
     */
    private def orderedActor (actor1: SimActor): Ordered [SimActor] =
    {
        new Ordered [SimActor]
            { def compare (actor2: SimActor) = actor1.actTime compare actor2.actTime }
    } // orderedActor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the simulation (includes scheduling all Sources) returning summary
     *  statistics.
     *  @param startTime the time at which the simulation is to begin
     */
    def simulate (startTime: Double = 0.0)
    {
        _clock = startTime
        trace (this, "starts", this, _clock)
        for (p <- parts) {
            trace (this, "establish x = " + p.at(0) + " y = " + p.at(1), p, _clock)
            p.setDirector (this)
            for (q <- p.subpart) q.setDirector (this)
            if (p.isInstanceOf [Source]) reschedule (p.asInstanceOf [Source]) 
        } // for

        if (animating) display ()             // turn animation on (true) off (false)
        simulating = true
        start ()                              // start the director thread/actor -> act ()
    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Finish by producing statistical reports and optionally animation.
     */
    def fini ()
    {
        report
        reportf
        if (animating) dgAni.animate (0, 100000)
    } // fini

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
            yyield (_theActor)
        } // while

        trace (this, "terminates", this, _clock)
        fini ()
    } // act

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation.
     */
    def report
    {
        println (Statistic.line)
        println (Statistic.labels)
        println (Statistic.line)
        for (stat <- getStatistics) println (stat)
        println (Statistic.line)
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation in a new frame.
     */
    def reportf { new StatTable (name + " statistics", getStatistics) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the statistical results of the simulation (statistics for each part).
     *  This includes the sample/duration statistics and if 'full', time persistent
     *  statistics as well.
     */
    def getStatistics: ListBuffer [Statistic] =
    {
        val stats = new ListBuffer [Statistic] ()
        for (p <- parts) {
            if (p.composite) p.aggregate ()
            stats += p.durationStat
        } // for
        if (full) {
            for (p <- parts if p.persistentStat != null) {
                stats += p.persistentStat
            } // for
        } // if
//      for (st <- stats) println ("getStatistics: " + st.show)
        stats
    } // getStatistics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the components on the animation engine's queue.
     */
    def display ()
    {
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
    def animate (who: Identifiable, what: Value, color: Color, shape: Shape, at: Array [Double])
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            if (DEBUG) {
                println (">< Model.animate: " + label + "." + eid + " " + what + " " + color +
                         " " + shape + " " + at.deep)
            } // if
            aniQ.add (AnimateCommand (what, eid, shape, label, true, color, at, _clock))
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
    def animate (who: Identifiable, what: Value, color: Color, shape: Shape,
                 from: Component, to: Component, at: Array [Double] = Array ())
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            if (DEBUG) {
                println (">< Model.animate: " + label + "." + eid + " " + what + " " + color +
                         " " + shape + " " + from.me + " " + to.me + " " + at.deep)
            } // if
            aniQ.add (AnimateCommand (what, eid, shape, label, true, color, at, _clock, from.id, to.id))
        } // if
    } // animate

} // Model class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ModelTest` object is used to test the `Model` class.
 *  Caveat: must add 'from' and 'to' components before transport!!
 */
object ModelTest extends App
{
    val rm = new RoadModel ("road", 20, Uniform (2000, 5000), Uniform (3400, 3600))
    rm.simulate ()
    rm.report

    class RoadModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name)
    {
        val onRamp  = new Source ("onRamp", this, Car, 0, nArrivals, iArrivalRV, (100.0, 200.0))
        val offRamp = new Sink ("offRamp", (500.0, 200.0))
        val road    = new Transport ("road", onRamp, offRamp, moveRV, false, 0.25)
  
        addComponent (onRamp, offRamp, road)        // Caveat: must add from and to before transport!!

        case class Car () extends SimActor ("c", this)
        {
            def act ()
            {
                road.move ()
                offRamp.leave ()
            } // act

        } // Car

    } // RoadModel class

} // ModelTest object

