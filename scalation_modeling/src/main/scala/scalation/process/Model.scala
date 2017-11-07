
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import java.util.concurrent.Semaphore

import scala.collection.mutable.{HashMap, ListBuffer, PriorityQueue}

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.linalgebra.VectorD
import scalation.math.ExtremeD.MAX_VALUE
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
 *  controls the flow of entities (`SimActor`s) through the model, following the
 *  process-interaction world-view.  It maintains a time-ordered priority queue
 *  to activate/re-activate each of the entities.  Each entity (`SimActor`) is
 *  implemented as a Scala `Actor` and may be thought of as running in its own thread.
 *  @param name       the name of the model
 *  @param reps       the number of independent replications to run
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param animating  whether to animate the model
 *  @param full       generate a full report with both sample and time-persistent statistics
 */
class Model (name: String, val reps: Int = 1, animating: Boolean = true, aniRatio: Double = 1.0,
             val full: Boolean = true)
      extends Coroutine (name) with Modelable with Component
{
    initComponent (name, Array ())

    /** The map of statistics vectors records the means of each replication
     */
    val statV = HashMap [String, VectorD] ()

    /** The stop time for the model
     */
    var stopTime = MAX_VALUE

    /** Debug flag
     */
    private val DEBUG = true

    /** The time at which the simulation is to begin
     */
    private var startTime = 0.0

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

    /** Application models extending the Model class may use this semaphore to wait for results
     */
    private val finished = new Semaphore (0)

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
    /** Indicate whether the model has been stopped.
     */
    def stopped: Boolean = ! simulating

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the agenda and stateful components for next replication.
     */
    def reset ()
    {
        println ("--------------------------------------------------------------------------")
        println ("Model.reset in progress")

        // reset the agenda - activation priority queue
        while (! agenda.isEmpty) agenda.dequeue ()                   // clean out actors from agenda

        // reset stateful components
        for (p <- parts) {
            if (p.isInstanceOf [Source]) {                           // reset sources
                val s = p.asInstanceOf [Source]
                reschedule (s)
            } // if
            if (p.isInstanceOf [WaitQueue]) {                        // reset wait queues
                val w = p.asInstanceOf [WaitQueue]
                while (! w.isEmpty) w.dequeue ()
            } // if
        } // for
    } // reset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset and aggregate all statistics.
     *  @param rep  the current replication (1, ... reps)
     */
    def resetStats (rep: Int)
    {
        if (rep == 1) {
            for (stat <- getStatistics) statV += stat.name -> new VectorD (reps)
        } // for
        for (stat <- getStatistics) {
            statV (stat.name)(rep - 1) = stat.mean
            stat.reset ()
        } // for
    } // resetStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the simulation (includes scheduling all Sources) returning summary
     *  statistics.
     */
    def simulate (_startTime: Double = 0.0)
    {
        startTime = _startTime
        _clock = startTime
        trace (this, "starts", this, _clock)

        for (p <- parts) {
            trace (this, "establish x = " + p.at(0) + " y = " + p.at(1), p, _clock)
            p.director = this
            for (q <- p.subpart) q.director = this
            if (p.isInstanceOf [Source]) reschedule (p.asInstanceOf [Source]) 
        } // for

        start ()                                            // start the director thread/actor -> act ()
    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cleanup the agenda and any stateful components.  Any actors left in the
     *  agenda or a wait queue must be terminated.  The model (i.e., the director)
     *  must be terminated as well.
     */
    def cleanup ()
    {
        println ("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
        println ("Model.cleanup in progress")

        println ("cleanup: agenda")
        while (! agenda.isEmpty) {                           // cleanup actors left on agenda
            val a = agenda.dequeue ()
            if (a != this) {
                println ("cleanup: terminate actor " + a + " in agenda")
                a.interrupt ()                               // terminate all actors, except director
            } // if
        } // while

        println ("cleanup: wait queues")
        for (p <- parts) {
            if (p.isInstanceOf [WaitQueue]) {                // cleanup wait queues
                val w = p.asInstanceOf [WaitQueue]
                while (! w.isEmpty) {
                    val a = w.dequeue ()
                    println ("cleanup: terminate actor " + " a " + " in " + w)
                    a.interrupt ()                           // terminate all actors in queue
                } // while
            } // if
        } // for
    } // cleanup

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Wait for the main simulation Coroutine to complete.  This can used used
     *  to time execution of models.
     *  @see apps.process.Bank2
     */
    def complete ()
    {
        finished.acquire ()                                  // wait on semaphore
    } // complete

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Schedule (first time) or reschedule (subsequent times) an actor to act.
     *  @param actor  the actor to be scheduled
     */
    def reschedule (actor: SimActor)
    {
        agenda += actor
    } // reschedule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The model itself is an Actor (not an ordinary `SimActor`) and may be
     *  thought of as the director.  The director iteratively manages the clock
     *  and the agenda of actors until the simulation flag becomes false
     *  or the agenda (priority queue) becomes empty.
     */
    def act ()
    {
        trace (this, "starts model for " + reps + " replications", null, _clock)
        for (rep <- 1 to reps) {                             // run reps replication
            _clock = startTime
            if (rep == 1 && animating) display ()            // turn animation on (true) off (false)

            trace (this, "starts rep " + rep, null, _clock)
            simulating = _clock <= stopTime                  // simulate unless past stop time
            while (simulating && ! agenda.isEmpty) {
                _theActor = agenda.dequeue ()                // next from priority queue
                _clock    = _theActor.actTime                // advance the time
                trace (this, "resumes", _theActor, _clock)
                yyield (_theActor)                           // director yields to actor
            } // while
            simulating = false
            trace (this, "ends rep " + rep, null, _clock)

            fini (rep)                                       // post-run results
            if (rep < reps) reset ()                         // reset for next replication
            resetStats (rep)                                 // reset and aggregate statistics
        } // for

        cleanup ()
        println ("statV = " + statV)
        println ("coroutine counts = " + counts)
        trace (this, "terminates model", null, _clock)
        finished.release ()                                  // signal via semaphore that simulation is finished
        yyield (null, true)                                  // yield and terminate the director
    } // act

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the order of actors based on their 'actTime's.
     *  @param actor1  the first actor in comparison
     */
    private def orderedActor (actor1: SimActor): Ordered [SimActor] =
    {
        new Ordered [SimActor]
            { def compare (actor2: SimActor) = actor1.actTime compare actor2.actTime }
    } // orderedActor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Finish by producing statistical reports and optionally animation.
     *  Typically animation and reports in pop up window turned off for high
     *  replications and/or simulation optimization.
     *  @param rep  the replication number (1, ... reps)
     */
    private def fini (rep: Int)
    {
        report ()                                        // report in terminal
        if (animating) {
            reportf ()                                   // report in new window
            if (rep == 1) dgAni.animate (0, 100000)      // only animate first rep
        } // if
    } // fini

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation.
     */
    private def report ()
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
    private def reportf () { new StatTable (name + " statistics", getStatistics) }

} // Model class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` companion object provides a shutdown method.
 */
object Model
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shutdown the Model execution infrastructure (WARNING: this method should
     *  only be called right before program termination).  Make sure all threads
     *  have finished (e.g., call `complete`), not just the main thread.
     *  If `shutdown` is not called, the application may hang.
     *  @see `ModelTest`, `apps.process.Bank2`
     */
    def shutdown ()
    {
        Coroutine.shutdown ()
    } // shutdown

} // Model object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ModelTest` object is used to test the `Model` class.
 *  Caveat: must add 'from' and 'to' components before transport!!
 */
object ModelTest extends App
{
    val rm = new RoadModel ("Road", 20, Uniform (2000, 5000), Uniform (3400, 3600))
    rm.simulate ()
    rm.complete ()
    Model.shutdown ()

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

