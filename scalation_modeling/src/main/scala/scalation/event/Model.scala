
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.event

import scala.collection.mutable.{ListBuffer, PriorityQueue}

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.model.Modelable
import scalation.random.{Exponential, Variate}
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.Shape
import scalation.stat.{Statistic, TimeStatistic}
import scalation.util.Identifiable
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` class schedules events and implements the time advance mechanism
 *  for simulation model following the event-scheduling world view.
 *  @param name       the name of the model
 *  @param animating  whether to animate the model (only for Event Graphs)
 */
class Model (name: String, animating: Boolean = false)
      extends Modelable with Identifiable
{
    /** The future event list (time-ordered list of events)
     */
    private val eventList = PriorityQueue.empty [Event]
//  private val eventList = new PQueue [Event] () 

    private val stats = ListBuffer [Statistic] ()

    /** The time in sYstem statistics
     */
    private val t_y_stat = new Statistic ("t_y")

    /** The animation engine
     */
    private val dgAni = if (animating) new DgAnimator ("Event Animator", black, white) else null

    /** The animation engine's command queue
     */
    private val aniQ = if (animating) dgAni.getCommandQueue else null

    /** The start time for the simulation
     */
    private var start = -1.0

    addStats (t_y_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add statistical collector to the model.
     *  @param stat  one or more statistical collectors
     */
    def addStats (stat: Statistic*)
    {
        for (st <- stat) stats += st
    } // addStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Place an event on the Future Event List 'FEL' for later execution, thus
     *  scheduling the event to occur sometime in the future.  Events are ordered
     *  by their event/act time.
     *  @param event  the event to schedule
     */
    def schedule (event: Event)
    {
        eventList += event
//      println ("event.proto = " + event.proto)
        if (animating) aniQ.add (AnimateCommand (CreateToken, event.id, Ellipse (),
                                 "ev" + event.id, false, black, null, _clock, event.proto.id))
    } // schedule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cancel the specified 'event' so it will not occur.
     *  @param event  the event to cancel
     */
    def cancel (event: Event) { event.cancel () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Leave the model recording the entity's time in the sYstem.
     *  @param entity  the entity leaving the model
     */
    def leave (entity: Entity) { t_y_stat.tally (clock - entity.arrivalT) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation by iteratively processing events in time order.
     *  Requires at least one to already be scheduled (see next method).
     *  @param startTime   the time at which the simulation is to begin
     */
    def simulate (startTime: Double = 0.0)
    {
        start  = startTime
        _clock = startTime
        trace (this, "starts", this, _clock)

        var nextEvent: Event = null
        simulating = true
        if (eventList.isEmpty) flaw ("simulate", "eventList must not be empty at start")
        while (simulating && ! eventList.isEmpty) {
            nextEvent = eventList.dequeue ()
            if (nextEvent.live) {
                _clock = nextEvent.actTime
//              trace (this, "executes " + nextEvent.me, nextEvent.entity, "%g".format (_clock))
                println (nextEvent + "\t" + "%g".format (_clock))
                nextEvent.occur ()
            } // if
            if (animating) aniQ.add (AnimateCommand (DestroyToken, nextEvent.id, null,
                                     null, false, null, null, _clock, nextEvent.proto.id))
        } // while

        if (animating) dgAni.animate (0, 100000)
        trace (this, "terminates", this, _clock)
    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report values of the specified model result/output variables.
     *  @param vars  the result/output variables for the simulation
     */
    def report (vars: Tuple2 [String, Double]*)
    {
        println ("\nResults for " + name + " model\n")
        for (v <- vars) println ("result: " + v._1 + " \t= " + v._2)
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report statistical results of the simulation for all the collectors.
     */
    def reportStats ()
    {
        println ("Statistical results for " + name)
        println (Statistic.line)
        println (Statistic.labels)
        println (Statistic.line)
        for (st <- stats) println (st)
        println (Statistic.line)
    } // reportStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the statistical results of the simulation (statistics for each part).
     */
    def getStatistics: ListBuffer [Statistic] = stats

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
            println ("Model.animate: " + label + "." + eid + " " + what + " " + color +
                     " " + shape + " " + at.deep)
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
    def animate (who: Identifiable, what: Value, color: Color,
                 shape: Shape, from: Event, to: Event, at: Array [Double] = Array ())
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            println ("Model.animate: " + label + "." + eid + " " + what + " " + color +
                     " " + shape + " " + from.me + " " + to.me + " " + at.deep)
            aniQ.add (AnimateCommand (what, eid, shape, label, true, color, at, _clock, from.id, to.id))
        } // if
    } // animate

} // Model class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ModelTest` object is used to test the `Model` class.
 *  > run-main scalation.event.ModelTest
 */
object ModelTest extends App
{
    new PoissonModel ("Poisson", 100, Exponential (1.0))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `PoissonModel` models the detection of particles (e.g., gamma rays)
     *  from the decay of radioactive atoms as a Poisson Process.
     *  @see http://stuff.mit.edu/afs/sipb/user/biyeun/Public/8.13/poisson/poisson_statistics_biyeun.pdf
     *  @param name        the name of the simulation model
     *  @param nArrivals   the number of arrivals to generate (stopping condition)
     *  @param iArrivalRV  the inter-arrival time distribution (Random Variate)
     */
    class PoissonModel (name: String, nArrivals: Int, iArrivalRV: Variate)
          extends Model (name, true)                             // true => turn on animation
    {
        val t_a_stat  = new Statistic ()                  // time between Arrivals statistics
        val aLoc      = Array (150.0, 200.0, 50.0, 50.0)  // Arrival event node location

        val aProto    = new EventNode (this, aLoc)        // prototype for all Arrival events

        val aLink = Array (CausalLink ("l_A2A", this, () => nArr < nArrivals-1, aProto))
    
        var nArr   = 0.0                           // number of particles detected

        aProto.displayLinks (aLink)

        addStats (t_a_stat)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** `Arrival` is a subclass of `EventNode` for handling arrival events.
         *  @param call   the entity that arrives, in this case a phone call
         *  @param delay  the time delay for this event's occurrence
         */
        case class Arrival (call: Entity, delay: Double)
             extends Event (call, this, delay, t_a_stat, aProto)
        {
            override def occur ()
            {
                if (aLink(0).condition ()) {
                    val toArrive = Entity (iArrivalRV.gen, 0.0, PoissonModel.this)
                    schedule (Arrival (toArrive, toArrive.iArrivalT))
                } // if
                nArr += 1                                        // update the current state
            } // occur

        } // Arrival class

        //:: start the simulation after scheduling the first priming event

        val firstArrival = Entity (iArrivalRV.gen, 0.0, this)
        schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
        simulate ()                                                   // start simulating

        Thread.sleep (20000)                                      // wait on animation trace
        report (("nArr", nArr))
        reportStats ()

    } // PoissonModel

} // ModelTest object

