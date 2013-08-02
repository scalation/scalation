
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.event

import collection.mutable.{ListBuffer, PriorityQueue}

import scalation.model.ModelT
import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.random.{Uniform, Variate}
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.Shape
import scalation.stat.Statistic
import scalation.util.{Identity, Monitor}
import scalation.util.Monitor._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class schedules events and implements the time advance mechanism for
 *  simulation model following the event-scheduling world view.
 *  @param name       the name of the model
 *  @param animation  whether to animate the model
 */
class Model (name: String, animation: Boolean = true)
      extends ModelT with Identity
{
    /** The future event list (time-ordered list of events)
     */
    private val eventList = PriorityQueue.empty [Event]
//  private val eventList = new PQueue [Event] () 

    /** The animation engine
     */
    private val dgAni = new DgAnimator ("Event Animator", black, white)

    /** The animation engine's command queue
     */
    private val aniQ = dgAni.getCommandQueue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Place an event on the Future Event List (FEL) for later execution, thus
     *  scheduling the event to occur sometime in the future.  Events are ordered
     *  by their event/act time.
     *  @param event      the event to schedule
     *  @param timeDelay  how far in the future to schedule the event
     */
    def schedule (event: Event, timeDelay: Double)
    {
        event.actTime = _clock + timeDelay
        eventList += event
        aniQ += AnimateCommand (CreateToken, event.id, Ellipse (),
               "ev" + event.id, false, black, null, _clock, event.proto.id)
    } // schedule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove an event from the Future Event List (FEL) before it occurs, thus
     *  cancelling the event.
     *  @param event  the event to cancel
     */
    def cancel (event: Event)
    {
        // FIX: if (! (eventList -= event))
        flaw ("cancel", "unable to cancel event " + event)
    } // cancel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation by iteratively processing events in time order.
     *  Requires at least one to already be scheduled (see next method).
     *  @param startTime   the time at which the simulation is to begin
     */
    def simulate (startTime: Double = 0.0): ListBuffer [Statistic] =
    {
        _clock = startTime
        trace (this, "starts", this, _clock)

        var nextEvent: Event = null
        while (simulating && ! eventList.isEmpty) {
            nextEvent = eventList.dequeue ()
            _clock    = nextEvent.actTime
            trace (this, "execute event", nextEvent, _clock)
            nextEvent.occur ()
            aniQ += AnimateCommand (DestroyToken, nextEvent.id, null,
                    null, false, null, null, _clock, nextEvent.proto.id)
        } // while

        if (animation) dgAni.animate (0, 100000)
        trace (this, "terminates", this, _clock)
        getStatistics
    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation by iteratively processing events in time order.
     *  @param firstEvent  the first event is used to prime the simulation
     *  @param startTime   the time at which the simulation is to begin
     */
    def simulate (firstEvent: Event, startTime: Double): ListBuffer [Statistic] =
    {
        schedule (firstEvent, startTime)
        simulate (startTime)
    } // simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation for a given type of
     *  event.
     *  @param eventType  the type of event (e.g., Arrival, Departure)
     *  @param links      the causal links enimating from this event type
     */
    def report (eventType: String, links: Array [CausalLink] = Array ())
    {
        println ("\nResults for " + eventType + " events")
        if (links.length > 0) {
            println ("\nstatistics:\t" + links(0).durationStat.labels ())
            for (arc <- links) {
                val space = if (arc.name.length > 6) ":\t" else ":\t\t"
                println (arc.name + space + arc.durationStat)
            } // for
        } else {
            println ("\n--> has no enimating causal links")
        } // if
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation for the overall model.
     *  @param vars  the result/output variables for the simulation
     */
    def report (vars: Array [Tuple2 [String, Double]])
    {
        println ("\nResults for " + name + " model\n")
        for (v <- vars) {
            println ("result variable " + v._1 + " = " + v._2)
        } // for
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report on the statistical results of the simulation for the overall model.
     */
    def report
    {
        // FIX
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the statistical results of the simulation (statistics for each part).
     */
    def getStatistics: ListBuffer [Statistic] =
    {
        val stat = new ListBuffer [Statistic] ()
        // FIX
        stat      
    } // getStatistics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put a node/token command on the animation queue.
     *  @param who    who is being animated
     *  @param what   what animation command
     *  @param color  the color the node/token
     *  @param shape  the shape of the node/token
     *  @param at     the location of the node/token
     */
    def animate (who: Identity, what: Value, color: Color, shape: Shape, at: Array [Double])
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            println ("Model.animate: " + label + "." + eid + " " + what + " " + color +
                     " " + shape + " " + at.deep)
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
                 shape: Shape, from: Event, to: Event, at: Array [Double] = Array ())
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            println ("Model.animate: " + label + "." + eid + " " + what + " " + color +
                     " " + shape + " " + from.me + " " + to.me + " " + at.deep)
            aniQ += AnimateCommand (what, eid, shape, label, true, color, at, _clock, from.id, to.id)
        } // if
    } // animate

} // Model class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Model class.
 */
object ModelTest extends App
{
    new PhoneModel ("phone", Uniform (5000, 7000), 100, Uniform (4000, 6000))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This class models a simple phone simulation.
     *  @param name         the name of the simulation model
     *  @param arrivalDist  the inter-arrival time distribution     
     *  @param maxCalls     the maximum number of phone call (stopping condition)
     *  @param serviceDist  the service time distribution
     */
    class PhoneModel (name: String, arrivalDist: Variate, maxCalls: Int, serviceDist: Variate)
          extends Model (name)
    {
        //:: define the state variables for the simulation

        var busy     = false
        var nCalls   = 0.0
        var nDrops   = 0.0
        var nHangups = 0.0

        //:: define the nodes in the event graph (event prototypes)

        val protoArrival   = Arrival (null)       // prototype for all Arrival events
        val protoDeparture = Departure (null)     // prototype for all Departure events

        //:: define the edges in the event graph (causal links between events)

        val aLinks = Array (CausalLink ("link2A", this, () => nCalls < maxCalls, protoArrival,
                                        () => Arrival (null), arrivalDist),
                            CausalLink ("link2D", this, () => ! busy, protoDeparture,
                                        () => Departure (null), serviceDist))

        protoArrival.displayLinks (aLinks)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Create a subclass of Event for Arrival events.
         *  @param call  the entity that arrives, in this case a phone call
         */
        case class Arrival (call: Entity)
             extends Event (protoArrival, call, aLinks, this, Array (150.0, 200.0, 50.0, 50.0))
        {
            override def occur ()
            {
                super.occur ()        // required if there are causal links
                nCalls += 1
                if (busy) nDrops += 1
                busy = true
            } // occur

        } // Arrival class

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Create a subclass of Event for Departure events.
         *  @param call  the entity that departs, in this case a phone call
         */
        case class Departure (call: Entity)
             extends Event (protoDeparture, call, null, this, Array (450.0, 200.0, 50.0, 50.0))
        {
            override def occur ()
            {
                busy      = false
                nHangups += 1
            } // occur

        } // Departure class

        //:: start the simulation, passing the first priming event and the start time

        simulate (Arrival (null), 0.0)
        Thread.sleep (2000)
        report (Array (("nCalls", nCalls), ("nDrops", nDrops), ("nHangups", nHangups)))
        report ("Arrival", aLinks)
        report ("Departure")

    } // PhoneModel

} // ModelTest object

