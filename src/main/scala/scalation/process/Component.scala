
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import collection.mutable.ListBuffer

import scalation.stat.{Statistic, TimeStatistic}
import scalation.util.{Identifiable, Locatable}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Component` trait provides basic common feature for simulation components.
 *  The list of subparts is empty for atomic components and nonempty for
 *  composite components.
 */
trait Component extends Identifiable with Locatable
{
    /** Radius of a token (for animating entities)
     */
    val RAD = 5.0

    /** Diameter of a token (for animating entities)
     */
    val DIAM = 2.0 * RAD

    /** List of subparts of the Component (empty for atomics, nonempty for composites)
     */
    val subpart = ListBuffer [Component] ()

    /** Director of the play/simulation model (to which this component belongs)
     */
    private var _director: Model = null

    /** Collector of sample statistics (e.g., waiting time)
     */
    private var _durationStat: Statistic = null

    /** Collector of time persistent statistics (e.g., number in queue)
     */
    private var _persistentStat: TimeStatistic = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate whether this component is composite, i.e., has subparts.
     */
    def composite: Boolean = subpart.size > 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize this component (all of its vars).
     *  @param label  the name of this component
     *  @param loc    the location of this component
     */
    def initComponent (label: String, loc: Array [Double])
    {
        setName (label)
        setAt (loc)
        initStats (label)
    } // initComponent

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize this component's statistical collectors.
     *  Sample statistics:  all `Component`s.
     *  Time-persistent statistics:  all except `Gate`, `Source` and `Sink`.
     *  @param label  the name of this component
     */
    def initStats (label: String)
    {
        _durationStat   = new Statistic (name)
        if (! this.isInstanceOf [Source] && ! this.isInstanceOf [Sink] && ! this.isInstanceOf [Gate]) {
            _persistentStat = new TimeStatistic ("p-" + name)
        } // if
    } // initStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate the statistics of this component's subparts.
     */
    def aggregate ()
    {
        val n = subpart.size
        if (n > 0) {
            val durationStatList   = ListBuffer [Statistic] ()
            val persistentStatList = ListBuffer [TimeStatistic] ()
            for (p <- subpart) {
                durationStatList += p.durationStat
                if (director.full && p.persistentStat != null) persistentStatList += p.persistentStat
            } // for
            _durationStat = Statistic.aggregate (durationStatList, name)
            if (director.full) _persistentStat = TimeStatistic.aggregate (persistentStatList, "p-" + name)
        } // if
    } // aggregate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the director who controls the play/simulation this component is in.
     */
    def director = _director

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this component's director (the controller of the simulation model).
     *  @param dir  the director of the play/simulation
     */
    def setDirector (dir: Model)
    {
        if (_director == null && dir != null) {
            _director = dir
        } else {
            flaw ("setDirector", "director may only be set once")
        } // if
    } // setDirector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Abstract display method.
     */
    def display ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tally the duration (e.g., waiting time) of an activity or delay.
     *  @param duration  the time duration
     */
    def tally (duration: Double) { _durationStat.tally (duration) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Accumulate the value (e.g., number in  queue) weighted by its time duration.
     *  @param value  the value to accumulate
     */
    def accum (value: Double) { _persistentStat.accum (value, _director.clock) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return sample statistics for durations for this component (e.g., Time in queue).
     */
    def durationStat = _durationStat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return time persistent statistics for value for this component (e.g. Number in queue).
     */
    def persistentStat = _persistentStat

} // Component trait

