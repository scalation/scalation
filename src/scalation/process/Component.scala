
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import scalation.stat.{Statistic, TimeStatistic}
import scalation.util.Identity

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Component trait provides basic common feature for simulation components.
 */
trait Component extends Identity
{
    /** Radius of a token (for animating entities)
     */
    val RAD = 5.

    /** Diameter of a token (for animating entities)
     */
    val DIAM = 2. * RAD

    /** Director of the play/simulation model (to which this component belongs)
     */
    private var _director: Model = null

    /** Where this component is at (its location)
     */
    private var _at: Array [Double] = null

    /** Collector of sample statistics (e.g., waiting time)
     */
    private var _durationStat: Statistic = null

    /** Collector of time persistent statistics (e.g., number in queue)
     */
    private var _persistentStat: TimeStatistic = null

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
     *  @param label  the name of this component
     */
    def initStats (label: String)
    {
        _durationStat   = new Statistic (name)
        _persistentStat = new TimeStatistic (name)
    } // initStats

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
    /** Return where this component is at (its location).
     */
    def at = _at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the location of this component.
     *  @param loc  the location of this component
     */
    def setAt (loc: Array [Double])
    {
        if (_at == null && loc != null) {
            _at = loc
        } else {
            flaw ("setAt", "location may only be set once")
        } // if
    } // setAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Abstract display method.
     */
    def display (): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tally the duration (e.g., waiting time) of an activity or delay.
     *  @param duration  the time duration
     */
    def tally (duration: Double) { _durationStat.tally (duration) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Accumulate the value (e.g., number in  queue) weighted by its time duration.
     *  @param value  the value to accumulate
     *  @param time   the current time of the observation
     */
    def accumulate (value: Double, time: Double) { _persistentStat.accumulate (value, time) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return sample statistics for durations for this component (e.g., Time in queue).
     */
    def durationStat = _durationStat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return time persistent statistics for value for this component (e.g. Number in queue).
     */
    def persistentStat = _persistentStat

} // Component trait

