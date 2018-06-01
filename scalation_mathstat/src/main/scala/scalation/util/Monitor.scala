
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Monitor` object is used to trace the actions/events in the models.
 */
object Monitor
{
    /** Flag indicating whether tracing is on (initially on)
     */
    private var tracing = true 

    /** Use `EasyWriter` to make it easy to switch from standard out to a (log) file
     */
    private val ew = new EasyWriter ("util", "monitor")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle output destination from default of (log) file to standard output. etc.
     */
    def toggle () { ew.toggle () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Turn tracing off.
     */
    def traceOff () { tracing = false }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Turn tracing back on.
     */
    def traceOn () { tracing = true }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Trace an action/event.
     *  @param who   who caused the action
     *  @param what  what was the action
     *  @param whom  whom did the action effect
     *  @param when  when was the action taken
     */
    def trace (who: Identifiable, what: String, whom: Identifiable, when: Double)
    {
        if (tracing) {
            if (whom == null) {
                ew.println ("+ " + who.me + " " + what + " at time " + when + ".")
            } else {
                ew.println ("+ " + who.me + " " + what + " " + whom.me + " at time " + when + ".")
            } // if
        } // if
    } // trace

} // Monitor object

