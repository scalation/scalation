
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
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
            println ("+ " + who.me + " " + what + " " + whom.me + " at time " + when + ".")
        } // if
    } // trace

} // Monitor object

