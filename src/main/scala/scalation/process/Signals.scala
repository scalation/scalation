
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Oct  9 14:56:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This trait defines standard messages sent between actors implementing
 *  process interaction simulations.
 */
trait Signals
{
    val RESUME_ACTOR    = "RA"
    val RESUME_DIRECTOR = "RD"
    val RETURN_RESULTS  = "RR"

} // Signals

