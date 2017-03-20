
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Nov 19 18:43:58 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrafficLight` object is an enumeration object for traffic light colors.
 *  Vertices are marked GreeN (unvisited), YelloW (processing), or ReD (done with).
 */
object TrafficLight extends Enumeration
{
    type TrafficLight = Value
    val G_N, Y_W, R_D = Value

} // TrafficLight object

