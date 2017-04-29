
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author   John Miller, Casey Bowman
 *  @version  1.2
 *  @date     Fri Apr 26 17:52:27 EDT 2013
 *  @see      LICENSE (MIT style license file).
 */

package apps.optimization

import scala.collection.mutable.HashMap

import scalation.graphalytics.ColorDAG
import scalation.random.Randi

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ServiceNetwork` class is used for optimizing service networks.
 *  @param dimensions  the dimension (number of nodes) for each stage
 *  @param maxIn       the maximum number of input colors for an internal node
 *  @param maxOut      the maximum number of output colors for an internal node
 *  @param minColors   the minimum number of child colors allowed in the color hierarchy
 *  @param maxColors   the maximum number of child colors allowed in the color hierarchy
 *  @param colorDepth  the maximum depth of the color hierarchy tree
 *  @param seed        the seed for the various random number generators
 *  @param maxunits    the maximum number of units made per production cycle for a node
 *  @param maxCost     the maximum cost for a node's service
 */

class ServiceNetwork (dimensions: Array [Int], maxIn: Int, maxOut: Int,
                      minColors: Int, maxColors: Int, colorDepth: Int, seed: Int,
                      maxUnits: Int = 5, maxCost: Int = 5)
      extends ColorDAG (dimensions, maxIn, maxOut, minColors, maxColors, colorDepth, seed)
{
    val randUnit = new Randi (1, maxUnits, seed)     // random number generator for node production capacities
    val randCost = new Randi (1, maxCost, seed)      // random number generator for node costs

    val unit = HashMap [Int, Int] ()                 // map node-id to node's # units
    val cost = HashMap [Int, Int] ()                 // map node-id to node's cost

    genNodes ()                                      // generate the nodes in the color DAG
    checkIO ()                                       // throw out nodes that are color/type incompatible
    connect ()                                       // connect the remaining node via edges
    popInOut ()                                      // populated the input/ouput lists from edges
    setCosts ()                                      // set costs and production for each node

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the costs and associated production parameters.
     */
    def setCosts ()
    {
        for (i <- 1 until stages; n <- nodes(i) if n.used) {
            unit(n.id) = randCost.igen                  // production level (# units) per cycle
            cost(n.id) = randCost.igen                  // cost of a production cycle
            if (n.isSink) cost(n.id) *= -10             // revenue comes from sinks
        } // for
    } // setCosts

} // ServiceNetwork class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ServiceNetworkTest` object is used to the run the `ServiceNetwork` example.
 *  > run-main apps.optimization.ServiceNetworkTest
 */
object ServiceNetworkTest extends App
{
    val seed = if (args.length == 1) args(0).toInt else 0

    val stg  = Array (5, 5, 5, 5)                             // define the number of nodes for each stage of color DAG
    val snet = new ServiceNetwork (stg, 2, 2, 2, 2, 2, seed)  // create a new color DAG
    snet.printColorTree                                       // print the color type representing the type hierarchy

    snet.printDAG                                             // print the nodes and edges
    snet.printColors                                          // print inputs and output colors for each node
//  snet.printInOut                                           // print the input and ouput lists for each node
    snet.animateDAG   

} // ServiceNetworkTest object

