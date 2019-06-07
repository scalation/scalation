
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Jul 21 13:35:37 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation

//import scala.language.implicitConversions

//import scalation.linalgebra.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `graph_db` package contains classes, traits and objects for graph
 *  analytics on Trees, 'DAG's and Directed Graphs.  It provides an implicit
 *  conversion when needed for converting doubles to vectors.
 */
package object graph_db
{
    /** The `EdgeType` object define basic type for representing edges
     */
    type Pair = Tuple2 [Int, Int]         // edge = (vertex, vertex)

    /** The relative path for base directory
     */
    val BASE_DIR = DATA_DIR + "graph_db" + ⁄

    /** The standard file extension for graphs
     */
    val EXT = ".grp"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Implicit conversion from 'Double' to 'VectorD' for cases when TLable
     *  is a vector.
     *  @param d  the double to convert to a vector
     */
//  implicit def double2VectorD (d: Double): VectorD = VectorD (d)

} // graph_db package object

