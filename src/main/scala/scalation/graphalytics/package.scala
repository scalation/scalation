
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Jul 21 13:35:37 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation

//import scala.language.implicitConversions

//import scalation.linalgebra.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `graphalytics` package contains classes, traits and objects for graph
 *  analytics on Trees, DAGs and Directed Graphs.  It provides an implicit
 *  conversion when needed for converting doubles to vectors.
 */
package object graphalytics
{
    import scalation.util.SEP

    /** The `EdgeType` object define basic type for represeting edges.
     */
    type Pair = Tuple2 [Int, Int]         // edge = (vertex, vertex)

    /** The relative path for base directory
     */
    val BASE = "data" + SEP + "graphalytics" + SEP

    /** The standard file extension for graphs
     */
    val EXT = ".grp"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Implicit conversion from 'Double' to 'VectorD' for cases when TLable
     *  is a vector.
     *  @param d  the double to connvert to a vector
     */
//  implicit def double2VectorD (d: Double): VectorD = VectorD (d)

} // graphalytics package object

