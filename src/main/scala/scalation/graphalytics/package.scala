
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Jul 21 13:35:37 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import language.implicitConversions

import scalation.linalgebra.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `graphalytics` package contains classes, traits and objects for
 *  graph analytics on Trees, DAGs and Directed Graphs. 
 */
package object graphalytics
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Implicit conversion from 'Double' to 'VectorD' for cases when TLable
     *  is a vector.
     *  @param d  the double to connvert to a vector
     */
//  implicit def double2VectorD (d: Double): VectorD = VectorD (d)

} // graphalytics package object

