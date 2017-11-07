
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.4
 *  @date    Tue oct 27 05:41:17 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.MultiSet

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LabelFunctions` object provides methods for obtaining child and  
 *  parents labels for the corresponding data and query graphs.
 */
object LabelFunctions
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the query graph and value of query vertex it returns its corresponding 
     *  child labels.
     *  @param q  query graph
     *  @param u  query graph vertex
     */
    def qChildLabels [TLabel: ClassTag] (q: Graph [TLabel], u: Int): MultiSet [TLabel] = 
    {
        val cLabel = new MultiSet [TLabel] ()
        for (c <- q.ch(u)) cLabel += q.label(c) 
        cLabel
    } // qChildLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the query graph and value of query vertex it returns its corresponding
     *  parent labels.
     *  @param q  query graph
     *  @param u  query graph vertex
     */
    def qParentLabels [TLabel: ClassTag] (q: Graph [TLabel], u: Int): MultiSet [TLabel] =
    {
        val pLabel = new MultiSet [TLabel] ()
        for (p <- q.pa(u)) pLabel += q.label(p)
        pLabel
    } // qParentLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the value of query vertex, graph vertex and phi, it returns the child 
     *  labels of the data graph corresponding to the child labels of query graph.
     *  @param g    data graph
     *  @param v    data graph vertex
     *  @param u    query graph vertex
     *  @param chu  children of vertex u
     *  @param phi  array of mappings from a query vertex u to { data graph vertices v }
     */
    def gChildLabels [TLabel: ClassTag] (g: Graph [TLabel], v: Int, u: Int, chu: SET [Int], 
                                         phi: Array [SET [Int]]): MultiSet [TLabel] =
    {
        val st = SET [Int] ()
        for (u_c <- chu; v_c <- g.ch(v)) if (phi(u_c) contains v_c) st += v_c
        val ms = new MultiSet [TLabel] ()
        for (v_c <- st) ms += g.label(v_c)
        ms
    } // gChildLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the value of query vertex, graph vertex and phi, it returns the parent
     *  labels of the data graph corresponding to the parent labels of query graph.
     *  @param g    data graph
     *  @param v    data graph vertex
     *  @param u    query graph vertex
     *  @param pau  parents of vertex u
     *  @param phi  array of mappings from a query vertex u to { data graph vertices v }
     */
    def gParentLabels [TLabel: ClassTag] (g: Graph [TLabel], v: Int, u: Int, pau: SET [Int], 
                                          phi: Array [SET [Int]]): MultiSet [TLabel] =
    {
        val st = SET [Int] ()
        for (u_p <- pau; v_p <- g.pa(v)) if (phi(u_p) contains v_p) st += v_p
        val ms = new MultiSet [TLabel] ()
        for (v_p <- st) ms += g.label(v_p)
        ms
    } // gParentLabels

} // LabelFunctions object

