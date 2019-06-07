
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.clusterer

import scalation.linalgebra.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Clusterer` trait provides a common framework for several clustering
 *  algorithms.
 */
trait Clusterer
{
    private var _name: Array [String] = null   // optional names for clusters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of points/vectors, put them in clusters, returning the cluster
     *  assignment vector.  A basic goal is to minimize the sum of the distances
     *  between points within each cluster.
     */
    def cluster (): Array [Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector y, determine which cluster it belongs to.
     *  @param y  the vector to classify
     */
    def classify (y: VectorD): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the names for the clusters.
     *  @param n  the array of names
     */
    def name_ (n: Array [String])
    {
        _name = n   
    } // name_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the name of the i-th cluster.
     *  @param y  the vector to classify
     */
    def getName (i: Int): String = 
    {
        if (_name != null && i < _name.length) _name(i) else "unknown"
    } // getName

} // Clusterer trait

