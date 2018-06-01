
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.clusterer

import scalation.linalgebra.{MatrixD, VectorD, VectorI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Clusterer` trait provides a common framework for several clustering
 *  algorithms.
 */
trait Clusterer
{
    /** Optional names for clusters
     */
    private var _name: Array [String] = null

    /** Flag indicating whether the points have already been clusterer
     */
    protected var clustered = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of points/vectors, put them in clusters, returning the cluster
     *  assignment vector.  A basic goal is to minimize the sum of the distances
     *  between points within each cluster.
     */
    def cluster (): Array [Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the centroids (a centroid is the mean of points in a cluster).
     *  Should only be called after 'cluster ()'. 
     */
    def centroids (): MatrixD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector y, determine which cluster it belongs to.
     *  @param y  the vector to classify
     */
    def classify (y: VectorD): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sizes (number of points within) of the clusters.  Should only
     *  be called after 'cluster ()'. 
     */
    def csize (): VectorI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the names for the clusters.
     *  @param n  the array of names
     */
    def name_ (n: Array [String]) { _name = n   } // name_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the name of the i-th cluster.
     *  @param y  the vector to classify
     */
    def getName (i: Int): String = 
    {
        if (_name != null && i < _name.length) _name(i) else "unknown"
    } // getName

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a distance metric (e.g., distance squared) between vectors/points
     *  'u' and 'v'.  Override this methods to use a different metric, e.g.,
     *     'norm'   - the Euclidean distance, 2-norm
     *     'norm1'  - the Manhattan distance, 1-norm
     *  @param u  the first vector/point
     *  @param v  the second vector/point
     */
    def distance (u: VectorD, v: VectorD): Double = 
    {
        (u - v).normSq     // squared Euclidean norm used for efficiency, may use other norms
    } // distance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squared errors within the clusters, where error is
     *  indicated by e.g., the distance from a point to its centroid.
     */
    def sse (x: MatrixD): Double =
    {
        val cent   = centroids ()
        val clustr = cluster ()
        var sum    = 0.0
        for (i <- x.range1) sum += distance (x(i), cent(clustr(i)))
        sum
    } // sse

} // Clusterer trait

