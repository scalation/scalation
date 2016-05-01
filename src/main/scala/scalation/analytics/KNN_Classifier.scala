
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Sep 22 18:45:44 EDT 2013 
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.util.control.Breaks.{breakable, break}
import scala.collection.mutable.Set

import scalation.linalgebra.{MatrixD, VectorD, VectoD, VectorI}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_Classifier` class is used to classify a new vector 'z' into one of
 *  'k' classes.  It works by finding its 'knn' nearest neighbors.  These neighbors
 *  essentially vote according to their classification.  The class with most
 *  votes is selected as the classification of 'z'.  Using a distance metric,
 *  the 'knn' vectors nearest to 'z' are found in the training data, which is
 *  stored row-wise in the data matrix 'x'.  The corresponding classifications
 *  are given in the vector 'y', such that the classification for vector 'x(i)'
 *  is given by 'y(i)'.
 *  @param x    the vectors/points of classified data stored as rows of a matrix
 *  @param y    the classification of each vector in x
 *  @param fn   the names for all features/variables
 *  @param k    the number of classes
 *  @param cn   the names for all classes
 *  @param knn  the number of nearest neighbors to consider
 */
class KNN_Classifier (x: MatrixD, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
                      knn: Int = 3)
      extends ClassifierReal (x, y, fn, k, cn)
{
    private val DEBUG      = true                                        // degug flag
    private val MAX_DOUBLE = Double.PositiveInfinity                     // infinity
    private val topK       = Array.ofDim [Tuple2 [Int, Double]] (knn)    // top-knn nearest points (in reserve order)
    private val count      = new VectorI (k)                             // how many nearest neighbors in each class.

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a distance metric between vectors/points u and v.
     *  @param u  the first vector/point
     *  @param v  the second vector/point
     */
    def distance (u: VectoD, v: VectoD): Double =
    {
        (u - v).normSq       // squared Euclidean norm used for efficiency, may use other norms
    } // distance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'knn' nearest neighbors (top-'knn') to vector 'z' and store in
     *  the 'topK' array.
     *  @param z  the vector to be classified
     */
    def kNearest (z: VectoD)
    {
        var dk = MAX_DOUBLE
        for (i <- 0 until x.dim1) {
            val di = distance (z, x(i))                   // compute distance to z
            if (di < dk) dk = replace (i, di)             // if closer, adjust top-knn
        } // for
        if (DEBUG) println ("topK = " + topK.deep)
    } // kNearest

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Training involves resetting the data structures before each classification.
     *  It uses lazy training, so most of it is done during classification.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int)    // FIX - use these parameters
    {
        for (i <- 0 until knn) topK(i)  = (-1, MAX_DOUBLE)   // intialize top-knn
        for (j <- 0 until k) count(j) = 0                    // initilize counters
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector 'z', determine which class it belongs to (i.e.,
     *  the class getting the most votes from its 'knn' nearest neighbors.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): Tuple2 [Int, String] =
    {
        kNearest (z)                                         // set top-knn to knn nearest
        for (i <- 0 until knn) count(y(topK(i)._1)) += 1     // tally per class
        println ("count = " + count)
        val best = count.argmax ()                           // class with maximal count
        (best, cn(best))                                     // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the most distant neighbor and add new neighbor 'i'.  Maintain the
     *  'topK' nearest neighbors in sorted order farthest to nearest.
     */
    private def replace (i: Int, di: Double): Double =
    {
        var j = 0
        while (j < knn-1 && di < topK(j)._2) { topK(j) = topK(j+1); j += 1 }
        topK(j) = (i, di)
        topK(0)._2                      // the distance of the new farthest neighbor
    } // replace

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        // FIX: to be implemented
    } // reset

} // KNN_Classifier class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_ClassifierTest` object is used to test the `KNN_Classifier` class.
 */
object KNN_ClassifierTest extends App
{
    val x = new MatrixD ((6, 2), 1.0, 2.0,      // data/feature matrix
                                 2.0, 1.0,
                                 5.0, 4.0,
                                 4.0, 5.0,
                                 9.0, 8.0,
                                 8.0, 9.0)
    val y  = VectorI (0, 0, 0, 1, 1, 1)         // classification for each vector in x
    val fn = Array ("x1", "x2")                 // feature/variable names
    val cn = Array ("No", "Yes")                // class names

    println ("----------------------------------------------------")
    println ("x = " + x)
    println ("y = " + y)
    val cl = new KNN_Classifier (x, y, fn, 2, cn)

    cl.train ()
    val z1 = VectorD (10.0, 10.0)
    println ("----------------------------------------------------")
    println ("z1 = " + z1)
    println ("class = " + cl.classify (z1))

    cl.train ()
    val z2 = VectorD ( 3.0,  3.0)
    println ("----------------------------------------------------")
    println ("z2 = " + z2)
    println ("class = " + cl.classify (z2))

} // KNN_ClassifierTest object

