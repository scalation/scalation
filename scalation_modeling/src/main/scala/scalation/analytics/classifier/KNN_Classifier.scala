
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Sep 22 18:45:44 EDT 2013 
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.plot.Plot
import scalation.random.Bernoulli

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_Classifier` class is used to classify a new vector 'z' into one of
 *  'k' classes.  It works by finding its 'kappa' nearest neighbors.  These neighbors
 *  essentially vote according to their classification.  The class with most
 *  votes is selected as the classification of 'z'.  Using a distance metric,
 *  the 'kappa' vectors nearest to 'z' are found in the training data, which is
 *  stored row-wise in the data matrix 'x'.  The corresponding classifications
 *  are given in the vector 'y', such that the classification for vector 'x(i)'
 *  is given by 'y(i)'.
 *  @param x    the vectors/points of classified data stored as rows of a matrix
 *  @param y    the classification of each vector in x
 *  @param fn   the names for all features/variables
 *  @param k    the number of classes
 *  @param cn   the names for all classes
 *  @param kappa  the number of nearest neighbors to consider
 */
class KNN_Classifier (x: MatriD, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                      kappa: Int = 3)
      extends ClassifierReal (x, y, fn, k, cn)
{
    private val DEBUG      = true                                // debug flag
    private val MAX_DOUBLE = Double.PositiveInfinity             // infinity
    private val topK       = Array.fill (kappa)(-1, MAX_DOUBLE)  // top-kappa nearest points (in reserve order)
    private val count      = new VectorI (k)                     // how many nearest neighbors in each class.
    private val coin       = Bernoulli ()                        // use a fair coin for breaking ties

    if (DEBUG) println (s" x = $x \n y = $y")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a distance metric between vectors/points 'x' and 'z'.
     *  The squared Euclidean norm used for efficiency, but may use other norms.
     *  @param x  the first vector/point
     *  @param z  the second vector/point
     */
    def distance (x: VectoD, z: VectoD): Double = (x - z).normSq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'kappa' nearest neighbors (top-'kappa') to vector 'z' and store in
     *  the 'topK' array.  Break ties by flipping a fair coin.
     *  @param z  the vector to be classified
     */
    def kNearest (z: VectoD)
    {
        var dk = MAX_DOUBLE
        for (i <- x.range1) {
            val di = distance (z, x(i))                          // compute distance to z
            if (di < dk) dk = replaceTop (i, di)                 // if closer, adjust top-kappa
            else if (di == dk && coin.igen == 1) replaceTop (i, di)    // for breaking ties, may comment out
        } // for
        if (DEBUG) println (s"z = $z: topK = ${topK.deep}")
    } // kNearest

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Training involves resetting the data structures before each classification.
     *  It uses lazy training, so most of it is done during classification.
     *  @param itest  the indices of the test data
     */
    def train (itest: IndexedSeq [Int]): KNN_Classifier =        // FIX - use this parameters
    {
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector 'z', determine which class it belongs to (i.e.,
     *  the class getting the most votes from its 'kappa' nearest neighbors.
     *  Return the best class, its name and its votes
     *  @param z  the vector to classify
     */
    override def classify (z: VectoD): (Int, String, Double) =
    {
        kNearest (z)                                             // set top-kappa to kappa nearest
        for (i <- 0 until kappa) count(y(topK(i)._1)) += 1       // tally votes per class
        val best = count.argmax ()                               // class with maximal count
        reset ()                                                 // reset topK and counters
        (best, cn(best), count(best))                            // return the best class, its name and votes
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the most distant neighbor and add new neighbor 'i'.  Maintain the
     *  'topK' nearest neighbors in sorted order farthest to nearest.
     *  @param i   new neighbor to be added
     *  @param di  distance of the new neighbor
     */
    private def replaceTop (i: Int, di: Double): Double =
    {
        var j = 0
        while (j < kappa-1 && di < topK(j)._2) { topK(j) = topK(j+1); j += 1 }
        topK(j) = (i, di)
        topK(0)._2                          // the distance of the new farthest neighbor
    } // replaceTop

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize 'topK' and counters.
     */
    def reset ()
    {
        for (i <- 0 until kappa) topK(i)  = (-1, MAX_DOUBLE)     // initialize top-kappa
        for (j <- 0 until k) count(j) = 0                        // initialize counters
    } // reset

} // KNN_Classifier class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_Classifier` companion object provides a factory method.
 */
object KNN_Classifier
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `KNN_Classifier` classifier from a joint 'xy' matrix
     *  @param x    the vectors/points of classified data stored as rows of a matrix
     *  @param y    the classification of each vector in x
     *  @param fn   the names for all features/variables
     *  @param k    the number of classes
     *  @param cn   the names for all classes
     *  @param kappa  the number of nearest neighbors to consider
     */
    def apply (xy: MatriD, fn: Array [String], k: Int, cn: Array [String], kappa: Int = 3): KNN_Classifier =
    {
        new KNN_Classifier (xy.sliceCol (0, xy.dim2-1), xy.col (xy.dim2-1).toInt, fn, k, cn, kappa)
    } // apply

} // KNN_Classifier object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_ClassifierTest` object is used to test the `KNN_Classifier` class.
 *  > runMain scalation.analytics.classifier.KNN_ClassifierTest
 */
object KNN_ClassifierTest extends App
{
    //                            x1 x2  y
    val xy = new MatrixD ((10, 3), 1, 5, 1,       // joint data matrix
                                   2, 4, 1,
                                   3, 4, 1,
                                   4, 4, 1,
                                   5, 3, 0,
                                   6, 3, 1,
                                   7, 2, 0,
                                   8, 2, 0,
                                   9, 1, 0,
                                  10, 1, 0)

    val fn = Array ("x1", "x2")                   // feature/variable names
    val cn = Array ("No", "Yes")                  // class names

    println ("----------------------------------------------------")
    println ("xy = " + xy)

    val cl = KNN_Classifier (xy, fn, 2, cn)

    val z1 = VectorD (10.0, 10.0)
    println ("----------------------------------------------------")
    println ("z1 = " + z1)
    println ("class = " + cl.classify (z1))

    val z2 = VectorD ( 3.0,  3.0)
    println ("----------------------------------------------------")
    println ("z2 = " + z2)
    println ("class = " + cl.classify (z2))

    val y  =  xy.col (xy.dim2-1).toInt
    val yp = VectorI (for (i <- xy.range1) yield cl.classify (xy(i).slice (0, xy.dim2-1))._1)
    println (cl.actualVpredicted (y, yp))

    new Plot (xy.col(0), y.toDouble, yp.toDouble)

} // KNN_ClassifierTest object

