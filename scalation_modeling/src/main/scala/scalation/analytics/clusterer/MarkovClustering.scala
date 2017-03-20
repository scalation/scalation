
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *  @see     www.cs.ucsb.edu/~xyan/classes/CS595D-2009winter/MCL_Presentation2.pdf
 *  @see     www.ebi.ac.uk/enright/docs/stijnmcl.pdf
 */

package scalation.analytics.clusterer

import scala.collection.mutable.ListMap
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectorD, VectorI}
//import scalation.linalgebra.SparseMatrixD
import scalation.math.double_exp
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MarkovClusterer` class implements a Markov Clustering Algorithm 'MCL'
 *  and is used to cluster nodes in a graph.  The graph is represented as an
 *  edge-weighted adjacency matrix (a non-zero cell indicates nodes i and j are
 *  connected).
 *  <p>
 *  The primary constructor takes either a graph (adjacency matrix) or a
 *  Markov transition matrix as input.  If a graph is passed in, the normalize
 *  method must be called to convert it into a Markov transition matrix.
 *  Before normalizing, it may be helpful to add self loops to the graph.
 *  The matrix (graph or transition) may be either dense or sparse.
 *  See the `MarkovClustererTest` object at the bottom of the file for examples.
 *  @param t  either an adjacency matrix of a graph or a Markov transition matrix
 *  @param k  the strength of expansion
 *  @param r  the strength of inflation
 */
class MarkovClusterer (t: MatrixD, k: Int = 2, r: Double = 2.0)
      extends Clusterer with Error
{
    private val DEBUG    = false           // debug flag
    private val MAX_ITER = 200             // maximum number of iterations
    private val EPSILON  = 1E-7            // number close to zero

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the centroids.  Should only be called after 'cluster ()'.
     */
    def centroids (): MatrixD = throw new UnsupportedOperationException ("not applicable")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sizes of the centroids.  Should only be called after
     *  'cluster ()'.
     */
    def csize (): VectorI = throw new UnsupportedOperationException ("not applicable")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add self-loops by setting the main diagonal to the weight parameter.
     *  @param weight  the edge weight on self-loops to be added.
     */
    def addSelfLoops (weight: Double = 1.0)
    {
        t.setDiag (weight)                 // add self-loops (make diagonal 1)
    } // addSelfLoops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize the matrix t so that each column sums to 1, i.e.0, convert
     *  the adjacency matrix of a graph into a Markov transition matrix.
     */
    def normalize ()
    {
        val sum = new VectorD (t.dim2)                  // to hold column sums
        for (j <- 0 until t.dim2) sum(j) = t.col(j).sum
        for (j <- 0 until t.dim2) t.setCol (j, t.col(j) / sum(j))
    } // normalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expansion tends to grow clusters (flow along path in graph).
     *  Expand by raising the matrix t to the k-th power.
     */
    def expand ()
    {
        for (l <- 1 until k) t *= k          // in-place matrix multiplication
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Inflation tends to strengthen strong connections and weaken weak ones.
     *  Inflate by raising each cell to the r-th power and normalize column-by-column.
     *  If a cell is close to zero, set it to zero (prune).  Also, detect
     *  convergence by making sure that the variance in each column is small
     *  enough.
     */
    def inflate (): Boolean =
    {
        var done = true
        for (j <- 0 until t.dim2) {
            var sum   = 0.0                       // column sum
            var sumSq = 0.0                       // column sum of squares
            var n     = 0.0                       // number of non-zero entries in column

            for (i <- 0 until t.dim1) {
                if (t(i, j) < EPSILON) {
                    t(i, j) = 0.0                 // prune this cell
                } else {
                    t(i, j) = t(i, j) ~^ r        // raise cell (i, j) to the r-th power
                    sum   += t(i, j)              // collect sum
                    sumSq += t(i, j) * t(i, j)    // collect sum of squares
                    n += 1.0
                } // if
            } // for

            for (i <- 0 until t.dim1) t(i, j) /= sum             // normalize
            if (sumSq - (sum * sum) / n > EPSILON) done = false  // variance in column too high
        } // for
        done                                      // whether convergence has been detected
    } // inflate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the processed matrix t.  The matrix is processed by repeated
     *  steps of expansion and inflation until convergence is detected.
     */
    def processMatrix (): MatrixD = 
    {
        breakable { for (l <- 1 to MAX_ITER) {
            expand ()                         // expansion step
            if (inflate ()) break             // inflation step with convergence check
            if (DEBUG) println ("(" + l + ") t = " + t)
        }} // for   
        t
    } // processMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cluster the nodes in the graph by interpreting the processed matrix t.
     *  Nodes not clustered will be in group 0; otherwise, they will be grouped
     *  with their strongest positive attractor.
     */
    def cluster (): Array [Int] =
    {
        val clust = Array.ofDim [Int] (t.dim2)     // vector of cluster assignments
        val force = new VectorD (t.dim2)           // force of attractor, initially 0
        var group = 1                              // first real group is 1
        for (i <- 0 until t.dim1) {
            var found = false
            for (j <- 0 until t.dim2) {
                if (t(i, j) > force(j)) {          // if attractor has greater force
                    clust(j) = group               // assign node j to this group
                    force(j) = t(i, j)             // make t(i, j) the new force
                    found = true                   // a group was found for this row
                } // if
            } // for
            if (found) group += 1                  // increment the group number
        } // for
        clust                      // return the vector of cluster assignments
    } // cluster

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This clustering method is not applicable to graph clustering.
     *  @param y  unused parameter
     */
    def classify (y: VectorD): Int =
    {
        throw new UnsupportedOperationException ()
    } // classify

} // MarkovClusterer class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MarkovClustererTest` object is used to test the `MarkovClusterer` class.
 *  @see www.cs.ucsb.edu/~xyan/classes/CS595D-2009winter/MCL_Presentation2.pdf
 ^  > run-main scalation.analytics.clusterer.MarkovClustererTest
 */
object MarkovClustererTest extends App
{
    import scalation.linalgebra.SparseMatrixD

    // Test the MCL Algorithm on a graph represented as an adjacency matrix.

    val g = new MatrixD ((12, 12),
        0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0,  0.0,
        1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0.0,
        0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0.0,
        0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0,  0.0,
        0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0,  0.0,
        1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,  0.0,
        1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,  0.0,
        0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0,  0.0,
        0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0,  1.0,
        1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0,  0.0,
        0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0,  1.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,  0.0) 

    println ("-----------------------------------------------------------")
    println ("g = " + g)
    val mg = new MarkovClusterer (g)
    mg.addSelfLoops ()
    mg.normalize ()
    println ("result  = " + mg.processMatrix ())
    println ("cluster = " + mg.cluster ())

    // Test the MCL Algorithm on a Markov transition matrix.

    val t = new MatrixD ((12, 12),
        0.2, 0.25, 0.0,  0.0, 0.0, 0.333, 0.25, 0.0, 0.0, 0.25, 0.0, 0.0,
        0.2, 0.25, 0.25, 0.0, 0.2, 0.0,   0.0,  0.0, 0.0, 0.0,  0.0, 0.0,
        0.0, 0.25, 0.25, 0.2, 0.2, 0.0,   0.0,  0.0, 0.0, 0.0,  0.0, 0.0,
        0.0, 0.0,  0.25, 0.2, 0.0, 0.0,   0.0,  0.2, 0.2, 0.0,  0.2, 0.0,
        0.0, 0.25, 0.25, 0.0, 0.2, 0.0,   0.25, 0.2, 0.0, 0.0,  0.0, 0.0,
        0.2, 0.0,  0.0,  0.0, 0.0, 0.333, 0.0,  0.0, 0.0, 0.25, 0.0, 0.0,
        0.2, 0.0,  0.0,  0.0, 0.2, 0.0,   0.25, 0.0, 0.0, 0.25, 0.0, 0.0,
        0.0, 0.0,  0.0,  0.2, 0.2, 0.0,   0.0,  0.2, 0.2, 0.0,  0.2, 0.0,
        0.0, 0.0,  0.0,  0.2, 0.0, 0.0,   0.0,  0.2, 0.2, 0.0,  0.2, 0.333,
        0.2, 0.0,  0.0,  0.0, 0.0, 0.333, 0.25, 0.0, 0.0, 0.25, 0.0, 0.0,
        0.0, 0.0,  0.0,  0.2, 0.0, 0.0,   0.0,  0.2, 0.2, 0.0,  0.2, 0.333,
        0.0, 0.0,  0.0,  0.0, 0.0, 0.0,   0.0,  0.0, 0.2, 0.0,  0.2, 0.333) 

    println ("-----------------------------------------------------------")
    println ("t = " + t)
    val mt = new MarkovClusterer (t)
    println ("result  = " + mt.processMatrix ())
    println ("cluster = " + mt.cluster ())

    // Test the MCL Algorithm on a graph represented as a sparse adjacency matrix.

//  val x = new SparseMatrixD (12, 12)
//  x(0)  = ListMap ((1, 1.0), (5, 1.0), (6, 1.0), (9, 1.0))
//  x(1)  = ListMap ((0, 1.0), (2, 1.0), (4, 1.0))
//  x(2)  = ListMap ((1, 1.0), (3, 1.0), (4, 1.0))
//  x(3)  = ListMap ((2, 1.0), (7, 1.0), (8, 1.0), (10, 1.0))
//  x(4)  = ListMap ((1, 1.0), (2, 1.0), (6, 1.0), (7, 1.0))
//  x(5)  = ListMap ((0, 1.0), (9, 1.0))
//  x(6)  = ListMap ((0, 1.0), (4, 1.0), (9, 1.0))
//  x(7)  = ListMap ((3, 1.0), (4, 1.0), (8, 1.0), (10, 1.0))
//  x(8)  = ListMap ((3, 1.0), (7, 1.0), (10, 1.0), (11, 1.0))
//  x(9)  = ListMap ((0, 1.0), (5, 1.0), (6, 1.0))
//  x(10) = ListMap ((3, 1.0), (7, 1.0), (8, 1.0), (11, 1.0))
//  x(11) = ListMap ((8, 1.0))

//  println ("-----------------------------------------------------------")
//  println ("x = " + x)
//  val mx = new MarkovClusterer (x)
//  mx.addSelfLoops ()
//  mx.normalize ()
//  println ("result  = " + mx.processMatrix ())
//  println ("cluster = " + mx.cluster ())

} // MarkovClustererTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MarkovClustererTest2` object is used to test the `MarkovClusterer` class.
 ^  > run-main scalation.analytics.clusterer.MarkovClustererTest2
 */
object MarkovClustererTest2 extends App
{
    // Test the MCL Algorithm on a randomly generated graph represented as an adjacency matrix.

    val rg = new RandomGraph (1000, .05, 10)
    val y  = rg.gen ()
    println ("-----------------------------------------------------------")
    val t0 = System.nanoTime ()
    val my = new MarkovClusterer (y)
    my.addSelfLoops ()
    my.normalize ()
    my.processMatrix ()
    val cluster = my.cluster ()
    println ("Elapsed time = " + (System.nanoTime - t0) + " ns")
    println ("-----------------------------------------------------------")
    println ("cluster = " + cluster)

} // MarkovClustererTest2 object

