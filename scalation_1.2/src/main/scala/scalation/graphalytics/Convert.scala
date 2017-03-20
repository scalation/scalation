
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Jul 31 13:54:40 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

import scalation.linalgebra.{MatriI, MatrixI, SparseMatrixI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Convert` object is used to convert between an Adjacency Matrix
 *  representation to an Adjacency Sets representation.
 */
object Convert
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the graph from an Adjacency Matrix representation to an
     *  Adjacency Sets representation.
     *  @param mat  the Adjacency Matrix representation of a graph
     */
    def matrix2Graph (mat: MatriI): Graph =
    {
        val n = mat.dim1
        val ch = Array.ofDim [SET [Int]] (n)
        for (i <- 0 until n) {
            ch(i) = Set [Int] ()
            for (j <- 0 until n if ! (mat(i, j) == 0)) ch(i) += j
        } // for
        new Graph (ch)
    } // matrix2Graph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the graph from an Adjacency Sets representation to an
     *  Adjacency Matrix representation.
     *  @param mat  the Adjacency Sets representation of a graph
     */
    def graph2Matrix (gr: Graph, sparse: Boolean = false): MatriI =
    {
        val n   = gr.ch.length
        val mat = if (sparse) new SparseMatrixI (n, n) else new MatrixI (n, n)
        for (i <- 0 until n; j <- 0 until n if gr.ch(i) contains j) mat(i, j) = 1
        mat
    } // graph2Matrix

} // Convert object


import Convert._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConvertTest` object is used to test the `Convert` object.
 *  > run-main scalation.graphalytics.ConvertTest
 */
object ConvertTest extends App
{
    val m1 = new MatrixI ((3, 3), 0, 1, 1,
                                  0, 0, 1,
                                  0, 1, 0)
    val g  = matrix2Graph (m1)
    val m2 = graph2Matrix (g)

    println ("m1 = " + m1)
    g.printG ()
    println ("m2 = " + m2)

} // ConvertTest

