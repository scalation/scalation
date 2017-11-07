
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vinay Kumar Bingi, Supriya Ramireddy
 *  @version 1.4
 *  @date    Fri Oct  20 15:41:16 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package graph_algebra

import scala.collection.mutable.{ArrayBuffer, Map, Set => SET}
import scala.reflect.ClassTag

import scalation.linalgebra.VectorS
import scalation.random.{Randi,Randi0, RandomSetW, RandomVecS,Bernoulli}
import scalation.math.double_exp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'RandomGraph' class is used to generate the random graphs of type 'MuGraph'
 *  and also the create statement for creating graph in Neo4j
 *  @param nVertices         number of vertices to be created
 *  @param nLabels           number of different labels for vertices
 *  @param nEdges            number of edges
 *  @param nELabels          number of edge labels
 *  @param maxElabelPerEdge  maximum number edge labels per edge
 *  @param schemaType        type of the vertices
 *  @param stream            the stream of words to be generated
 */
class RandomGraph [TLabel: ClassTag] (nVertices: Int, nLabels: Int, nEdges: Int, nELabels: Int = 2, maxElabelPerEdge: Int = 2,
                   schemaType: Array[String] = Array ("Person", "Movie"), stream: Int = 0)
{
    val eLabels = genElabels
    val schema  = assignSchema
    val lv      = assignVLabels (genVLabels)
    val le      = assignElabels (eLabels)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the random graph of type 'MuGraph' and the 'create' statement for
     *  creating the graph in Neo4j.
     */
    def gen: (MuGraphAlgebra [String], String) =
    {
        val mg = MuGraph (lv, le, null, true, schema)
        val ng = createNeo
        val dg = MuGraphAlgebra (mg)
        (dg, ng)
    } // gen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign the type (schema) to all the vertices.
     */
    def assignSchema: Array [String] =
    {
        var aschema   = Array.ofDim [String] (nVertices)
        val bernoulli = Bernoulli (0.2)
        for (i <- 0 until nVertices) aschema(i) = schemaType(bernoulli.igen)
        aschema
    } // assignSchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the labels for vertices.
     */
    def genVLabels: VectorS = RandomVecS (nLabels, true, stream).sgen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assigns the vertex labels to the vertices.
     *  @param  vlabs the vector of labels to be assigned for vertices
     */
    def assignVLabels (vlabs: VectorS): Array [String] =
    {
        val lv     = Array.ofDim [String] (nVertices)
        val rLabel = Randi0 (nLabels - 1, stream)                         // replaced nLables with nVertices
        for(i <- 0 until nVertices) lv(i) = vlabs(rLabel.igen).toString ()
        lv
    } // assignVLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the labels for edges.
     */
    def genElabels: SET [String] = RandomSetW (nELabels, 2 * nELabels).sgen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pick the labels to be assigned for the edges.
     *  @param  elabs the set of edge labels to be used for edges
     */
    def pickELabels (elabs: SET [String]): SET [String] =
    {
        val rLabel = new Randi0 (maxElabelPerEdge, stream)
        rLabel.igen match {
        case 0 => SET (elabs.head)
        case 1 => elabs.tail
        case _ => elabs
        } // match
    } // pickELabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign the labels to edges.
     *  @param elabs  the set of edge labels to be used for edges
     */
    def assignElabels (elabs: SET [String]): Map [Pair, SET [String]] =
    {
        val le    = Map [Pair, SET [String]] ()
        val rVert = Randi0 (nVertices-1, stream)
        for(i <- 0 until nEdges) {
            var sv = 0
            var ev = 0
            do { sv = rVert.igen; ev = rVert.igen
            } while (sv == ev)
            le += (sv,ev) -> pickELabels (elabs)
        } // for
        le
    } // assignElabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the 'CREATE' statement to be used for creating the data graph in Neo4j.
     */
    def createNeo: String =
    {
        val var_labs = RandomVecS (nVertices, true, stream).sgen
        val rLabel   =  Randi0 (nVertices-1, stream)
        val vl = Array.ofDim [String] (nVertices)                 // for assigning variable names for the vertices
        for (i <- 0 until nVertices) vl(i) = var_labs(i).toString

        var nq = "CREATE "
        for (i <- 0 until nVertices) nq += "(" + vl(i) + ":" + schema(i) + " {name: " + "\"" + lv(i) + "\"}),\n"
        for ((k,v) <- le) nq += "(" + vl(k._1) + ") - [:" + v.toSeq(0) + "] -> (" + vl(k._2) + "),\n" //v.toSeq(0) has to be fixed
        for ((k,v) <- le) nq += "(" + vl(k._1) + ") - [:" + v.toSeq(1) + "] -> (" + vl(k._2) + "),\n"
        nq = nq.dropRight (2)
        nq
    } // createNeo

} // RandomGraph class

