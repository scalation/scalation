
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Wed Aug 24 11:41:48 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Query` class provides a means for entering queries as a sequence
 *  of triples.  A triple may be viewed as (subject, predicate, object), (s, p, o)
 *  or (source vertex, edge, target vertex).  Support for wildcards, regexes
 *  and variables are provided.
 *  @param triples  the sequence of triples forming the query
 */
case class Query [TLabel: ClassTag] (triples: (TLabel, TLabel, TLabel)*)
{
    private val DEBUG  = true                      // debug flag
    private var qc     = 0                         // query counter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an `MGraph` from the sequence of triples.
     *  @param inverse  whether to create parent references
     */
    def buildGraph (inverse: Boolean = false): MGraph [TLabel] =
    {
        var count = 0                              // vertex counter
        val vMap  = Map [TLabel, Int] ()
        for (t <- triples) {
            if (! (vMap contains t._1)) { vMap += t._1 -> count; count += 1 }
            if (! (vMap contains t._3)) { vMap += t._3 -> count; count += 1 }
        } // for

        if (DEBUG) { println ("vMap = "); for (entry <- vMap) println (entry) }

        val ch = Array.ofDim [SET [Int]] (count)
        for (i <- ch.indices) ch(i) = SET [Int] ()
        val label  = Array.ofDim [TLabel] (count)
        val elabel = Map [Pair, TLabel] ()

        for (t <- triples) {
            val (u, v) = (vMap (t._1), vMap (t._3))
            if (DEBUG) println ("(u, v) = " + (u, v))
            ch (u) += v
            label (u) = t._1
            label (v) = t._3
            elabel += (u, v) -> t._2
        } // for
        qc += 1
        new MGraph (ch, label, elabel, inverse, "qg" + qc)
    } // buildGraph

} // Query class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Query` companion object provides methods that work on triples.
 */
object Query
{
    import scalation.util.Wildcard.hasWildcards

    val STR_REGEX      = "\\.r"                   // suffix indicating its a regular expression
    val CHAR_VARIABLE  = '?'                      // treat label as a variable

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine which parts (s, p, o)/(1, 2, 3) of the triple have wildcards,
     *  returning the set labels, e.g., {2, 3} that have one.  A wildcard is
     *  indicated by the existence of a 'CHAR_WILD' character anywhere in a label.
     *  @param triple  the triple to be check for wildcards
     */
    def hasWildcard [TLabel] (triple: (TLabel, TLabel, TLabel)): SET [Int] =
    {
        val s = SET [Int] ()
        if (triple._1.isInstanceOf [String])
            if (hasWildcards (triple._1))      s += 1
            else if (hasWildcards (triple._2)) s += 2
            else if (hasWildcards (triple._3)) s += 3
        s
    } // hasWildcard

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine which parts (s, p, o)/(1, 2, 3) of the triple include regexes,
     *  returning the set labels, e.g., {2, 3} that have one.  A regex is
     *  indicated when a label ends with 'STR_REGEX'.
     *  @param triple  the triple to be check for regexes
     */
    def hasRegex [TLabel] (triple: (TLabel, TLabel, TLabel)): SET [Int] =
    {
        val s = SET [Int] ()
        if (triple._1.isInstanceOf [String])
            if (triple._1.asInstanceOf [String].endsWith (STR_REGEX))      s += 1
            else if (triple._2.asInstanceOf [String].endsWith (STR_REGEX)) s += 2
            else if (triple._3.asInstanceOf [String].endsWith (STR_REGEX)) s += 3
        s
    } // hasRegex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine which parts (s, p, o)/(1, 2, 3) of the triple have variables,
     *  returning the set labels, e.g., {2, 3} that have one.  A variable is
     *  indicated when a label starts with 'CHAR_VARIABLE'.
     *  @param triple  the triple to be check for variables
     */
    def hasVariable [TLabel] (triple: (TLabel, TLabel, TLabel)): SET [Int] =
    {
        val s = SET [Int] ()
        if (triple._1.isInstanceOf [String])
            if (triple._1.asInstanceOf [String].charAt(0) == CHAR_VARIABLE)      s += 1
            else if (triple._2.asInstanceOf [String].charAt(0) == CHAR_VARIABLE) s += 2
            else if (triple._3.asInstanceOf [String].charAt(0) == CHAR_VARIABLE) s += 3
        s
    } // hasVariable

} // Query object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QueryTest` object is used to test the `Query` class.  It converts
 *  a sequence of triples into a graph.
 *  > run-main scalation.graph_db.pattern_matching.QueryTest
 */
object QueryTest extends App
{
    val q = Query (("Marcel Proust", "wrote", "In Search of Lost Time"),
                   ("James Joyce", "wrote", "Ulysses"),
                   ("Don Quixote", "wrote", "Miguel de Cervantes"),
                   ("Herman Melville", "wrote", "Moby Dick"),
                   ("Marcel Proust", "read", "Ulysses"))

    println ("q  = " + q)
    q.buildGraph ().printG ()

} // QueryTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QueryTest2` object is used to test the `Query` class.  It checks if
 *  variables or wildcards exist in a triple's labels.
 *  > run-main scalation.graph_db.pattern_matching.QueryTest2
 */
object QueryTest2 extends App
{
    import Query._

    val triples = Seq (("Marcel Proust", "wrote", "In Search of Lost Time"),
                       ("?author", "wrote", "In Search of Lost Time"),
                       ("Marcel Proust", "wrote", "I_ Search of Lost Time"),
                       ("Marcel Proust", "wrote", "In % of Lost Time"),
                       ("Marcel Proust", "wrote", "In Search [a-z]+ Lost Time\\.r"))

    for (t <- triples) {
        println ("t = " + t)
        println ("hasWildcard = " + hasWildcard (t))  
        println ("hasRegex = "    + hasRegex (t))  
        println ("hasVariable = " + hasVariable (t))  
    } // for

} // QueryTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QueryTest3` object is used to test the `Query` class.  It converts
 *  a sequence of triples into a graph.
 *  > run-main scalation.graph_db.pattern_matching.QueryTest3
 */
object QueryTest3 extends App
{
    import Query._

//  val g = ReadCSVFile.readCSV ()

    // Original
    val q = Query (("The Three Musketeers", "WrittenBy", "Alexandre Dumas"),
                   ("In Search of Lost Time", "WrittenBy", "Marcel Proust")).buildGraph ()

    // Wildcards
//  val q = Query (("T_e Thr_e Muske%", "WrittenB_", "Ale_andre Du%"),
//                 ("In Sear_h of Lost T%", "Wri%", "Mar_el Prou%")).buildGraph ()

    // Regex
//  val q = Query (("The [A-Z][a-z]+ Musketeers\\.r", "[a-z][A-Z]+\\.r", "Alexandre [A-Z][a-z]+\\.r"),
//                 ("In [A-Z][a-z]+ of Lost Time\\.r", "[a-z][A-Z]+\\.r", "Marcel [A-Z][a-z]+\\.r")).buildGraph ()

    // Variable
//  val q = Query (("The Three Musketeers", "?x", "?author"),
//                 ("?Book", "?x", "Marcel Proust")).buildGraph ()

//  val q = Query (("?Book", "?x", "Alexandre Dumas")).buildGraph ()

    // Edges
//  val q = Query (("The Three Musketeers", "Wri_ten%", "Alexandre Dumas")).buildGraph ()
//  val q = Query (("The Three Musketeers", "[a-z][A-Z]+\\.r", "Alexandre Dumas")).buildGraph ()
//  val q = Query (("The Three Musketeers", "?x", "Alexandre Dumas")).buildGraph ()

    q.printG ()
//  g.printG ()

//  val res = VariableQuery.variableMatching (g, q, s)
//  (new MDualSimWRV (g, q)).test ("MDualSimWRV")
//  (new GraphSim (g, q)).test ("GraphSim")
//  (new DualSim (g, q)).test ("DualSim")

     // Edges
//   (new MDualSimWRV (g, q)).test ("MDualSimWRV")

} // QueryTest3 object

