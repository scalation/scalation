
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Vinay Bingi
 *  @version 1.6
 *  @date    Mon Jun 25 16:56:30 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package columnar_db

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

import scalation.linalgebra.Vec
//import scalation.math.{Complex, Rational, Real}
//import scalation.math.StrO.StrNum
import scalation.util.{banner, ReArray}

import TableObj._
import columnar_db._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationSQL` class provides an SQL-like API to data stored internally
 *  in a `Relation` object.
 *  @param name     the name of the relation
 *  @param colName  the names of columns
 *  @param col      the Scala Vector of columns making up the columnar relation
 *  @param key      the column number for the primary key (< 0 => no primary key)
 *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
 *  @param fKeys    an optional sequence of foreign keys - Seq (column name, ref table name, ref column position)
 */
class RelationSQL (name: String, colName: Seq [String], col: Vector [Vec],
                   key: Int = 0, domain: String = null, fKeys: Seq [(String, String, Int)] = null)
      extends Tabular with Serializable
{
    /** the debug flag
     */
    private val DEBUG = true

    /** the internal representation - calss delegates work to `Relation` operations
     */
    private val r = new Relation (name, colName, col, key, domain, fKeys)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the internal representation.
     */
    def repr: Relation = r

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a new `RelationSQL` object from an existing relation 'r'.
     *  @param r  the existing relation
     */
    def this (r: Relation) = this (r.name, r.colName, r.col, r.key, r.domain, r.fKeys)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select the attributes to return in the answer to the query.
     *  @param cName  the attribute names
     */
    def select (cName: String*): RelationSQL = new RelationSQL (repr.project (cName :_*))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing a "natural-join".
     *  @param r2  the other relation
     */
    def join (r2: RelationSQL): RelationSQL = new RelationSQL ((repr join r2.repr).asInstanceOf [Relation])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The where function filters on predicates (logic is and),
     *  returning the relation satisfying the predicates (column compare with constant)
     *  @param  p  tuple(1): column name, tuple(2): predicate (T => Boolean)
     *  @tparam T  the predicate type
     */
    def where [T: ClassTag] (p: Predicate [T]*): RelationSQL =
    {
        var pos = ArrayBuffer [Int] ()
        for (i <- p.indices) {
            val p_i = p(i)
            val pos1 = Vec.filterPos (col(repr.colMap(p_i._1)), p_i._2)
            if (DEBUG) println (s"where: p_$i = ${p_i}, pos1 = $pos1")
            if (i > 0) pos = pos intersect pos1 else pos ++= pos1
        } // for
        new RelationSQL (repr.selectAt (pos))
    } // where

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group 'this' relation by the specified column names, returning 'this' relation.
     *  @param cName  the group column names
     */
    def groupBy (cName: String*): RelationSQL = new RelationSQL (repr.groupBy (cName :_*))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (ascending) the rows in the relation by the selected columns '_cName'.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def orderBy (cName: String*): RelationSQL = new RelationSQL (repr.orderBy (cName :_*))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (descending) the rows in the relation by the selected columns '_cName'.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def reverseOrderBy (cName: String*): RelationSQL = new RelationSQL (repr.reverseOrderBy (cName :_*))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  If they are not, return the first 'this' relation.
     *  @param r2  the other relation
     */
    def union (r2: RelationSQL): RelationSQL = new RelationSQL (repr union r2.repr)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  Use index to finish intersect operation.
     *  @param r2  the other relation
     */
    def intersect (r2: RelationSQL): RelationSQL = new RelationSQL (repr intersect r2.repr)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of 'this' relation and 'r2' ('this - r2').  Check that
     *  the two relations are compatible.
     *  @param r2  the other relation
     */
    def minus (r2: RelationSQL): RelationSQL = new RelationSQL (repr minus r2.repr)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show 'this' relation row by row.
     *  @param limit  the limit on the number of rows to display
     */
    def show (limit: Int = Int.MaxValue) { r.show (limit) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save 'this' relation in a file using serialization.
     */
    def save () { r.save () }

} // RelationSQL class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationSQL` companion object provides factory methods for creating `RelationSQL`
 *  object.
 */
object RelationSQL
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a SQL-relation from a sequence of row/tuples.  These rows must be converted
     *  to columns.
     *  @param relName  the name of the relation
     *  @param colName  the names of columns
     *  @param row      the sequence of rows to be converted to columns for the columnar relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def apply (relName: String, colName: Seq [String], row: Seq [Row], key: Int, domain: String): RelationSQL =
    {
        val equivCol = Vector.fill [Vec] (colName.length)(null)
        val r2 = new Relation (relName, colName, equivCol, key, domain)
        for (tuple <- row) r2.add (tuple)
        new RelationSQL (r2.materialize ())
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a SQL-relation from a saved relation.
     *  @param relName  the name of the relation
     */
    def apply (relName: String): RelationSQL = new RelationSQL (Relation (relName))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a SQL-relation from a saved relation.
     *  FIX - should handle other than 2 relations
     *  @param relNames  the names of the relations
     */
    def from (relNames: String*): (RelationSQL, RelationSQL) =
    {
        (Catalog.getRelation (relNames(0)).asInstanceOf [RelationSQL],
         Catalog.getRelation (relNames(1)).asInstanceOf [RelationSQL])
    } // from

} // RelationSQL object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationSQLTest` object is used to t3est the `RelationSQL` class.
 *  > runMain scalation.columnar_db.RelationSQLTest
 */
object RelationSQLTest extends App
{
    val professor = RelationSQL ("professor",
        Seq ("pid", "name", "department", "title"),
        Seq (Vector [Any] (1, "jackson", "pharm", 4),
             Vector [Any] (2, "ken", "cs", 2),
             Vector [Any] (3, "pan", "pharm", 0),
             Vector [Any] (4, "yang", "gis", 3),
             Vector [Any] (5, "zhang", "cs", 0),
             Vector [Any] (6, "Yu", "cs", 0)),
        -1, "ISSI")

    val professor2 = RelationSQL ("professor2",
        Seq ("pid", "name", "department", "title"),
        Seq (Vector [Any] (7, "LiLy", "gis", 5),
             Vector [Any] (8, "Marry", "gis", 5),
             Vector [Any] (0, "Kate", "cs", 5)),
        0, "ISSI")

    banner ("professor")
    professor.show ()

    banner ("professor2")
    professor2.show ()

    banner ("Example SQL-like Query")
    (professor join professor2)
        .where [String] (("department", (x: String) => x == "cs"))
        .groupBy ("title")
        .select ("pid", "name", "department", "title")

} // RelationSQLTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationSQLTest2` object tests 'save' method.
 *  > runMain scalation.columnar_db.RelationSQLTest2
 */
object RelationSQLTest2 extends App
{
    val professor = Relation ("professor",
        Seq ("pid", "name", "department", "title"),
        Seq (Vector [Any] (1, "jackson", "pharm", 4),
             Vector [Any] (2, "ken", "cs", 2),
             Vector [Any] (3, "pan", "pharm", 0),
             Vector [Any] (4, "yang", "gis", 3),
             Vector [Any] (5, "zhang", "cs", 0),
             Vector [Any] (6, "Yu", "cs", 0)),
        -1, "ISSI")

    professor.save ()

} // RelationSQLTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationSQLTest3` object tests 'apply' method to load a saved relation.
 *  > runMain scalation.columnar_db.RelationSQLTest3
 */
object RelationSQLTest3 extends App
{
    Relation ("professor").show ()

} // RelationSQLTest3

