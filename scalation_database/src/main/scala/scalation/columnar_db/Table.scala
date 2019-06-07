
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Santosh Uttam Bobade
 *  @version 1.6
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  An implementation supporting columnar relational databases facilitating easy
 *  and rapid analytics.  The columns in a table/relation are vectors from the
 *  `scalation.linalgebra` package.  Vectors and matrices may be readily extracted
 *  from a relation and feed into any of the numerous analytics techniques provided
 *  in `scalation.analytics`.  The implementation provides most of the columnar
 *  relational algebra operators given in the following paper:
 *  @see db.csail.mit.edu/projects/cstore/vldb.pdf
 *
 *  Some of the operators have Unicode versions: @see `scalation.util.UnicodeTest`
 */

package scalation.columnar_db

import scala.collection.mutable.Map
import scala.collection.immutable.StringOps
import scala.reflect.ClassTag

import scalation.linalgebra.Vec
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO.StrNum
import scalation.math.TimeO.TimeNum
import scalation.math.{noComplex, noDouble, noInt, noLong, noRational, noReal, noStrNum, noTimeNum}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Table` object provides functions for the `Table` companion objects.
 */
object TableObj extends Error
{
    /** Type definition for a row/tuple
     */
    type Row = Vector [Any]

    /** File-name extension for serialized tables
     *  FIX: investigate using more efficient serialization, e.g.,
     *  @see github.com/EsotericSoftware/kryo
     */
    val SER = ".ser"

    /** File-name extension for CSV data files
     */
    val CSV = ".csv"

    /** File-name extension for JSON data files
     */
    val JSON = ".json"

    /** The token/element separation character (',' for CSV)
     */
    val SP = ','

    /** Counter for making unique table names
     */
    private var _ucount = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next unique count.
     */
    def ucount (): Int = { _ucount += 1; _ucount }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given row 'tuple', project onto the given column positions specified in 'cPos'.
     *  @param tuple  the row on which to apply the projection
     *  @param cPos   the column positions 
     */
    def project (tuple: Row, cPos: Seq [Int]): Row =
    {
        cPos.map (tuple(_)).toVector
    } // project

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a domain string 'dom', project onto the given column positions specified
     *  in 'cPos'.
     *  @param dom   the domain string on which to apply the projection
     *  @param cPos  the column positions 
     */
    def projectD (dom: String, cPos: Seq [Int]): String =
    {
       if (dom != null) {
            val sb = new StringBuilder
            for (i <- cPos) sb.append (dom(i))
            sb.toString
        } else null
    } // projectD
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 't' and 'u' are the same on column positions 'tp' and 'up'.
     *  @param t   the first tuple
     *  @param u   the second tuple
     *  @param tp  the column positions for tuple t
     *  @param up  the column positions for tuple u
     */
    def sameOn (t: Row, u: Row, tp: Seq [Int], up: Seq [Int]): Boolean =
    {
         project (t, tp) sameElements project (u, up)
    } // sameOn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a tuple with missing values for each column according to the given
     *  domains.  This function is used by 'leftJoin' and 'rightJoin'.
     *  @param domain  the domains of the table for which a null tuple is required
     */
    def nullTuple (domain: String): Row =
    {
        var v = Array.ofDim [Any] (domain.length)
        v.indices.map (i =>
            domain(i) match {
                case 'C' | 'c' | 'χ' => v(i) = noComplex
                case 'D' | 'd' | 'δ' => v(i) = noDouble
                case 'I' | 'i' | 'ι' => v(i) = noInt
                case 'L' | 'l' | 'λ' => v(i) = noLong
                case 'Q' | 'q' | 'ϟ' => v(i) = noRational
                case 'R' | 'r' | 'ρ' => v(i) = noReal
                case 'S' | 's' | 'σ' => v(i) = noStrNum
                case 'T' | 't' | 'τ' => v(i) = noTimeNum
                case _ => flaw ("nullTuple", s"not supported domain type ${domain(i)}")
            })
        v.toVector
    } // nullTuple

} // TableObj object

import TableObj._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Table` trait defines methods for operating on vectors.
 *  The vectors form the columns of the columnar relational datastore.
 *  Columns may have any of the following types:
 *  <p>
 *      C - `Complex`  - `VectorC` - 128 bit complex number a + bi
 *      D - `Double`   - `VectorD` -  64 bit double precision floating point number
 *      I - `Int`      - `VectorI` -  32 bit integer
 *      L - `Long`     - `VectorL` -  64 bit long integer
 *      Q - `Rational` - `VectorQ` - 128 bit ratio of two long integers
 *      R - `Real`     - `VectorR` - 128 bit quad precision floating point number
 *      S - `StrNum`   - `VectorS` - variable length numeric string
 *      T - `TimeNum`  - `VectorT` -  96 bit time Instant = (Long, Int)
 *  <p>
 */
trait Table extends Tabular
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of columns in the table.
     */
    def cols: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return all of the columns in the table.
     */
    def columns: Vector [Vec]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the column in the table with column name 'cName'.
     *  @param cName  column name used to retrieve the column vector
     */
    def column (cName: String): Vec

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the names of columns in the table.
     */
    def colNames: Seq [String]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mapping from column names to column positions.
     */
    def colsMap: Map [String, Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the domains for the columns in the table.
     */
    def domains: String

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the range of index values for the table.
     */
    def indices: Range = 0 until rows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of rows in the table.
     */
    def rows: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row by pulling values from all columns at position 'i'.
     *  @param i  the 'i'th position
     */
    def row (i: Int): Row

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row by pulling values from an array of strings and converting
     *  elements to their appropriate types.
     *  @param sos   the sequence of strings holding the values
     *  @param _typ  the string of corresponding types, e.g., 'SDI'
     */
    @throws (classOf [Exception])
    def row (sos: Seq [String], _typ: String): Row =
    {
        var result: Vector [Any] = null
        val typ = if (_typ == null) "S" * sos.length else _typ    // missing => assume StrNum
        try {
            result = (for (j <- sos.indices) yield
                typ(j) match {
                case 'C' => if (sos(j).isEmpty) Complex (0)  else Complex (sos(j))
                case 'D' => if (sos(j).isEmpty) 0.0          else new StringOps (sos(j)).toDouble
                case 'I' => if (sos(j).isEmpty) 0            else new StringOps (sos(j)).toInt
                case 'L' => if (sos(j).isEmpty) 0l           else new StringOps (sos(j)).toLong
                case 'Q' => if (sos(j).isEmpty) Rational (0) else Rational (sos(j))
                case 'R' => if (sos(j).isEmpty) Real (0)     else Real (sos(j))
                case 'T' => if (sos(j).isEmpty) TimeNum (0)  else TimeNum (sos(j))
                case _   => StrNum (sos(j))
            }).toVector
        } catch {
            case ex: Exception =>
                println ("row function throw exception, row is:\n" + sos + "\ntuple length is: " + sos.size)
                throw ex
        } // try
        result
    } // row

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' table contains a row matching the given 'tuple'.
     *  @param tuple  an aggregation of columns values (potential row)
     */
    def contains (tuple: Row): Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rename 'this' table, returning a shallow copy of the table.
     *  @param newName  the new name for the table.
     */
    def rename (newName: String): Table

    // ================================================================= PROJECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def project (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def π (cName: String*): Table = project (cName :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def pi (cName: String*): Table = project (cName :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions.
     *  @param cPos   the column positions to project onto
     *  @param cName  the optional new names for the columns to project onto
     */
    def project (cPos: Seq [Int], cName: Seq [String] = null): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions.
     *  @param cPos   the column positions to project onto
     *  @param cName  the optional new names for the columns to project onto
     */
    def π (cPos: Seq [Int], cName: Seq [String] = null): Table = project (cPos, cName)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions.
     *  @param cPos   the column positions to project onto
     *  @param cName  the optional new names for the columns to project onto
     */
    def pi (cPos: Seq [Int], cName: Seq [String] = null): Table = project (cPos, cName)

    // ======================================================== EXTENDED PROJECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given columns (an extended projection operator that
     *  applies aggregate operators to aggregation columns and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggCol  the columns to aggregate on: (aggregate function, new column name, old column name)*
     *  @param cName   the other columns to project on
     */
    def eproject (aggCol: AggColumn*)(cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given columns (an extended projection operator that
     *  applies aggregate operators to aggregation columns and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggCol  the columns to aggregate on: (aggregate function, new column name, old column name)*
     *  @param cName   the other columns to project on
     */
    def Π (aggCol: AggColumn*)(cName: String*): Table = eproject (aggCol :_*)(cName :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given columns (an extended projection operator that
     *  applies aggregate operators to aggregation columns and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggCol  the columns to aggregate on: (aggregate function, new column name, old column name)*
     *  @param cName   the other columns to project on
     */
    def epi (aggCol: AggColumn*)(cName: String*): Table = eproject (aggCol :_*)(cName :_*)

    // ========================================================== PROJECT-SELECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from column 'cName' in 'this' table that satisfy the
     *  predicate 'p' and project onto that column.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def pisigmaC (cName: String, p: Complex => Boolean): Table

    def pisigmaD (cName: String, p: Double => Boolean): Table

    def pisigmaI (cName: String, p: Int => Boolean): Table

    def pisigmaL (cName: String, p: Long => Boolean): Table

    def pisigmaQ (cName: String, p: Rational => Boolean): Table

    def pisigmaR (cName: String, p: Real => Boolean): Table

    def pisigmaS (cName: String, p: StrNum => Boolean): Table

    // ================================================================== SELECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' table that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def select [T : ClassTag] (cName: String, p: T => Boolean): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' table that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def σ [T : ClassTag] (cName: String, p: T => Boolean): Table = select (cName, p)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' table that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def sigma [T : ClassTag] (cName: String, p: T => Boolean): Table = select (cName, p)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' table that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def sigmaC (cName: String, p: Complex => Boolean): Table

    def sigmaD (cName: String, p: Double => Boolean): Table

    def sigmaI (cName: String, p: Int => Boolean): Table

    def sigmaL (cName: String, p: Long => Boolean): Table

    def sigmaQ (cName: String, p: Rational => Boolean): Table

    def sigmaR (cName: String, p: Real => Boolean): Table

    def sigmaS (cName: String, p: StrNum => Boolean): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select the positions of elements from columns in 'cName' in 'this' table
     *  that satisfy the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def selectC (cName: String, p: Complex => Boolean): Seq [Int]

    def selectD (cName: String, p: Double => Boolean): Seq [Int]

    def selectI (cName: String, p: Int => Boolean): Seq [Int]

    def selectL (cName: String, p: Long => Boolean): Seq [Int]

    def selectQ (cName: String, p: Rational => Boolean): Seq [Int]

    def selectR (cName: String, p: Real => Boolean): Seq [Int]

    def selectS (cName: String, p: StrNum => Boolean): Seq [Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select across all columns at the specified column positions.
     *  @param pos  the specified column positions
     */
    def selectAt (pos: Seq [Int]): Table

    // =========================================================== SET OPERATORS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union 'this' table and 'r2'.  Check that the two tables are compatible.
     *  @param r2  the other table
     */
    def union (r2: Table): Table 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union 'this' table and 'r2'.  Check that the two tables are compatible.
     *  @param r2  the other table
     */
    def ⋃ (r2: Table): Table = union (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' table and 'r2'.  Check that the two tables are compatible.
     *  @param r2  the other table
     */
    def intersect (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' table and 'r2'.  Check that the two tables are compatible.
     *  @param r2  the other table
     */
    def ⋂ (r2: Table): Table = intersect (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of 'this' table and 'r2' ('this - r2').  Check that
     *  the two tables are compatible.
     *  @param r2  the other table
     */
    def minus (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of 'this' table and 'r2' ('this - r2').  Check that
     *  the two tables are compatible.
     *  @param r2  the other table
     */
    def - (r2: Table): Table = minus (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' table and 'r2' are incompatible by having
     *  differing numbers of columns or differing domain strings.
     *  @param r2  the other table
     */
    def incompatible (r2: Table): Boolean

    // ================================================================= PRODUCT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cartesian product of 'this' table and 'r2' ('this × r2').
     *  @param r2  the second table
     */
    def product (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cartesian product of 'this' table and 'r2' ('this × r2').
     *  @param r2  the second table
     */
    def × (r2: Table): Table = product (r2)

    // ==================================================================== JOIN

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing a "natural-join".  Rows from both
     *  tables are compared requiring agreement on common attributes (column names).
     *  @param r2  the rhs table in the join operation
     */
    def join (r2: Table): Table = join (colNames intersect r2.colNames, r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing a "natural-join".  Rows from both
     *  tables are compared requiring agreement on common attributes (column names).
     *  @param r2  the rhs table in the join operation
     */
    def ⋈ (r2: Table): Table = join (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing a "natural-join".  Rows from both
     *  tables are compared requiring 'cName' values to be equal.
     *  @param cName  the common join column name for both table
     *  @param r2     the rhs table in the join operation
     */
    def join (cName: String, r2: Table): Table = join (Seq (cName), r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing a "natural-join".  Rows from both
     *  tables are compared requiring 'cName' values to be equal.
     *  @param cName  the common join column name for both table
     *  @param r2     the rhs table in the join operation
     */
    def ⋈ (cName: String, r2: Table): Table = join (Seq (cName), r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing a "natural-join".  Rows from both
     *  tables are compared requiring 'cName' values to be equal.
     *  @param cNames  the common join column names for both table
     *  @param r2      the rhs table in the join operation
     */
    def join (cNames: Seq [String], r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing a "natural-join".  Rows from both
     *  tables are compared requiring 'cName' values to be equal.
     *  @param cNames  the common join column names for both table
     *  @param r2      the rhs table in the join operation
     */
    def ⋈ (cNames: Seq [String], r2: Table): Table = join (cNames, r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "equi-join".  Rows from both
     *  tables are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column name of this table (e.g., the Foreign Key)
     *  @param cName2  the join column name of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def join (cName1: String, cName2: String, r2: Table): Table = join (Seq (cName1), Seq (cName2), r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "equi-join".  Rows from both
     *  tables are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column name of this table (e.g., the Foreign Key)
     *  @param cName2  the join column name of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def ⋈ (cName1: String, cName2: String, r2: Table): Table = join (Seq (cName1), Seq (cName2), r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "equi-join".  Rows from both
     *  tables are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cNames1  the join column names of this table (e.g., the Foreign Key)
     *  @param cNames2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2       the rhs table in the join operation
     */
    def join (cNames1: Seq [String], cNames2: Seq [String], r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "equi-join".  Rows from both
     *  tables are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cNames1  the join column names of this table (e.g., the Foreign Key)
     *  @param cNames2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2       the rhs table in the join operation
     */
    def ⋈ (cNames1: Seq [String], cNames2: Seq [String], r2: Table): Table = join (cNames1, cNames2, r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The theta join, handle the predicates in where are connect by "and" (where a....and b....).
     *  @param r2  the second table
     *  @param p0  the first theta join predicate (r1 cName, r2 cName, predicate to compare these two column)
     *  @param p   the rest of theta join predicates (r1 cName, r2 cName, predicates to compare these two column)
     */
    def join [T] (r2: Table, p0: Predicate2 [T], p: Predicate2 [T]*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The theta join, handle the predicates in where are connect by "and" (where a....and b....).
     *  @param r2  the second table
     *  @param p0  the first theta join predicate (r1 cName, r2 cName, predicate to compare these two column)
     *  @param p   the rest of theta join predicates (r1 cName, r2 cName, predicates to compare these two column)
     */
    def ⋈ [T] (r2: Table, p0: Predicate2 [T], p: Predicate2 [T]*): Table = join (r2, p0, p :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "left-join".  Rows from both
     *  tables are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def leftJoin (cName1: String, cName2: String, r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "left-join".  Rows from both
     *  tables are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  Note: although this is the semi-join symbol, due to Unicode limitations, it is
     *  used for left-join.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def ⋉  (cName1: String, cName2: String, r2: Table): Table = leftJoin (cName1, cName2, r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "apprimate left-join".  Rows from both
     *  tables are compared requiring 'cName1' values to apprximately equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param thres   the approximate equality threshold
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def leftJoin (thres: Double = 0.01) (cName1: String, cName2: String, r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "apprimate left-join".  Rows from both
     *  tables are compared requiring 'cName1' values to apprximately equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def ⋉  (thres: Double = 0.01) (cName1: String, cName2: String, r2: Table): Table =
    {
        leftJoin (thres)(cName1, cName2, r2)
    } // ⋉ 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "right-join".  Rows from both
     *  tables are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the right table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def rightJoin (cName1: String, cName2: String, r2: Table): Table = r2.leftJoin (cName2, cName1, this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' table and 'r2' by performing an "right-join".  Rows from both
     *  tables are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the right table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this table (e.g., the Foreign Key)
     *  @param cName2  the join column names of table r2 (e.g., the Primary Key)
     *  @param r2      the rhs table in the join operation
     */
    def ⋊  (cName1: String, cName2: String, r2: Table): Table = r2.leftJoin (cName2, cName1, this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine two sequences of column names, keeping all names from 'cn1' and
     *  only those in 'cn2' that are not repeats (i.e., not already in 'cn1').
     *  @param cn1  the first sequence of column names
     *  @param cn2  the second sequence of column names
     */
    protected def uniq_union (cn1: Seq [String], cn2: Seq [String]): Seq [String] =
    {
        var cn3 = cn1
        for (j <- cn2.indices if ! (cn3 contains cn2(j))) cn3 = cn3 :+ cn2(j)
        cn3
    } // uniq_union

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine two sequences of column names and disambiguate any repeated names
     *  by appending "2".
     *  @param cn1  the first sequence of column names
     *  @param cn2  the second sequence of column names 
     */
    protected def disambiguate (cn1: Seq [String], cn2: Seq [String]): Seq [String] =
    {
        val n1 = cn1.length
        for (j <- 0 until n1 + cn2.length) yield
            if (j < n1) cn1(j)
            else { val nm2 = cn2(j - n1); if (cn1 contains nm2) nm2 + "2" else nm2 }
    } // disambiguate

    // ================================================================ GROUP BY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group 'this' table by specified column names, returning 'this' table.
     *  @param cName group columns
     */
    def groupBy (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group 'this' table by specified column names, returning 'this' table.
     *  @param cName group columns
     */
    def γ (cName: String*): Table = groupBy (cName :_*)

    // ================================================================= ORDER BY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (ascending) the rows in the table by the selected columns 'cName'.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def orderBy (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (ascending) the rows in the table by the selected columns 'cName'.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def ω (cName: String*): Table = orderBy (cName :_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (descending) the rows in the table by the selected columns 'cName'.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def reverseOrderBy (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (descending) the rows in the table by the selected columns 'cName'.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param cName  the column names that are to be sorted
     */
    def ωω (cName: String*): Table = reverseOrderBy (cName :_*)

    // ================================================================ COMPRESS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compress the selected columns 'cName' in 'this' table.
     *  @param cName  the names of the columns to be compressed
     */
    def compress (cName: String*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compress the selected columns 'cName' in 'this' table.
     *  @param cName  the names of the columns to be compressed
     */
    def ζ (cName: String*) { compress (cName :_*) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Uncompress the selected columns 'cName' in 'this' table.
     *  @param cName  the names of the columns to be uncompressed
     */
    def uncompress (cName: String*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Uncompress the selected columns 'cName' in 'this' table.
     *  @param cName  the names of the columns to be uncompressed
     */
    def ζζ (cName: String*) { uncompress (cName :_*) }

    // ================================================================= UPDATES

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'tuple to 'this' table as a new row.
     *  @param tuple  an aggregation of columns values (new row)
     *  @param typ    the string of corresponding types, e.g., 'SDI'
     */
    def add (tuple: Row)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named 'cName' using function 'func' for elements with
     *  value 'matchStr'.
     *  @param cName     the name of the column to be updated
     *  @param newVal    the value used to assign updated values
     *  @param matchVal  the value to be matched to elements
     *  @tparam T        type of the column
     */
    def update [T ] (cName: String, newVal: T, matchVal: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named 'cName' using function 'func' for elements with
     *  value 'matchStr'.
     *  @param cName     the name of the column to be updated
     *  @param func      the function used to assign updated values
     *  @param matchVal  the value to be matched to elements
     *  @tparam T        type of the column
     */
    def update [T ] (cName: String, func: (T) => T, matchVal: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named 'cName' using function 'func' for elements where
     *  the predicate 'pred' evaluates to true.
     *  @param cName  the name of the column to be updated
     *  @param func   the function used to assign updated values         // FIX - generalize type
     *  @param pred   the predicated used to select elements for update
     *  @tparam T     type of the column
     */
    def update [T ] (cName: String, func: (T) => T, pred: (T) => Boolean)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Delete the rows from 'this' table that satisfy the predicates.
     *  @param  p  tuple(1): column name, tuple(2): predicate (T => `Boolean`)
     *  @tparam T  the predicate type
     */
    def delete [T] (p: Predicate [T]*): Table

    // =============================================================== TO MATRIX

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' table into a matrix of doubles, e.g., 
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'xy'
     *  <p>
     *  @param colPos  the column positions to use for the matrix
     *  @param kind    the kind of matrix to create
     */
//  def toMatriD (colPos: Seq [Int], kind: MatrixKind = DENSE): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' table into a matrix of doubles and a vector of doubles.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
//  def toMatriDD (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): (MatriD, VectorD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' table into a matrix of doubles and a vector of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
//  def toMatriDI (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): (MatriD, VectorI)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' table into a matrix of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'xy'
     *  <p>
     *  @param colPos  the column positions to use for the matrix
     *  @param kind    the kind of matrix to create
     */
//  def toMatriI (colPos: Seq [Int], kind: MatrixKind = DENSE): MatriI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' table into a matrix of integers and a vector of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
//  def toMatriII (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): (MatriI, VectorI)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save 'this' table in a file using serialization.
     */
    def save ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' table into a CSV file with each row written to a line.
     *  @param fileName  the file name of the data file
     */
    def writeCSV (fileName: String)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' table into a JSON file.
     *  @param fileName  the file name of the data file
     */
    def writeJSON (fileName: String)
  
} // Table trait

