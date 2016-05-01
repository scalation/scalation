
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  An implementation supporting columnar relational databases facilitating easy
 *  and rapid analytics.  The columns in a relation are vectors from the
 *  `scalation.linalgebra` package.  Vectors and matrices may be readily extracted
 *  from a relation and feed into any of the numerous analytics techniques provided
 *  in `scalation.analytics`.  The implementation provides most of the columnar
 *  relational algebra operators given in the following paper:
 *  @see db.csail.mit.edu/projects/cstore/vldb.pdf
 *
 *  Some of the operators have Unicode versions: @see `scalation.util.UnicodeTest`
 */

package scalation.relalgebra

import scala.collection.immutable.StringOps

import scalation.linalgebra._
import scalation.linalgebra.MatrixKind._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Table` object provides functions for the `Table` companion objects.
 */
object TableObj
{
    /** Type definition for a row/tuple
     */
    type Row = Vector [Any]

    /** File-name extension for serialized relations
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

    /** Counter for making unique relation names
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
        if (dom == null) null else cPos.map (dom(_)).toString
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

} // TableObj object

import TableObj._


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Table` trait stores and operates on vectors.  The vectors form the
 *  columns of the columnar relational datastore.  Columns may have any of the
 *  following types:
 *  <p>
 *      C - `Complex`  - `VectorC` - 128 bit complex number a + bi
 *      D - `Double`   - `VectorD` -  64 bit double precision floating point number
 *      I - `Int`      - `VectorI` -  32 bit integer
 *      L - `Long`     - `VectorL` -  64 bit long integer
 *      Q - `Rational` - `VectorQ` - 128 bit ratio of two long integers
 *      R - `Real`     - `VectorR` - 128 bit quad precision floating point number
 *      S - `StrNum`   - `VectorS` - variable length numeric string
 *  <p>
 *------------------------------------------------------------------------------
 *  @param name     the name of the relation
 *  @param colName  the names of columns
 *  @param col      the Scala Vector of columns making up the columnar relation
 *  @param key      the column number for the primary key (< 0 => no primary key)
 *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = `StrNum`, `Double`) 
 */
trait Table
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of columns in the relation.
     */
    def cols: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of rows in the relation.
     */
    def rows: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def pi (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def π (cName: String*): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions.
     *  @param cPos   the column positions to project onto
     *  @param cName  the optional new names for the columns to project onto
     */
    def pi (cPos: Seq [Int], cName: Seq [String] = null): Table 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from column 'cName' in 'this' relation that satisfy the
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' relation that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def sigma [T <: Any] (cName: String, p: T => Boolean): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' relation that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def σ [T <: Any] (cName: String, p: T => Boolean): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' relation that satisfy
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
    /** Select the positions of elements from columns in 'cName' in 'this' relation
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' relation and 'r2' are incompatible by having
     *  differing numbers of columns or differing domain strings.
     *  @param r2  the other relation
     */
    def incompatible (r2: Table): Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  @param r2  the other relation
     */
    def union (r2: Table): Table 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  @param r2  the other relation
     */
    def ⋃ (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  @param r2  the other relation
     */
    def intersect (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  @param r2  the other relation
     */
    def ⋂ (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of 'this' relation and 'r2' ('this - r2').  Check that
     *  the two relations are compatible.
     *  @param r2  the other relation
     */
    def - (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing an "equi-join".  Rows from both
     *  relations are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def join (cName1: Seq [String], cName2: Seq [String], r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing an "equi-join".  Rows from both
     *  relations are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the string of join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the string of join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def join (cName1: String, cName2: String, r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing a "natural-join".  Rows from both
     *  relations are compared requiring 'cName' values to be equal.
     *  @param cName  the common join column names for both relation
     *  @param r2     the rhs relation in the join operation
     */
    def join (cName: Seq [String], r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing a "natural-join".  Rows from both
     *  relations are compared requiring agreement on common attributes (column names).
     *  @param r2  the rhs relation in the join operation
     */
    def >< (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing a "natural-join".  Rows from both
     *  relations are compared requiring agreement on common attributes (column names).
     *  @param r2  the rhs relation in the join operation
     */
    def ⋈ (r2: Table): Table

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine two sequences of column names and disambiguate any repeated names
     *  by appending "2".
     *  @param cn1  the first sequence of column names
     *  @param cn2  the second sequence of column names 
     */
    def disambiguate (cn1: Seq [String], cn2: Seq [String]): Seq [String] =
    {
        val n1 = cn1.length
        for (j <- 0 until n1 + cn2.length) yield
            if (j < n1) cn1(j)
            else { val nm2 = cn2(j - n1); if (cn1 contains nm2) nm2 + "2" else nm2 }
    } // disambiguate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine two sequences of column names, keeping all names from 'cn1' and
     *  only those in 'cn2' that are not repeats (i.e., not already in 'cn1').
     *  @param cn1  the first sequence of column names
     *  @param cn2  the second sequence of column names 
     */
    def uniq_union (cn1: Seq [String], cn2: Seq [String]): Seq [String] =
    {
        var cn3 = cn1
        for (j <- cn2.indices if ! (cn3 contains cn2(j))) cn3 = cn3 :+ cn2(j)
        cn3
    } // uniq_union

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' relation contains a row matching the given 'tuple'.
     *  @param tuple  an aggregation of columns values (potential row)
     */
    def contains (tuple: Row): Boolean

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
    def row (sos: Seq [String], _typ: String): Row =
    {
        val typ = if (_typ == null) "S" * sos.length else _typ    // missing => assume StrNum
        (for (j <- sos.indices) yield
            typ(j) match {
            case 'C' => Complex (sos(j))
            case 'D' => new StringOps (sos(j)).toDouble
            case 'I' => new StringOps (sos(j)).toInt
            case 'L' => new StringOps (sos(j)).toLong
            case 'Q' => Rational (sos(j))
            case 'R' => Real (sos(j))
            case _   => StrNum (sos(j))
            }).toVector
    } // row

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'tuple to 'this' relation as a new row.
     *  FIX:  want an efficient, covariant, mutable data structure, but `Array` is invariant.
     *  @param tuple  an aggregation of columns values (new row)
     *  @param typ    the string of corresponding types, e.g., 'SDI'
     */
    def add (tuple: Row)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show 'this' relation row by row.
     */
    def show ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of doubles, e.g., 
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'xy'
     *  <p>
     *  @param colPos  the column positions to use for the matrix
     *  @param kind    the kind of matrix to create
     */
//  def toMatriD (colPos: Seq [Int], kind: MatrixKind = DENSE): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of doubles and a vector of doubles.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
//  def toMatriDD (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): Tuple2 [MatriD, VectorD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of doubles and a vector of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
//  def toMatriDI (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): Tuple2 [MatriD, VectorI]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'xy'
     *  <p>
     *  @param colPos  the column positions to use for the matrix
     *  @param kind    the kind of matrix to create
     */
//  def toMatriI (colPos: Seq [Int], kind: MatrixKind = DENSE): MatriI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of integers and a vector of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
//  def toMatriII (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): Tuple2 [MatriI, VectorI]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save 'this' relation in a file using serialization.
     */
    def save ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' relation into a CSV file with each row written to a line.
     *  @param fileName  the file name of the data file
     */
    def writeCSV (fileName: String)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' relation into a JSON file.
     *  @param fileName  the file name of the data file
     */
    def writeJSON (fileName: String)
  
} // Table trait

