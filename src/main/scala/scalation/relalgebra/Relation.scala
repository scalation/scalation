
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  An implementation supporting columnar relational databases faciltating easy
 *  and rapid analytics.  The columns in a relation are vectors from the
 *  `scalation.linalgebra` package.  Vectors and matrices may be readily extracted
 *  from a relation and feed into any of the numerous analytics techniques provided
 *  in `scalation.analytics`.  The implemenation provides most of the columnar
 *  relational algebra operators given in the following paper:
 *  @see db.csail.mit.edu/projects/cstore/vldb.pdf
 */

package scalation.relalgebra

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, PrintWriter}

import collection.immutable.StringOps
import collection.mutable.Map
import io.Source.fromFile

import scalation.linalgebra._
import scalation.linalgebra.MatrixKind._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO._
import scalation.util.SEP

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Relation` companion object provides additional functions for the `Relation`
 *  class.
 */
object Relation
{
    /** Type definition for a row/tuple
     */
    type Row = Vector [Any]

    /** Relative path for database storage directory
     */
    private val DB_DIR = "store" + SEP

    /** Relative path for input/output data directory
     */
    private val DATA_DIR = "data" + SEP

    /** Filename extension for serialized relations
     *  FIX: investigate using more efficient serialization, e.g.,
     *  @see github.com/EsotericSoftware/kryo
     */
    private val SER = ".ser"

    /** Filename extension for CSV data files
     */
    private val CSV = ".csv"

    /** Filename extension for JSON data files
     */
    private val JSON = ".json"

    /** The token/element separation character (',' for CSV)
     */
    private val SP = ','

    /** Counter for making unique relation names
     */
    private var _count = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next count.
     */
    def count (): Int = { _count += 1; _count }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory using serialization.
     *  @param name  the name of the relation to load
     */
    def apply (name: String): Relation =
    {
        val ois = new ObjectInputStream (new FileInputStream (DB_DIR + name + SER))
        val obj = ois.readObject ()
        ois.close ()
        obj.asInstanceOf [Relation]
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory loading its columns
     *  with data from the CSV file named 'fileName'.
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param colName   the names of columns
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., SD = StrNum, Double) 
     */
    def apply (fileName: String, name: String, colName: Seq [String], key: Int,
               domain: String): Relation =
    {
        val sp     = ','                                    // element separator
        val lines  = fromFile (DATA_DIR + fileName).getLines
        val newCol = Vector [Vec] ()
        val r3 = Relation (name, colName, newCol, key, domain)
        for (ln <- lines) r3.add (r3.row (ln.split (sp), domain))
        r3
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory from a JSON file.
     *  @param fileName  the file name of the JSON file
     *  @param name      the name of the relation to load
     */
    def apply (fileName: String, name: String): Relation =
    {
        null                                                     // FIX - needs to be implemented
    } // apply

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the 'xy' matrix of doubles.
     *  @param xy       the matrix containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., SD = StrNum, Double) 
     */
    def fromMatriD (xy: MatriD, name: String, colName: Seq [String], key: Int = -1,
                    domain: String = null): Relation =
    {
        val newCol = for (j <- 0 until xy.dim2) yield xy.col (j).asInstanceOf [Vec]
        Relation (name, colName, newCol.toVector, key, domain)
    } // fromMatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the 'x' matrix of doubles and 'y' vector of doubles
     *  or integers.
     *  @param x        the matrix containing the data
     *  @param y        the vector containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., SD = StrNum, Double) 
     */
    def fromMatriD_ (x: MatriD, y: Vec, name: String, colName: Seq [String], key: Int = -1,
                    domain: String = null): Relation =
    {
        val newCol = for (j <- 0 until x.dim2) yield x.col (j).asInstanceOf [Vec]
        Relation (name, colName, newCol.toVector :+ y, key, domain)
    } // fromMatriD_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the 'xy' matrix of integers.
     *  @param xy       the matrix containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., SD = StrNum, Double) 
     */
    def fromMatriI (xy: MatriI, name: String, colName: Seq [String], key: Int = -1,
                    domain: String = null): Relation =
    {
        val newCol = for (j <- 0 until xy.dim2) yield xy.col (j).asInstanceOf [Vec]
        Relation (name, colName, newCol.toVector, key, domain)
    } // fromMatriI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the 'xy' matrix of integers and 'y' vector of integers.
     *  @param x        the matrix containing the data
     *  @param y        the vector containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., SD = StrNum, Double) 
     */
    def fromMatriII (x: MatriI, y: VectorI, name: String, colName: Seq [String], key: Int = -1,
                     domain: String = null): Relation =
    {
        val newCol = for (j <- 0 until x.dim2) yield x.col (j).asInstanceOf [Vec]
        Relation (name, colName, newCol.toVector :+ y, key, domain)
    } // fromMatriII

} // Relation object

import Relation._


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Relation` class stores and operates on vectors.  The vectors form the
 *  columns of the columnar relational datastore.  Columns may have any of the
 *  following types:
 *  <p>
 *      C - Complex  - VectorC - 128 bit complex number a + bi
 *      D - Double   - VectorD -  64 bit double precision floating point number
 *      I - Int      - VectorI -  32 bit integer
 *      L - Long     - VectorL -  64 bit long integer
 *      Q - Rational - VectorQ - 128 bit ratio of two long integers
 *      R - Real     - VectorR - 128 bit quad precision floating point number
 *      S - StrNum   - VectorS - variable length numeric string
 *  <p>
 *------------------------------------------------------------------------------
 *  @param name     the name of the relation
 *  @param colName  the names of columns
 *  @param col      the Scala Vector of columns making up the columnar relation
 *  @param key      the column number for the primary key (< 0 => no primary key)
 *  @param domain   an optional string indicating domains for columns (e.g., SD = StrNum, Double) 
 */
case class Relation (name: String, colName: Seq [String], var col: Vector [Vec],
                     key: Int = 0, domain: String = null)
{
    if (colName.length != col.length) println ("Relation.constructor: incompatible sizes")

    /** The colMap maps column names to column positions
     */
    private val colMap = Map [String, Int] ()
    for (j <- colName.indices) colMap += colName(j) -> j

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of columns in the relation.
     */
    def cols: Int = col.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of rows in the relation.
     */
    def rows: Int = col(0).size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def pi (cName: String*): Relation = pi (cName.map(colMap (_)), cName)

    def π (cName: String*): Relation = pi (cName: _*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions.
     *  @param cPos   the column positions to project onto
     *  @param cName  the optional new names for the columns to project onto
     */
    def pi (cPos: Seq [Int], cName: Seq [String] = null): Relation =
    {
        val newCName  = if (cName == null) cPos.map (colName(_)) else cName
        val newCol    = cPos.map (col(_)).toVector
        val newKey    = if (cPos contains key) key else -1
        val newDomain = projectD (domain, cPos)
        Relation (name + "_p_" + count (), newCName, newCol, newKey, newDomain)
    } // pi

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from column 'cName' in 'this' relation that satisfy the
     *  predicate 'p' and project onto that column.
     *  Version for `StrNum`.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (Boolean function) to be satisfied
     */
    def pisigmaS (cName: String, p: StrNum => Boolean): Relation =
    {
        val colNum    = colMap (cName)
        val newCName  = Seq (cName)
        val newCol    = Vector (col (colNum).asInstanceOf [VectorS].filter (p))
        val newKey    = if (colNum == key) key else -1
        val newDomain = projectD (domain, Array (colNum))
        Relation (name + "_s_" + count (), newCName, newCol, newKey, newDomain)
    } // pisigmaS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from column 'cName' in 'this' relation that satisfy the
     *  predicate 'p' and project onto that column.
     *  Version for `Double`.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (Boolean function) to be satisfied
     */
    def pisigmaD (cName: String, p: Double => Boolean): Relation =
    {
        val colNum    = colMap (cName)
        val newCName  = Seq (cName)
        val newCol    = Vector (col (colNum).asInstanceOf [VectorD].filter (p))
        val newKey    = if (colNum == key) key else -1
        val newDomain = projectD (domain, Array (colNum))
        Relation (name + "_s_" + count (), newCName, newCol, newKey, newDomain)
    } // pisigmaD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' relation that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (Boolean function) to be satisfied
     */
    def sigma [T <: Any] (cName: String, p: T => Boolean): Relation =
    {
        if (domain != null) {
            domain(colMap (cName)) match {
            case 'D' => selectAt (selectD (cName, p.asInstanceOf [Double => Boolean]))
            case 'I' => selectAt (selectI (cName, p.asInstanceOf [Int => Boolean]))
            case 'L' => selectAt (selectL (cName, p.asInstanceOf [Long => Boolean]))
            case 'S' => selectAt (selectS (cName, p.asInstanceOf [StrNum => Boolean]))
            case _  => { println ("sigma: predicate type not supported"); null }
            } // match
        } else {
            println ("sigma: optional domains not given - use type specific sigma?")
            null
        } // if
    } // sigma

    def σ [T <: Any] (cName: String, p: T => Boolean): Relation = sigma (cName, p)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' relation that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (Boolean function) to be satisfied
     */
    def sigmaC (cName: String, p: Complex => Boolean): Relation = selectAt (selectC (cName, p))

    def sigmaD (cName: String, p: Double => Boolean): Relation = selectAt (selectD (cName, p))

    def sigmaI (cName: String, p: Int => Boolean): Relation = selectAt (selectI (cName, p))

    def sigmaL (cName: String, p: Long => Boolean): Relation = selectAt (selectL (cName, p))

    def sigmaQ (cName: String, p: Rational => Boolean): Relation = selectAt (selectQ (cName, p))

    def sigmaR (cName: String, p: Real => Boolean): Relation = selectAt (selectR (cName, p))

    def sigmaS (cName: String, p: StrNum => Boolean): Relation = selectAt (selectS (cName, p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select the positions of elements from columns in 'cName' in 'this' relation
     *  that satisfy the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (Boolean function) to be satisfied
     */
    def selectC (cName: String, p: Complex => Boolean): Seq [Int] =
    {
        col (colMap (cName)).asInstanceOf [VectorC].filterPos (p)
    } // selectC

    def selectD (cName: String, p: Double => Boolean): Seq [Int] =
    {
        col (colMap (cName)).asInstanceOf [VectorD].filterPos (p)
    } // selectD

    def selectI (cName: String, p: Int => Boolean): Seq [Int] =
    {
        col (colMap (cName)).asInstanceOf [VectorI].filterPos (p)
    } // selectI

    def selectL (cName: String, p: Long => Boolean): Seq [Int] =
    {
        col (colMap (cName)).asInstanceOf [VectorL].filterPos (p)
    } // selectL

    def selectQ (cName: String, p: Rational => Boolean): Seq [Int] =
    {
        col (colMap (cName)).asInstanceOf [VectorQ].filterPos (p)
    } // selectQ

    def selectR (cName: String, p: Real => Boolean): Seq [Int] =
    {
        col (colMap (cName)).asInstanceOf [VectorR].filterPos (p)
    } // selectR

    def selectS (cName: String, p: StrNum => Boolean): Seq [Int] =
    {
        col (colMap (cName)).asInstanceOf [VectorS].filterPos (p)
    } // selectS


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select across all columns at the specified column positions.
     *  @param pos  the specified column positions
     */
    def selectAt (pos: Seq [Int]): Relation =
    {
       val newCol = (for (j <- col.indices) yield Vec.select (col(j), pos)).toVector
       Relation (name + "_s_" + count (), colName, newCol, key, domain)
    } // selectAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' relation and 'r2' are incompatible by having
     *  differing numbers of columns or differing domain strings.
     *  @param r2  the other relation
     */
    def incompatible (r2: Relation): Boolean =
    {
         if (cols != r2.cols) {
             println ("compatible: differing number of columns")
             true
         } else if (domain != r2.domain) {
             println ("compatible: differing domain strings")
             true
         } else {
             false
         } // if
    } // incompatible

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  @param r2  the other relation
     */
    def union (r2: Relation): Relation =
    {
       if (incompatible (r2)) return null
       val newCol = (for (j <- col.indices) yield Vec.++ (col(j), r2.col(j)))
       Relation (name + "_u_" + count (), colName, newCol.toVector, -1, domain)
    } // union

    def ⋃ (r2: Relation): Relation = union (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of 'this' relation and 'r2' ('this - r2').  Check that
     *  the two relations are compatible.
     *  @param r2  the other relation
     */
    def - (r2: Relation): Relation =
    {
       if (incompatible (r2)) return null
       val newCol = Vector.fill [Vec] (colName.length)(null)
       val r3 = Relation (name + "_m_" + count (), colName, newCol, key, domain)
       for (i <- 0 until rows if ! (r2 contains row(i))) r3.add (row(i))
       r3
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing an "equi-join".  Rows from both
     *  relations are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def join (cName1: Seq [String], cName2: Seq [String], r2: Relation): Relation =
    {
        val ncols = cols + r2.cols
        val cp1   = cName1.map (colMap (_))                        // get column positions in 'this'
        val cp2   = cName2.map (r2.colMap (_))                     // get column positions in 'r2'
        if (cp1.length != cp2.length) println ("join: incompatible sizes on match columns")

        val newCName  = disambiguate (colName, r2.colName)
        val newCol    = Vector.fill [Vec] (ncols) (null)
        val newKey    = key                                        // FIX
        val newDomain = domain + r2.domain
        val r3 = Relation (name + "_j_" + count (), newCName, newCol, newKey, newDomain)

        for (i <- 0 until rows) {
            val t = row(i)
            for (j <- 0 until r2.rows) {
                val u = r2.row(j)
                if (sameOn (t, u, cp1, cp2)) r3.add (t ++ u)
            } // for
        } // for
        r3
    } // join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing an "equi-join".  Rows from both
     *  relations are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  @param cName1  the string of join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the string of join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def join (cName1: String, cName2: String, r2: Relation): Relation =
    {
        join (cName1.split (" "), cName2.split (" "), r2)
    } // join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing a "natural-join".  Rows from both
     *  relations are compared requiring 'cName' values to be equal.
     *  @param cName  the common join column names for both relation
     *  @param r2     the rhs relation in the join operation
     */
    def join (cName: Seq [String], r2: Relation): Relation =
    {
        val ncols = cols + r2.cols - cName.length
        val cp1   = cName.map (colMap (_))                         // get column positions in 'this'
        val cp2   = cName.map (r2.colMap (_))                      // get column positions in 'r2'
        val cp3   = r2.colName.map (r2.colMap (_)) diff cp2        // 'r2' specific columns
        if (cp1.length != cp2.length) println ("join: incompatible sizes on match columns")

        val newCName  = disambiguate (colName, r2.colName)
        val newCol    = Vector.fill [Vec] (ncols) (null)
        val newKey    = key                                        // FIX
        val newDomain = domain + r2.domain
        val r3 = Relation (name + "_j_" + count (), newCName, newCol, newKey, newDomain)

        for (i <- 0 until rows) {
            val t = row(i)
            for (j <- 0 until r2.rows) {
                val u = r2.row(j)
                if (sameOn (t, u, cp1, cp2)) { val u3 = project (u, cp3); r3.add (t ++ u3) }
            } // for
        } // for
        r3
    } // join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2 by performing a "natural-join".  Rows from both
     *  relations are compared requiring agreement on common attibutes (column names).
     *  @param r2  the rhs relation in the join operation
     */
    def >< (r2: Relation): Relation =
    {
        val common = colName intersect r2.colName
        join (common, r2)
    } // ><

    def ⋈ (r2: Relation): Relation = >< (r2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine two sequences of column names, disambiguating any repeated names by
     *  appending "2".
     *  @param cn1  the first sequence of column names
     *  @param cn2  the second sequence of column names 
     */
    def disambiguate (cn1: Seq [String], cn2: Seq [String]): Seq [String] =
    {
        val n1 = cn1.length
        for (j <- 0 until n1 + cn2.length) yield
            if (j < n1) cn1(j)
            else { val nm2 = cn2(j - n1); if (colName contains nm2) nm2 + "2" else nm2 }
    } // disambiguate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' relation contains a row matching the given 'tuple'.
     *  @param tuple  an aggregation of columns values (potential row)
     */
    def contains (tuple: Row): Boolean =
    {
        for (i <- 0 until rows if row(i) sameElements tuple) return true 
        false
    } // contains

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row by pulling values from all columns at position 'i'.
     *  @param i  the ith position
     */
    def row (i: Int): Row = col.map (Vec (_, i)).toVector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row by pulling values from an array of strings and converting
     *  elements to their appropriate types.
     *  @param sos  the sequence of strings holding the values
     *  @param typ  the string of corresponding types, e.g., "SDI"
     */
    def row (sos: Seq [String], typ: String): Row =
    {
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
     *  FIX:  want an efficient, covariant, mutable data structue, but Array is invariant.
     *  @param tuple  an aggregation of columns values (new row)
     *  @param typ    the string of corresponding types, e.g., "SDI"
     */
    def add (tuple: Row)
    {
        col = (for (j <- tuple.indices) yield Vec.:+ (col(j), tuple(j))).toVector
    } // add 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a string.
     */
    override def toString: String =
    {
        var sb = new StringBuilder ("Relation(" + name + ", " + key + ",\n" + colName + ",\n")
        for (i <- col.indices) sb.append (col(i) + "\n")
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of doubles, e.g., 
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'xy'
     *  <p>
     *  @param colPos  the column positions to use for the matrix
     *  @param kind    the kind of matrix to create
     */
    def toMatriD (colPos: Seq [Int], kind: MatrixKind = DENSE): MatriD =
    {
        kind match {
        case DENSE           => MatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]])
        case SPARSE          => SparseMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]])
        case SYM_TRIDIAGONAL => SymTriMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]])
        case BIDIAGONAL      => BidMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]])
        } // match
    } // toMatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of doubles and a vector of doubles.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
    def toMatriDD (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): Tuple2 [MatriD, VectorD] =
    {
        kind match {
        case DENSE           => (MatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]]),
                                 col(colPosV).asInstanceOf [VectorD])
        case SPARSE          => (SparseMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]]),
                                 col(colPosV).asInstanceOf [VectorD])
        case SYM_TRIDIAGONAL => (SymTriMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]]),
                                 col(colPosV).asInstanceOf [VectorD])
        case BIDIAGONAL      => (BidMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]]),
                                 col(colPosV).asInstanceOf [VectorD])
        } // match
    } // toMatriDD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of doubles and a vector of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
    def toMatriDI (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): Tuple2 [MatriD, VectorI] =
    {
        kind match {
        case DENSE           => (MatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]]),
                                 col(colPosV).asInstanceOf [VectorI])
        case SPARSE          => (SparseMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]]),
                                 col(colPosV).asInstanceOf [VectorI])
        case SYM_TRIDIAGONAL => (SymTriMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]]),
                                 col(colPosV).asInstanceOf [VectorI])
        case BIDIAGONAL      => (BidMatrixD (pi (colPos).col.asInstanceOf [Vector [VectorD]]),
                                 col(colPosV).asInstanceOf [VectorI])
        } // match
    } // toMatriDI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'xy'
     *  <p>
     *  @param colPos  the column positions to use for the matrix
     *  @param kind    the kind of matrix to create
     */
    def toMatriI (colPos: Seq [Int], kind: MatrixKind = DENSE): MatriI =
    {
        kind match {
        case DENSE           => MatrixI (pi (colPos).col.asInstanceOf [Vector [VectorI]])
        case SPARSE          => SparseMatrixI (pi (colPos).col.asInstanceOf [Vector [VectorI]])
        case SYM_TRIDIAGONAL => SymTriMatrixI (pi (colPos).col.asInstanceOf [Vector [VectorI]])
        case BIDIAGONAL      => BidMatrixI (pi (colPos).col.asInstanceOf [Vector [VectorI]])
        } // match
    } // toMatriI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of integers and a vector of integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'x' and vector 'y'
     *  <p>
     *  @param colPos   the column positions to use for the matrix
     *  @param colPosV  the column position to use for the vector
     *  @param kind     the kind of matrix to create
     */
    def toMatriII (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): Tuple2 [MatriI, VectorI] =
    {
        kind match {
        case DENSE           => (MatrixI (pi (colPos).col.asInstanceOf [Vector [VectorI]]),
                                 col(colPosV).asInstanceOf [VectorI])
        case SPARSE          => (SparseMatrixI (pi (colPos).col.asInstanceOf [Vector [VectorI]]),
                                 col(colPosV).asInstanceOf [VectorI])
        case SYM_TRIDIAGONAL => (SymTriMatrixI (pi (colPos).col.asInstanceOf [Vector [VectorI]]),
                                 col(colPosV).asInstanceOf [VectorI])
        case BIDIAGONAL      => (BidMatrixI (pi (colPos).col.asInstanceOf [Vector [VectorI]]),
                                 col(colPosV).asInstanceOf [VectorI])
        } // match
    } // toMatriII

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save 'this' relation in a file using serialization.
     */
    def save ()
    {
        val oos = new ObjectOutputStream (new FileOutputStream (DB_DIR + name + SER))
        oos.writeObject (this)
        oos.close ()
    } // save

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' relation into a CSV file with each row written to a line.
     *  @param fileName  the file name of the data file
     */
    def writeCSV (fileName: String)
    {
        val out = new PrintWriter (DATA_DIR + fileName)
        for (i <- 0 until rows) out.println (row(i).toString.drop (7).dropRight (1))
        out.close
    } // writeCSV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' relation into a JSON file.
     *  @param fileName  the file name of the data file
     */
    def writeJSON (fileName: String)
    {
        // FIX - to be implemented
    } // writeJSON
  
} // Relation class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest` object tests the operations provided by `Relation`.
 *  > run-main scalation.relalgebra.RelationTest
 */
object RelationTest extends App
{
    val weekdays = Relation ("weekdays", Seq ("day", "time"),
                              Vector (VectorS ("Mon", "Tue", "Wed", "Thu", "Fri"),
                                      VectorD (5.00, 8.15, 6.30, 9.45, 7.00)),
                              0, "SD")

    val weekend = Relation ("weekends", Seq ("day", "time"),
                              Vector (VectorS ("Sat", "Sun"),
                                      VectorD (3.00, 4.30)))

    println ("--------------------------------------------")
    println ("weekdays                                  = " + weekdays)
    println ("--------------------------------------------")
    println ("weekend                                   = " + weekend)
    println ("--------------------------------------------")
    println ("weekdays.pi (\"day\")                     = " + weekdays.pi ("day"))
    println ("--------------------------------------------")
    println ("weekdays.pisigmaS (\"day\", _ == \"Mon\") = " + weekdays.pisigmaS ("day", _ == "Mon"))
    println ("--------------------------------------------")
    println ("weekdays.sigmaS (\"day\", _ == \"Mon\")   = " + weekdays.sigmaS ("day", _ == "Mon"))
    println ("--------------------------------------------")
    println ("weekdays.sigma (\"day\", _ == \"Mon\")    = " + weekdays.sigma ("day", (x: StrNum) => x == "Mon"))
    println ("--------------------------------------------")
    println ("weekdays.sigma (\"time\", _ == 5.00)      = " + weekdays.sigma ("time", (x: Double) => x == 5.00))
    println ("--------------------------------------------")
    println ("weekdays.sigmaS (\"day\", _ > \"Mon\")    = " + weekdays.sigmaS ("day", _ > "Mon"))
    println ("--------------------------------------------")
    println ("weekdays.selectS (\"day\", _ > \"Mon\")   = " + weekdays.selectS ("day", _ > "Mon"))
    println ("--------------------------------------------")
    println ("weekdays.sigmaSD (\"day\", \"time\")      = " + weekdays.sigmaS ("day",  _ == "Mon")
                                                                      .sigmaD ("time", _ == 5.00))

    val week = weekdays.union (weekend)
    println ("--------------------------------------------")
    println ("weekdays.union (weekend)                  = " + week)
    println ("--------------------------------------------")
    
    weekend.add (Vector ("Zday", 1.00))
    println ("weekend add (\"Zday\", 1.00))             = " + weekend)
    println ("--------------------------------------------")
    println ("week - weekend                            = " + (week - weekend))
    println ("--------------------------------------------")
    println ("week.join (\"day\", \"day\" weekend)      = " + week.join ("day", "day", weekend))
    println ("--------------------------------------------")
    println ("week >< weekend                           = " + (week >< weekend))

    week.writeCSV ("relalgebra" + SEP + "week.csv")

} // RelationTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest2` object tests the operations provided by `Relation`.
 *  The realational algebra operators are given using Unicode.
 *  @see en.wikipedia.org/wiki/List_of_Unicode_characters
 *  > run-main scalation.relalgebra.RelationTest2
 */
object RelationTest2 extends App
{
    val weekdays = Relation ("weekdays", Seq ("day", "time"),
                              Vector (VectorS ("Mon", "Tue", "Wed", "Thu", "Fri"),
                                      VectorD (5.00, 8.15, 6.30, 9.45, 7.00)),
                              0, "SD")

    val weekend = Relation ("weekends", Seq ("day", "time"),
                              Vector (VectorS ("Sat", "Sun"),
                                      VectorD (3.00, 4.30)),
                              0, "SD")

    println ("weekdays.π (\"day\")               = " + weekdays.π ("day"))
    println ("-------------------------------------")
    println ("weekdays.π (\"time\")              = " + weekdays.π ("time"))
    println ("-------------------------------------")
    println ("weekdays.σ (\"day\", _ == \"Mon\") = " + weekdays.σ ("day", (x: StrNum) => x == "Mon"))
    println ("-------------------------------------")
    println ("weekdays.σ (\"time\", _ == 5.00)   = " + weekdays.σ ("time", (x: Double) => x == 5.00))
    println ("-------------------------------------")
    println ("weekdays.σ (\"day\", _ > \"Mon\")  = " + weekdays.σ ("day", (x: StrNum) => x > "Mon"))
    println ("-------------------------------------")
    println ("weekdays.σ (\"time\", _ > 5.00)    = " + weekdays.σ ("time", (x: Double) => x > 5.00))
    println ("-------------------------------------")
    println ("weekdays.σ (\"day\", \"time\")     = " + weekdays.σ ("day",  (x: StrNum) => x == "Mon")
                                                               .σ ("time", (x: Double) => x == 5.00))
    val week = weekdays.union (weekend)
    println ("-------------------------------------")
    println ("weekdays ⋃ weekend)                = " + weekdays ⋃ weekend)
    println ("-------------------------------------")
    println ("week - weekend                     = " + (week - weekend))
    println ("-------------------------------------")
    println ("week ⋈ weekend                     = " + (week ⋈ weekend))

} // RelationTest2 object

