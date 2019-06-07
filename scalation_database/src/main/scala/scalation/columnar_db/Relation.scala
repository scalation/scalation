
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Yang Fan, Vinay Bingi, Santosh Uttam Bobade
 *  @version 1.6
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
 *  Some of the operators have unicode versions: @see `scalation.util.UnicodeTest`
 *
 *  Supports Time Series Databases (TSDB) via `TimeNum` domain/datatype and 'leftJoinApx'
 *  'rightJoinApx' methods.
 */

package scalation
package columnar_db

import java.io._

import scala.collection.mutable.{ArrayBuffer, HashMap, Map}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.math.{min => MIN}
import scala.reflect.ClassTag

import scalation.linalgebra._
import scalation.linalgebra.Vec_Elem.{<, =~, !=~}
import scalation.linalgebra.MatrixKind._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO.StrNum
import scalation.math.TimeO.{TimeNum, setThreshold}
import scalation.math.{noComplex, noDouble, noInt, noLong, noRational, noReal, noStrNum, noTimeNum}
import scalation.util.{banner, Error, getFromURL_File, MergeSortIndirect, ReArray, removeAt, time}

import TableObj._
import columnar_db._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Relation` companion object provides additional functions for the `Relation`
 *  class.
 */
object Relation
       extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an unpopulated relation.
     *  @param name     the name of the relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     *  @param colName  the names of columns
     */
    def apply (name: String, key: Int, domain: String, colName: String*): Relation =
    {
        val n = colName.length
        new Relation (name, colName, Vector.fill [Vec] (n)(null), key, domain)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from a sequence of row/tuples.  These rows must be converted
     *  to columns.
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param row      the sequence of rows to be converted to columns for the columnar relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def apply (name: String, colName: Seq [String], row: Seq [Row], key: Int, domain: String): Relation =
    {
        val equivCol = Vector.fill [Vec] (colName.length)(null)
        val r2 = new Relation (name, colName, equivCol, key, domain)
        for (tuple <- row) r2.add (tuple)
        r2.materialize ()
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from a sequence of row/tuples.  These rows must be converted
     *  to columns.
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param row      the sequence of rows to be converted to columns for the columnar relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     */
    def apply (name: String, colName: Seq [String], row: Seq [Row], key: Int): Relation =
    {
        val equivCol = Vector.fill [Vec] (colName.length)(null)
        val r2 = new Relation (name, colName, equivCol, key, null)
        for (tuple <- row) r2.add (tuple)
        r2.materialize ()
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory using serialization.
     *  @param name  the name of the relation to load
     */
    def apply (name: String): Relation =
    {
        val ois = new ObjectInputStream (new FileInputStream (STORE_DIR + name + SER))
        val obj = ois.readObject ()
        ois.close ()
        val res = obj.asInstanceOf [Relation]
        res
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory loading its columns
     *  with data from the CSV file named 'fileName'.
     *  Note: "ln.split (eSep, -1)" will keep all values even if empty "one,,three" -> "one","",three"
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param colName   the names of columns
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     *  @param skip      the number of lines in the CSV file to skip (e.g., header line(s))
     *  @param eSep      the element separation string/regex (e.g., "," ";" " +")
     */
    def apply (fileName: String, name: String, colName: Seq [String], key: Int,
               domain: String, skip: Int, eSep: String): Relation =
    {
        var cnt    = skip
        val lines  = getFromURL_File (fileName)
        val newCol = Vector.fill [Vec] (colName.length)(null)
        val r3     = new Relation (name, colName, newCol, key, domain)
        for (ln <- lines) {
            if (cnt <= 0) r3.add (r3.row (ln.split (eSep, -1), domain)) else cnt -= 1
        } // for
        r3.materialize ()
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory loading its columns
     *  with data from the CSV file named 'fileName'.  In this version, the column
     *  names are read from the first line of the file.
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     *  @param eSep      the element separation string/regex (e.g., "," ";" " +")
     *  @param cPos      the sequence of column positions in the input file to be used (null => select all)
     */
    def apply (fileName: String, name: String, key: Int, domain: String, eSep: String,
               cPos: Seq [Int]): Relation =
    {
        val lines = getFromURL_File (fileName)
        var first = true
        var colBuffer: Array [ArrayBuffer [String]] = null
        var colName:   Array [String] = null
        var newCol:    Vector [Vec] = null

        if (cPos == null) {                                              // select all columns
            for (ln <- lines) {
                if (first) {
                    colName   = ln.split (eSep, -1).map (_.trim)
                    colBuffer = Array.fill (colName.length)(new ArrayBuffer ())
                    first = false
                } else {
                    val values = ln.split (eSep, -1).map (_.trim)
                    for (i <- colName.indices) colBuffer(i) += values(i)
                } // if
            } // for
        } else {                                                        // select cPos columns
            if (domain.length != cPos.length) {
                flaw ("apply", "cPos length should be same as domain length")
            } // if
            for (ln <- lines) {
                if (first) {
                    val name  = ln.split (eSep, -1).map (_.trim)
                    colName   = Array.ofDim (cPos.length)
                    colBuffer = Array.fill (cPos.length)(new ArrayBuffer ())
                    for (i <- colBuffer.indices) colName(i) = name(cPos(i))
                    first = false
                } else {
                    val values = ln.split (eSep, -1).map (_.trim)
                    for (i <- colName.indices) colBuffer(i) += values(cPos(i))
                } // if
            } // for
        } // if
        new Relation (name, colName, makeCol (colBuffer, domain), key, domain)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make the columns for the columnar table from data stored in 'colBuffer'.
     *  @param colBuffer  the column buffer holding the data
     *  @param domain     the domains/datatypes for the columns
     */
    private def makeCol (colBuffer: Array [ArrayBuffer [String]], domain: String): Vector [Vec] =
    {
        colBuffer.indices.map (i =>
            if (domain == null || domain == "") VectorS (colBuffer(i).toArray)
            else domain(i) match {
                case 'C' => VectorC (colBuffer(i).toArray)             // dense vectors
                case 'D' => VectorD (colBuffer(i).toArray)
                case 'I' => VectorI (colBuffer(i).toArray)
                case 'L' => VectorL (colBuffer(i).toArray)
                case 'Q' => VectorQ (colBuffer(i).toArray)
                case 'R' => VectorR (colBuffer(i).toArray)
                case 'S' => VectorS (colBuffer(i).toArray)
                case 'T' => VectorT (colBuffer(i).toArray)

                case 'c' => RleVectorC (colBuffer(i).toArray)          // compressed vectors
                case 'd' => RleVectorD (colBuffer(i).toArray)
                case 'i' => RleVectorI (colBuffer(i).toArray)
                case 'l' => RleVectorL (colBuffer(i).toArray)
                case 'q' => RleVectorQ (colBuffer(i).toArray)
                case 'r' => RleVectorR (colBuffer(i).toArray)
                case 's' => RleVectorS (colBuffer(i).toArray)
                case 't' => RleVectorT (colBuffer(i).toArray)

                case 'χ' => SparseVectorC (colBuffer(i).toArray)       // sparse vectors
                case 'δ' => SparseVectorD (colBuffer(i).toArray)
                case 'ι' => SparseVectorI (colBuffer(i).toArray)
                case 'λ' => SparseVectorL (colBuffer(i).toArray)
                case 'ϟ' => SparseVectorQ (colBuffer(i).toArray)
                case 'ρ' => SparseVectorR (colBuffer(i).toArray)
                case 'σ' => SparseVectorS (colBuffer(i).toArray)
                case 'τ' => SparseVectorT (colBuffer(i).toArray)

                case _   => flaw ("makeCol", s"domain type ${domain(i)} not supported")
                            null.asInstanceOf [Vec]
        }).toVector
    } // makeCol

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory loading its columns
     *  with data from the CSV file named 'fileName'.  In this version, the column
     *  names are read from the first line of the file.  It uses 'col2' which is a
     *  temporary ReArray, and maintains indices.
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     *  @param eSep      the element separation string/regex (e.g., "," ";" " +")
     */
    def apply (fileName: String, name: String, domain: String, key: Int, eSep: String = ","): Relation =
    {
        var first         = true
        val lines         = getFromURL_File (fileName)
        var r3: Relation  = null
        var currentlineno = 0

        for (ln <- lines) {
            if (first) {
                val colName = ln.split (eSep, -1)
                val newCol  = Vector.fill [Vec] (colName.length)(null)
                r3    = new Relation (name, colName, newCol, key, domain)
                first = false
            } else {
                if (currentlineno % 1000 == 0) println (s"$currentlineno")
                r3.add (r3.row (ln.split (eSep, -1), domain))
                currentlineno += 1
            } // if
        } // for
        r3.materialize ()
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory loading its columns
     *  with data from the CSV file named 'fileName'.  This version assumes
     *  defaults for 'eSep' and 'skip' of ("," and 0).
     *  @param fileName  the file name of the data file
     *  @param name      the name of the relation
     *  @param colName   the names of columns
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def apply (fileName: String, name: String, colName: Seq [String], key: Int,
               domain: String): Relation =
    {
        val eSep   = ","
        val lines  = getFromURL_File (fileName)
        val newCol = Vector.fill [Vec] (colName.length)(null)
        val r3     = new Relation (name, colName, newCol, key, domain)
        for (ln <- lines) r3.add (r3.row (ln.split (eSep, -1), domain))
        r3.materialize ()
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the relation with the given 'name' into memory loading its columns
     *  with data from the '.arff' file named 'fileName'.
     *  @param fileName  the file name of the data file
     *  @param key       the column number for the primary key (< 0 => no primary key)
     *  @param domain    an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def apply (fileName: String, key: Int, domain: String): Relation =
    {
        val eSep = "[, ]"
        val lines = getFromURL_File (fileName)
        var name: String = null
        var colBuffer: Array [ArrayBuffer [String]] = null
        var colName = ArrayBuffer [String]()
        var newCol: Vector [Vec] = null
        var foundData = false
        for (ln <- lines) {
            if (ln.indexOf ("%") == 0) {
                // skip comment
            } else if (ln.indexOf ("@relation") == 0) {
                name = ln.split (eSep, -1)(1)
            } else if (ln.indexOf ("@attribute") == 0) {
                colName += ln.split(eSep, -1)(1)
            } else if (ln.indexOf ("@data") == 0) {
                foundData = true
                colBuffer = Array.ofDim (colName.length)
                for (i <- colBuffer.indices) colBuffer (i) = new ArrayBuffer ()
            } else if (foundData) {
                val values = ln.split (eSep, -1)
                values.indices.foreach (i => { colBuffer (i) += values (i) })
            } // if
        } // for
        new Relation (name, colName, colBuffer.indices.map (i => VectorS (colBuffer(i).toArray)).toVector, key, domain)
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
    /** Create a relation from the 'xy' matrix of doubles.
     *  @param xy       the matrix containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def fromMatriD (xy: MatriD, name: String, colName: Seq [String], key: Int = -1,
                    domain: String = null): Relation =
    {
        val newCol = for (j <- 0 until xy.dim2) yield xy.col (j).asInstanceOf [Vec]
        new Relation (name, colName, newCol.toVector, key, domain)
    } // fromMatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the 'x' matrix of doubles and 'y' vector of doubles
     *  or integers.
     *  @param x        the matrix containing the data
     *  @param y        the vector containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def fromMatriD_ (x: MatriD, y: Vec, name: String, colName: Seq [String], key: Int = -1,
                    domain: String = null): Relation =
    {
        val newCol = for (j <- 0 until x.dim2) yield x.col (j).asInstanceOf [Vec]
        new Relation (name, colName, newCol.toVector :+ y, key, domain)
    } // fromMatriD_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the 'xy' matrix of integers.
     *  @param xy       the matrix containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def fromMatriI (xy: MatriI, name: String, colName: Seq [String], key: Int = -1,
                    domain: String = null): Relation =
    {
        val newCol = for (j <- 0 until xy.dim2) yield xy.col (j).asInstanceOf [Vec]
        new Relation (name, colName, newCol.toVector, key, domain)
    } // fromMatriI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a relation from the 'xy' matrix of integers and 'y' vector of integers.
     *  @param x        the matrix containing the data
     *  @param y        the vector containing the data
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def fromMatriII (x: MatriI, y: VectorI, name: String, colName: Seq [String], key: Int = -1,
                     domain: String = null): Relation =
    {
        val newCol = for (j <- 0 until x.dim2) yield x.col (j).asInstanceOf [Vec]
        new Relation (name, colName, newCol.toVector :+ y, key, domain)
    } // fromMatriII

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the count (number of elements) of each of the columns of columnar
     *  relation 'r'.
     *  @param r  the given relation
     */
    def count (r: Relation): Seq [Any] = for (j <- r.col.indices) yield r.col(j).size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of each of the columns of columnar relation 'r'.
     *  @param r  the given relation
     */
    def min (r: Relation): Seq [Any] = for (c <- r.col) yield Vec.min (c)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of each of the columns of columnar relation 'r'.
     *  @param r  the given relation
     */
    def max (r: Relation): Seq [Any] = for (c <- r.col) yield Vec.max (c)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of each of the columns of columnar relation 'r'.
     *  @param r  the given relation
     */
    def sum (r: Relation): Seq [Any] = for (c <- r.col) yield Vec.sum (c)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of each of the columns of columnar relation 'r'.
     *  @param r  the given relation
     */
    def mean (r: Relation): Seq [Any] = for (c <- r.col) yield Vec.mean (c)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of each of the columns of columnar relation 'r'.
     *  @param r  the given relation
     */
    def Ɛ (r: Relation): Seq [Any] = for (c <- r.col) yield Vec.mean (c)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the variance of each of the columns of columnar relation 'r'.
     *  @param r  the given relation
     */
    def variance (r: Relation): Seq [Any] = for (c <- r.col) yield Vec.variance (c)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the variance of each of the columns of columnar relation 'r'.
     *  @param r  the given relation
     */
    def Ʋ (r: Relation): Seq [Any] = for (c <- r.col) yield Vec.variance (c)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the correlation of column 'i' and 'j' within columnar relation 'r'.
     *  @param r  the given relation
     *  @param i  the first column vector
     *  @param j  the second column vector
     */
    def corr (r: Relation, i: Int = 0, j: Int = 1): Double = Vec.corr (r.col(i), r.col(j))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vec of sum of the 'cName' column for the 'r' relation base on each group,
     *  the result will be the same size.
     *  @param r      the relation to operate on
     *  @param cName  sum on column "cName"
     */
    def sum (r: Relation, cName: String): Vec =
    {
        val cPos    = r.colMap.get(cName).get
        val domainc = r.domain(cPos)
        var columnlist:Vec = null
        var count   = 0
        var pointer = 0
        var sumlist: Vec = null
        for (idx <- r.orderedIndex) {
//          columnlist = Vec.:+ (columnlist,r.index(idx)(cPos),r.domain,cPos)
            columnlist = Vec.:+ (columnlist,r.index(idx)(cPos))
            if (count +1 == r.grouplist(pointer)) {
                val thisroundsum = Vec.sum(columnlist)
//              sumlist = Vec.:+ (sumlist, thisroundsum, r.domain, cPos)
                sumlist = Vec.:+ (sumlist, thisroundsum)
                columnlist = null
                pointer += 1
            } // if
            count += 1
        } // for
        sumlist
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vec of max of the 'cName' column for the 'r' relation.
     *  @param r the relation you want to operate on
     *  @param cName  max on column "cName"
     */
    def max (r: Relation, cName: String): Vec =
    {
        val cPos    = r.colMap.get(cName).get
        val domainc = r.domain(cPos)
        var columnlist:Vec = null
        var count   = 0
        var pointer = 0
        var maxlist: Vec=null
        for(idx <- r.orderedIndex) {
//          columnlist = Vec.:+ (columnlist,r.index(idx)(cPos),r.domain,cPos)
            columnlist = Vec.:+ (columnlist,r.index(idx)(cPos))
            if (count +1 == r.grouplist(pointer)) {
                val thisroundsum = Vec.max(columnlist)
//              maxlist = Vec.:+ (maxlist, thisroundsum, r.domain, cPos)
                maxlist = Vec.:+ (maxlist, thisroundsum)
                columnlist = null
                pointer += 1
            } // if
            count += 1
        } // for
        maxlist
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vec of min of the 'cName' column for the 'r' relation
     *  @param r      the relation you want to operate on
     *  @param cName  min on column "cName"
     */
    def min (r: Relation, cName: String): Vec =
    {
        val cPos    = r.colMap.get(cName).get
        val domainc = r.domain(cPos)
        var columnlist:Vec = null
        var count   = 0
        var pointer = 0
        var minlist:Vec=null
        for (idx <- r.orderedIndex) {
//          columnlist = Vec.:+ (columnlist,r.index(idx)(cPos),r.domain,cPos)
            columnlist = Vec.:+ (columnlist,r.index(idx)(cPos))
            if (count +1 == r.grouplist(pointer)) {
                val thisroundsum = Vec.min(columnlist)
//              minlist = Vec.:+ (minlist, thisroundsum, r.domain, cPos)
                minlist = Vec.:+ (minlist, thisroundsum)
                columnlist = null
                pointer += 1
            } // if
            count += 1
        } // for
        minlist
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vec of average of the 'cName' column for the 'r' relation.
     *  @param r      the relation you want to operate on
     *  @param cName  average on column "cName"
     */
    def avg (r: Relation, cName: String): Vec =
    {
        val cPos    = r.colMap.get(cName).get
        val domainc = r.domain(cPos)
        var columnlist: Vec = null
        var count   = 0
        var pointer = 0
        var avglist: Vec = null
        for (idx <- r.orderedIndex) {
//          columnlist = Vec.:+ (columnlist, r.index(idx)(cPos), r.domain, cPos)
            columnlist = Vec.:+ (columnlist, r.index(idx)(cPos))
            if (count + 1 == r.grouplist(pointer)) {
                val thisroundsum = Vec.mean(columnlist)
//              avglist = Vec.:+ (avglist, thisroundsum, r.domain, cPos)
                avglist = Vec.:+ (avglist, thisroundsum)
                columnlist = null
                pointer += 1
            } // if
            count += 1
        } // for
        avglist
    } // avg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a Vec of count of the 'cName' column for the 'r' relation.
     *  @param r      the relation you want to operate on
     *  @param cName  the column name for the column to be counted
     */
    def count (r: Relation, cName: String): Vec =
    {
        val cPos = r.colMap.get(cName).get
        var countlist: Vec = null
        var i = 0
        for (p <- r.grouplist) {
            val count = p - i
//          countlist = Vec.:+ (countlist, count, r.domain, cPos)
            countlist = Vec.:+ (countlist, count)
            i = p
        } // for
        countlist
    } // count

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From function return cartesian product of all the relations.
     *  @param relations  the relations making up the from clause
     */
    def from (relations: Relation*): Relation =
    {
        var result = relations(0)
        for (i <- 1 until relations.size) result = result product relations(i)
        result
    } // from

} // Relation object

import Relation._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Relation` class stores and operates on vectors.  The vectors form the
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
 *      T - `TimeNum`  - `VectorT` -  96 bit time Instant = (Long, Int)
 *  <p>
 *  FIX - (1) don't allow (public) var
          (2) avoid unchecked or incomplete .asInstanceOf [T]
 *------------------------------------------------------------------------------
 *  @param name     the name of the relation
 *  @param colName  the names of columns
 *  @param col      the Scala Vector of columns making up the columnar relation
 *  @param key      the column number for the primary key (< 0 => no primary key)
 *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
 *  @param fKeys    an optional sequence of foreign keys - Seq (column name, ref table name, ref column position)
 *  @param enter    whether to enter the newly created relation into the `Catalog`
 */
class Relation (val name: String, val colName: Seq [String], var col: Vector [Vec],
                val key: Int = 0, val domain: String = null, var fKeys: Seq [(String, String, Int)] = null,
                enter: Boolean = true)
     extends Table with Error with Serializable
{
    private   val DEBUG              = true                                           // debug flag
    private [columnar_db] val colMap = Map [String, Int] ()                           // map column name -> column number
    @transient
//  private   val col2               = Vector.fill (colName.size)(new ReArray [Any])  // efficient holding area for building columns
//  private   val col2               = Vector [ReArray [Any]] ()                      // efficient holding area for building columns
    private   var grouplist          = Vector [Int] ()                                // rows in group
    protected val index              = Map [KeyType, Row] ()                          // index that maps a key into row
    protected val indextoKey         = HashMap [Int, KeyType] ()                      // map index -> key
    private   var keytoIndex         = HashMap [KeyType, Int] ()                      // map key -> index
    protected var orderedIndex       = Vector [KeyType] ()                            // re-ordering of the key column

    if (colName.length != col.length) flaw ("constructor", "incompatible sizes for 'colName' and 'col'")
    if (enter) Catalog.add (name, colName, key, domain)

    for (j <- colName.indices) colMap += colName(j) -> j
    private val col2 = 
        if (domain == null) (for (j <- colName.indices) yield new ReArray [Any] ()).toVector
        else (for (j <- colName.indices) yield 
            domain(j) match {
            case 'C' => new ReArray [Complex] ()
            case 'D' => new ReArray [Double] ()
            case 'I' => new ReArray [Int] ()
            case 'L' => new ReArray [Long] ()
            case 'Q' => new ReArray [Rational] ()
            case 'R' => new ReArray [Real] ()
            case 'S' => new ReArray [StrNum] ()
            case 'T' => new ReArray [TimeNum] ()
            case _   => { flaw ("constructor", s"unsupported column type ${domain(j)} for column $j"); null }
        }).toVector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'generateIndex' method helps, e.g., the 'popTable', methods to generate
     *  an index for the table.
     *  @param reset  if reset is true, use old index to build new index; otherwise, create new index
     */
    def generateIndex (reset: Boolean = false)
    {
        if (! reset) {                                                      // create new index
            for (i <- 0 until rows) {
                val mkey = if (key != -1) new KeyType (row(i)(key))         // key column is specified
                           else new KeyType(i)                              // key column is not specified
                val tuple    = row(i)
                index       += mkey -> tuple
                indextoKey  += i -> mkey
                keytoIndex  += mkey -> i
                orderedIndex = orderedIndex :+ mkey
            } // for
        } else {                                                            // use old index to build
            val newoderedIndex = new ReArray [KeyType] ()
            val newkeytoIndex =  new HashMap [KeyType, Int] ()
            for (i <- orderedIndex.indices) {
                val mkey       = if (key != -1) orderedIndex(i) else new KeyType (i)
                val tuple      = row(keytoIndex(mkey))
                index         += mkey -> tuple
                newkeytoIndex += mkey -> i
                newoderedIndex.update (newoderedIndex.length, mkey)
            } // for
            orderedIndex = newoderedIndex.toVector                          // map old keytoIndex to rowIndex to
            keytoIndex   = newkeytoIndex
        } // if
    } // generateIndex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of columns in the relation.
     */
    def cols: Int = col.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return all of the columns in the relation.
     */
    def columns: Vector [Vec] = col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the column in the relation with column name 'cName'.
     *  @param cName  column name used to retrieve the column vector
     */
    def column (cName: String): Vec = col(colMap (cName))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the names of columns in the relation.
     */
    def colNames: Seq [String] = colName

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mapping from column names to column positions.
     */
    def colsMap: Map [String, Int] = colMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the domains for the columns in the relation.
     */
    def domains: String = domain

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row by pulling values from all columns at position 'i'.
     *  @param i  the 'i'th position
     */
    def row (i: Int): Row = col.map (Vec (_, i)).toVector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size in terms of number of rows in the relation.
     */
    def rows: Int = if (col(0) == null) 0 else col(0).size

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
    /** Rename 'this' table, returning a shallow copy of 'this' table.
     *  @param newName  the new name for the table.
     */
    def rename (newName: String): Relation =
    {
        new Relation (newName, colName, col, key, domain, fKeys)
    } // rename

    // ================================================================= PROJECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column names.
     *  @param cName  the names of the columns to project onto
     */
    def project (cName: String*): Relation = project (cName.map (colMap (_)), cName)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Project onto the columns with the given column positions using the given
     *  column names.
     *  @param cPos   the positions of the columns to project onto
     *  @param cName  the names of the columns to project onto
     */
    def project (cPos: Seq [Int], cName: Seq [String] = null): Relation =
    {
        val newCName  = if (cName == null) cPos.map (colName(_)) else cName
        val newCol    = cPos.map (col(_)).toVector
        val newKey    = if (cPos contains key) cPos.indexOf (key) else -1
        val newDomain = projectD (domain, cPos)
        new Relation (name + "_p_" + ucount (), newCName, newCol, newKey, newDomain)
    } // project

    // ======================================================== EXTENDED PROJECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate/project on the given columns (an extended projection operator that
     *  applies aggregate operators to aggregation columns and regular projection
     *  to projection columns).
     *  @see en.wikipedia.org/wiki/Relational_algebra
     *  @param aggCol  the columns to aggregate on: (aggregate function, new column name, old column name)*
     *  @param cName   the other columns to project on
     */
    def eproject (aggCol: AggColumn*)(cName: String*): Relation =
    {
        val aRange  = 0 until aggCol.size
        val nCols   = aggCol.size + cName.size
        val funName = ArrayBuffer [String] ()
        for (c <- aggCol) {
            if (! (colName contains c._3)) throw new IllegalArgumentException (s"column ${c._3} to aggregate on does not exist")
            else funName += c._2
        } // for

        if (grouplist.isEmpty) groupBy (colName(key))
        val newCol    = Vector.fill [Vec] (nCols)(null)
        val newCName  = cName ++ funName
        var newDomain = cName.map (n => colMap(n)).map (i => domain(i))
        for (i <- aRange) {
            newDomain = if (funName(i) contains "count") newDomain :+ 'I'          // aggregate's result domain is based on aggregate column
                        else newDomain :+ domain(colMap(aggCol(i)._3))
        } // for
        val r2 = new Relation (name + "_e_" + ucount (), newCName, newCol, key, newDomain.mkString (""))
        if (rows == 0) return r2                                                   // no rows means early return

        val agglist = for (i <- aRange) yield aggCol(i)._1(this, aggCol(i)._3)
        if (cName.size != 0) {
            val cPos    = cName.map (colMap(_))                                    // position of cName
            val cPos2   = aggCol.map ((a: AggColumn) => colMap(a._3))              // position of aggregate columns
            val shrinkR = pi(cPos, null)                                           // projected relation
            var row_i   = 0
            var group_j = 0
            orderedIndex.foreach (idx => {
                var thisrow = shrinkR.row(keytoIndex(idx))
                for (aggf <- agglist.indices) thisrow = thisrow :+ Vec (agglist(aggf), group_j)
                r2.add_ni (thisrow)
                row_i += 1
                if (row_i == grouplist(group_j)) group_j += 1
            }) // foreach
            r2.materialize ()
        } else {                                                                   // only project on the aggregate column
            for (i <- aRange) {
                r2.col = if (i == 0) Vector (agglist(i)) else r2.col :+ agglist(i)
            } // for
        } // if
        r2
    } // eproject

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Method 'epiAny' is a special case of epi.  When the projected columns can not be
     *  decided by the group by columns, only one representative will be shown for each group.
     *  FIX - change name
     *  @param aggF      the aggregate functions you want to use
     *  @param funName   the newly created aggregate columns'names
     *  @param aggFAttr  the columns you want to use of correspondent aggregate functions
     *  @param cName     the columns you want to project on
     */
    def epiAny (aggF: Seq [AggFunction], funName: Seq [String], aggFAttr: Seq [String], cName: String*): Relation =
    {
        aggFAttr.foreach (a =>
            if (! colName.contains(a)) throw new IllegalArgumentException("the attribute you want to aggregate on does not exists"))
        cName.foreach (a =>
            if (! colName.contains(a)) throw new IllegalArgumentException("the attribute you want to project on does not exists"))

        if (grouplist.isEmpty) groupBy (colName(key))
        val newCol = Vector.fill [Vec](aggFAttr.size + cName.size)(null)
        val colNamenew = cName ++ funName
        var newDomain = cName.map (n => colMap(n)).map (i => domain(i))
        for (i <- funName.indices) {
            newDomain = if (funName(i) contains "count") newDomain :+ 'I'
                        else newDomain :+ domain(colMap(aggFAttr(i)))
        } // for
        val r2 = new Relation (name + "_e_" + ucount (), colNamenew, newCol, key, newDomain.mkString (""))
        if (rows == 0) return r2

        val agglist = for (i <- aggF.indices) yield aggF(i)(this, aggFAttr(i))
        var group_j = 0
        if (cName.size != 0) {
            val cPos = cName.map (colMap(_))
            val shrinkR = pi(cPos, null)
            grouplist.foreach (idx => {
                var newrow: Vector[Any] = null
                val rownumber = keytoIndex(orderedIndex(idx-1))
                newrow        = shrinkR.row(rownumber)
                for (i<- aggF.indices) {
                    val aggtemp = Vec (agglist(0), group_j)
                    newrow      = newrow:+ aggtemp
                } // for
                r2.add_ni (newrow)
                group_j += 1
            }) // foreach
        } // if
        r2.materialize ()
    } // epiAny

    // ========================================================== PROJECT-SELECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from column 'cName' in 'this' relation that satisfy the
     *  predicate 'p' and project onto that column.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def pisigmaC (cName: String, p: Complex => Boolean): Relation =
    {
        val nu     = getMeta (cName)
        val newCol = Vector (col (nu._1).asInstanceOf [VectorC].filter (p))
        new Relation (name + "_s_" + ucount (), nu._2, newCol, nu._3, nu._4)
    } // pisigmaC

    def pisigmaD (cName: String, p: Double => Boolean): Relation =
    {
        val nu     = getMeta (cName)
        val newCol = Vector (col (nu._1).asInstanceOf [VectorD].filter (p))
        new Relation (name + "_s_" + ucount (), nu._2, newCol, nu._3, nu._4)
    } // pisigmaD

    def pisigmaI (cName: String, p: Int => Boolean): Relation =
    {
        val nu     = getMeta (cName)
        val newCol = Vector (col (nu._1).asInstanceOf [VectorI].filter (p))
        new Relation (name + "_s_" + ucount (), nu._2, newCol, nu._3, nu._4)
    } // pisigmaI

    def pisigmaL (cName: String, p: Long => Boolean): Relation =
    {
        val nu     = getMeta (cName)
        val newCol = Vector (col (nu._1).asInstanceOf [VectorL].filter (p))
        new Relation (name + "_s_" + ucount (), nu._2, newCol, nu._3, nu._4)
    } // pisigmaL

    def pisigmaQ (cName: String, p: Rational => Boolean): Relation =
    {
        val nu     = getMeta (cName)
        val newCol = Vector (col (nu._1).asInstanceOf [VectorQ].filter (p))
        new Relation (name + "_s_" + ucount (), nu._2, newCol, nu._3, nu._4)
    } // pisigmaQ

    def pisigmaR (cName: String, p: Real => Boolean): Relation =
    {
        val nu     = getMeta (cName)
        val newCol = Vector (col (nu._1).asInstanceOf [VectorR].filter (p))
        new Relation (name + "_s_" + ucount (), nu._2, newCol, nu._3, nu._4)
    } // pisigmaR

    def pisigmaS (cName: String, p: StrNum => Boolean): Relation =
    {
        val nu     = getMeta (cName)
        val newCol = Vector (col (nu._1).asInstanceOf [VectorS].filter (p))
        new Relation (name + "_s_" + ucount (), nu._2, newCol, nu._3, nu._4)
    } // pisigmaS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get meta-data about the column with name 'cName'.
     *  @param cName  the name of the column
     */
    private def getMeta (cName: String): (Int, Seq [String], Int, String) =
    {
        val cn = colMap (cName)                                 // column position
        (cn, Seq (cName), if (cn == key) key else -1, projectD (domain, Seq (cn)))
    } // getMeta

    // ================================================================== SELECT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' relation that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def select [T : ClassTag] (cName: String, p: T => Boolean): Relation =
    {
        if (domain != null) {
            domain(colMap (cName)) match {
            case 'C' | 'c' | 'χ' => selectAt (selectC (cName, p.asInstanceOf [Complex => Boolean]))
            case 'D' | 'd' | 'δ' => selectAt (selectD (cName, p.asInstanceOf [Double => Boolean]))
            case 'I' | 'i' | 'ι' => selectAt (selectI (cName, p.asInstanceOf [Int => Boolean]))
            case 'L' | 'l' | 'λ' => selectAt (selectL (cName, p.asInstanceOf [Long => Boolean]))
            case 'Q' | 'q' | 'ϟ' => selectAt (selectQ (cName, p.asInstanceOf [Rational => Boolean]))
            case 'R' | 'r' | 'ρ' => selectAt (selectR (cName, p.asInstanceOf [Real => Boolean]))
            case 'S' | 's' | 'σ' => selectAt (selectS (cName, p.asInstanceOf [StrNum => Boolean]))
            case 'T' | 't' | 'τ' => selectAt (selectT (cName, p.asInstanceOf [TimeNum => Boolean]))
            case _  => { flaw ("sigma", "predicate type not supported"); null }
            } // match
        } else {
            flaw ("select", "optional domains not given - use type specific sigma?")
            null
        } // if
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from columns in 'cName' in 'this' relation that satisfy
     *  the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
     */
    def sigmaC (cName: String, p: Complex => Boolean): Relation = selectAt (selectC (cName, p))

    def sigmaD (cName: String, p: Double => Boolean): Relation = selectAt (selectD (cName, p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The parellel version of 'selectD'.
     *  FIX - move to .par package
     *  @param cName column to select on
     *  @param p  predicate to select
     */
    def sigmaDpar (cName: String, p: Double => Boolean): Relation =
    {
        val filtercol = new scalation.linalgebra.par.VectorD (col (colMap (cName)).asInstanceOf [VectorD].toArray)
        selectAt (filtercol.filterPos (p))
    } // sigmaDpar

    def sigmaI (cName: String, p: Int => Boolean): Relation = selectAt (selectI (cName, p))

    def sigmaL (cName: String, p: Long => Boolean): Relation = selectAt (selectL (cName, p))

    def sigmaQ (cName: String, p: Rational => Boolean): Relation = selectAt (selectQ (cName, p))

    def sigmaR (cName: String, p: Real => Boolean): Relation = selectAt (selectR (cName, p))

    def sigmaS (cName: String, p: StrNum => Boolean): Relation = selectAt (selectS (cName, p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select the positions of elements from columns in 'cName' in 'this' relation
     *  that satisfy the predicate 'p'.
     *  @param cName  the name of the column used for selection
     *  @param p      the predicate (`Boolean` function) to be satisfied
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

    def selectT (cName: String, p: TimeNum => Boolean): Seq [Int] =
    {
        col (colMap (cName)).asInstanceOf [VectorT].filterPos (p)
    } // selectT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select across all columns at the specified column positions.
     *  @param pos  the specified column positions
     */
    def selectAt (pos: Seq [Int]): Relation =
    {
        val newCol = (for (j <- col.indices) yield Vec.select (col(j), pos)).toVector
        new Relation (name + "_s_" + ucount (), colName, newCol, key, domain)
    } // selectAt

    // =========================================================== SET OPERATORS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  If they are not, return the first 'this' relation.
     *  @param r2  the other relation
     */
    def union (r2: Table): Relation =
    {
        if (incompatible (r2)) return this                // take only this relation

//      if (col(0) == null) return if (r2.col(0) == null) null else r2
//      else if (r2.col(0) == null) return this

        val newCol = (for (j <- col.indices) yield Vec.++ (col(j), r2.columns(j)))
        new Relation (name + "_u_" + ucount (), colName, newCol.toVector, -1, domain)
    } // union

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  Use index to finish intersect operation.
     *  @param _r2  the other relation
     */
    def intersect (_r2: Table): Relation =
    {
        val r2 = _r2.asInstanceOf [Relation]
        if (incompatible (r2)) return null

        val newCol = Vector.fill [Vec] (colName.length) (null)
        val r3     = new Relation (name + "_u_" + ucount (), colName, newCol, -1, domain)
        for (i <- orderedIndex.indices) {
            if (r2.keytoIndex isDefinedAt orderedIndex(i)) {
                if (row(i) sameElements r2.row(r2.keytoIndex (orderedIndex(i)))) r3.add_ni (row(i))
            } // if
        } // for
        r3.materialize ()
    } // intersect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  Use index to finish intersect operation.  Superceded by method above.
     *  @param _r2  the other relation
     *
    def intersect (_r2: Table): Relation =
    {
        val r2 = _r2.asInstanceOf [Relation]
        if (incompatible (r2)) return null

        val newCol = Vector.fill [Vec] (colName.length)(null)
        val r3     = new Relation (name + "_u_" + ucount (), colName, newCol, -1, domain)
        for (key <- orderedIndex) {
            if (r2.orderedIndex contains key) {
                val thisrow = index(key)
                if (thisrow sameElements (r2.index(key))) r3.add_ni (thisrow)
            } // if
        } // for
        r3.materialize ()
    } // intersect
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' relation and 'r2'.  Check that the two relations are compatible.
     *  Slower and only to be used if there is no index.
     *  @param r2  the other relation
     */
    def intersect2 (r2: Table): Relation =
    {
        if (incompatible (r2)) return null
        val newCol = Vector.fill [Vec] (colName.length)(null)
        val r3 = new Relation (name + "_u_" + ucount (), colName, newCol.toVector, -1, domain)
        for (i <- 0 until rows if r2 contains row(i)) r3.add (row(i))
        r3.materialize ()
    } // intersect2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the difference of 'this' relation and 'r2' ('this - r2').  Check that
     *  the two relations are compatible.
     *  @param r2  the other relation
     */
    def minus (r2: Table): Relation =
    {
        if (incompatible (r2)) return null
        val newCol = Vector.fill [Vec] (colName.length)(null)
        val r3 = new Relation (name + "_m_" + ucount (), colName, newCol, key, domain)
        for (i <- 0 until rows if ! (r2 contains row(i))) r3.add (row(i))
        r3.materialize ()
    } // minus

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' relation and 'r2' are incompatible by having
     *  differing numbers of columns or differing domain strings.
     *  @param r2  the other relation/table
     */
    def incompatible (r2: Table): Boolean =
    {
         if (cols != r2.cols) {
             flaw ("incompatible", s"${this.name} and r2 have differing number of columns")
             true
         } else if (domains != r2.domains) {
             flaw ("incompatible", "${this.name} and r2 have differing domain strings")
             true
         } else {
             false
         } // if
    } // incompatible

    // ================================================================= PRODUCT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cartesian product of this' relation and 'r2' ('this × r2').
     *  @param r2  the second relation
     */
    def product (r2: Table): Relation =
    {
        val ncols     = cols + r2.cols
        val newCName  = disambiguate (colName, r2.colNames)
        val newCol    = Vector.fill [Vec] (ncols) (null)
        val newKey    = key                                        // FIX
        val newDomain = domain + r2.domains
        val r3 = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)

        for (i <- 0 until rows) {
            val t = row(i)
            for (j <- 0 until r2.rows) r3.add (t ++ r2.row(j))
        } // for
        r3.materialize ()
    } // product

    // ==================================================================== JOIN

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing an "equi-join".  Rows from both
     *  relations are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  FIX - only allows single attribute in join condition
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def join (cName1: Seq [String], cName2: Seq [String], r2: Table): Relation =
    {
        val ncols = cols + r2.cols
        val cp1   = cName1.map (colMap (_))                        // get column positions in 'this'
        val cp2   = cName2.map (r2.colsMap (_))                    // get column positions in 'r2'
        if (cp1.length != cp2.length) flaw ("join", "incompatible sizes on match columns")

        val newCName  = disambiguate (colName, r2.colNames)
        val newCol    = Vector.fill [Vec] (ncols) (null)
        val newKey    = key                                        // FIX
        val newDomain = domain + r2.domains
        val r3 = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)

        for (i <- 0 until rows) {
            val t = row(i)
            for (j <- 0 until r2.rows) {
                val u = r2.row(j)
                if (sameOn (t, u, cp1, cp2)) r3.add (t ++ u)
            } // for
        } // for
        r3.materialize ()
    } // join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing an "equi-join", use index to join
     *  FIX - only allows single attribute in join condition
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param _r2     the rhs relation in the join operation
     */
    def joinindex (cName1: Seq [String], cName2: Seq [String], _r2: Table): Relation =
    {
        val r2 = _r2.asInstanceOf [Relation]
        val ncols = cols + r2.cols
        val cp1   = cName1.map (colMap (_))                        // get column positions in 'this'
        val cp2   = cName2.map (r2.colMap (_))                     // get column positions in 'r2'
        if (cp1.length != cp2.length) flaw ("join", "incompatible sizes on match columns")

        val newCName = disambiguate (colName, r2.colName)
        val newCol   = Vector.fill [Vec] (ncols)(null)
        val newKey   = if (r2.key == cp2(0))   key                 // foreign key in this relation
                       else if (key == cp1(0)) r2.key              // foreign key in r2 table
                       else -1                                     // key not in join and composite keys not allowed

        val newDomain = domain + r2.domains
        val r3 = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)

        if (cp1.size == 1 && cp2.size == 1) {
            if (key == cp1(0) && r2.key == cp2(0)) {
                for (k <- orderedIndex) {
                    val t = index(k)
                    val u = r2.index.getOrElse (k, null)
                    if (u != null) r3.add_ni (t ++ u)
                } // for
            } else if (key == cp1(0)) {
                for (idx <- r2.orderedIndex) {
                    val u = r2.index(idx)
                    val t = index.getOrElse (new KeyType (u(cp2(0))), null)
                    if (t != null) r3.add_ni (t ++ u)
                    r3.add_ni(t ++ u)
                } // for
            } else if (r2.key == cp2(0)) {
                for (idx <- orderedIndex) {
                    val t = index(idx)
                    val u = r2.index.getOrElse (new KeyType (t(cp1(0))), null)
                    if (u != null) r3.add_ni (t ++ u)
                } // for
            } // if
        } // if
        r3.materialize ()
    } // joinindex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing a "natural-join".  Rows from both
     *  relations are compared requiring 'cName' values to be equal.
     *  @param cName  the common join column names for both relation
     *  @param _r2    the rhs relation in the join operation
     */
    def join (cName: Seq [String], _r2: Table): Relation =
    {
       val r2 = _r2.asInstanceOf [Relation]
        val ncols = cols + r2.cols - cName.length
        val cp1   = cName.map (colMap (_))                         // get column positions in 'this'
        val cp2   = cName.map (r2.colMap (_))                      // get column positions in 'r2'
        var newDomain2 = r2.domain
        for (i <- 0 until cp1.length) {
            if (domain(cp1(i)) != r2.domain(cp2(i))) flaw ("join", "column types do not match")
            newDomain2 = removeAt (newDomain2, cp2(i))
        } // for
        val cp3   = r2.colName.map (r2.colMap (_)) diff cp2        // 'r2' specific columns

        val newCName  = uniq_union (colName, r2.colName)
        val newCol    = Vector.fill [Vec] (ncols) (null)
        val newKey    = key                                        // FIX
        val newDomain = domain + newDomain2
        val r3 = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)

        for (i <- 0 until rows) {
            val t = row(i)
            for (j <- 0 until r2.rows) {
                val u = r2.row(j)
                if (sameOn (t, u, cp1, cp2)) { val u3 = TableObj.project (u, cp3); r3.add (t ++ u3) }
            } // for
        } // for
        r3.materialize ()
    } // join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The theta join, handle the predicates in where are connect by "and" (where a....and b....).
     *  @param _r2  the second relation
     *  @param p0   the first theta join predicate (r1 cName, r2 cName, predicate to compare these two column)
     *  @param p    the rest of theta join predicates (r1 cName, r2 cName, predicates to compare these two column)
     */
    def join [T] (_r2: Table, p0: Predicate2 [T], p: Predicate2 [T]*): Relation =
    {
        val r2        = _r2.asInstanceOf [Relation]
        val ncols     = cols + r2.cols
        val newCName  = disambiguate (colName, r2.colName)
        val newCol    = Vector.fill [Vec] (ncols) (null)
        val newKey    = key                                        // FIX
        val newDomain = domain + r2.domain
        val r3        = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain, null)

        var resultlist = Seq [(Int, Int)] ()
        for (i <- 0 to p.size) {
            var result = Seq [(Int, Int)] ()
            val p_i = if (i == 0) p0 else p(i-1)
            val cp1 = colMap (p_i._1)
            val cp2 = r2.colMap (p_i._2)
            if (domain.charAt (cp1) != r2.domain.charAt (cp2))  flaw ("join", "differing domain strings")
            val psingle = p_i._3                                    // single predicate

            domain (colMap(p_i._1)) match {
            case 'C' => result = col(cp1).asInstanceOf [VectorC].filterPos2 (r2.col (cp2).asInstanceOf [VectorC],
                                                       psingle.asInstanceOf [(Complex, Complex) => Boolean])
            case 'D' => result = col(cp1).asInstanceOf [VectorD].filterPos2 (r2.col (cp2).asInstanceOf [VectorD],
                                                       psingle.asInstanceOf [(Double, Double) => Boolean])
            case 'I' => result = col(cp1).asInstanceOf [VectorI].filterPos2 (r2.col (cp2).asInstanceOf [VectoI],
                                                       psingle.asInstanceOf [(Int, Int) => Boolean])
            case 'L' => result = col(cp1).asInstanceOf [VectorL].filterPos2 (r2.col (cp2).asInstanceOf [VectorL],
                                                       psingle.asInstanceOf [(Long, Long) => Boolean])
            case 'Q' => result = col(cp1).asInstanceOf [VectorQ].filterPos2 (r2.col (cp2).asInstanceOf [VectorQ],
                                                       psingle.asInstanceOf [(Rational, Rational) => Boolean])
            case 'R' => result = col(cp1).asInstanceOf [VectorR].filterPos2 (r2.col (cp2).asInstanceOf [VectorR],
                                                       psingle.asInstanceOf [(Real, Real) => Boolean])
            case 'S' => result = col(cp1).asInstanceOf [VectorS].filterPos2 (r2.col (cp2).asInstanceOf [VectorS],
                                                       psingle.asInstanceOf [(StrNum, StrNum) => Boolean])
            case 'T' => result = col(cp1).asInstanceOf [VectorT].filterPos2 (r2.col (cp2).asInstanceOf [VectorT],
                                                       psingle.asInstanceOf [(TimeNum, TimeNum) => Boolean])

            case 'c' => result = col(cp1).asInstanceOf [RleVectorC].filterPos2 (r2.col (cp2).asInstanceOf [RleVectorC],
                                                       psingle.asInstanceOf [(Complex, Complex) => Boolean])
            case 'd' => result = col(cp1).asInstanceOf [RleVectorD].filterPos2 (r2.col (cp2).asInstanceOf [RleVectorD],
                                                       psingle.asInstanceOf [(Double, Double) => Boolean])
            case 'i' => result = col(cp1).asInstanceOf [RleVectorI].filterPos2 (r2.col (cp2).asInstanceOf [RleVectorI],
                                                       psingle.asInstanceOf [(Int, Int) => Boolean])
            case 'l' => result = col(cp1).asInstanceOf [RleVectorL].filterPos2 (r2.col (cp2).asInstanceOf [RleVectorL],
                                                       psingle.asInstanceOf [(Long, Long) => Boolean])
            case 'q' => result = col(cp1).asInstanceOf [RleVectorQ].filterPos2 (r2.col (cp2).asInstanceOf [RleVectorQ],
                                                       psingle.asInstanceOf [(Rational, Rational) => Boolean])
            case 'r' => result = col(cp1).asInstanceOf [RleVectorR].filterPos2 (r2.col (cp2).asInstanceOf [RleVectorR],
                                                       psingle.asInstanceOf [(Real, Real) => Boolean])
            case 's' => result = col(cp1).asInstanceOf [RleVectorS].filterPos2 (r2.col (cp2).asInstanceOf [RleVectorS],
                                                       psingle.asInstanceOf [(StrNum, StrNum) => Boolean])
            case 't' => result = col(cp1).asInstanceOf [RleVectorT].filterPos2 (r2.col (cp2).asInstanceOf [RleVectorT],
                                                       psingle.asInstanceOf [(TimeNum, TimeNum) => Boolean])

            case 'χ' => result = col(cp1).asInstanceOf [SparseVectorC].filterPos2 (r2.col (cp2).asInstanceOf [SparseVectorC],
                                                       psingle.asInstanceOf [(Complex, Complex) => Boolean])
            case 'δ' => result = col(cp1).asInstanceOf [SparseVectorD].filterPos2 (r2.col (cp2).asInstanceOf [SparseVectorD],
                                                       psingle.asInstanceOf [(Double, Double) => Boolean])
            case 'ι' => result = col(cp1).asInstanceOf [SparseVectorI].filterPos2 (r2.col (cp2).asInstanceOf [SparseVectorI],
                                                       psingle.asInstanceOf [(Int, Int) => Boolean])
            case 'λ' => result = col(cp1).asInstanceOf [SparseVectorL].filterPos2 (r2.col (cp2).asInstanceOf [SparseVectorL],
                                                       psingle.asInstanceOf [(Long, Long) => Boolean])
            case 'ϟ' => result = col(cp1).asInstanceOf [SparseVectorQ].filterPos2 (r2.col (cp2).asInstanceOf [SparseVectorQ],
                                                       psingle.asInstanceOf [(Rational, Rational) => Boolean])
            case 'ρ' => result = col(cp1).asInstanceOf [SparseVectorR].filterPos2 (r2.col (cp2).asInstanceOf [SparseVectorR],
                                                       psingle.asInstanceOf [(Real, Real) => Boolean])
            case 'σ' => result = col(cp1).asInstanceOf [SparseVectorS].filterPos2 (r2.col (cp2).asInstanceOf [SparseVectorS],
                                                       psingle.asInstanceOf [(StrNum, StrNum) => Boolean])
            case 'τ' => result = col(cp1).asInstanceOf [SparseVectorT].filterPos2 (r2.col (cp2).asInstanceOf [SparseVectorT],
                                                       psingle.asInstanceOf [(TimeNum, TimeNum) => Boolean])

            case _ => flaw ("join", "domain string is missing"); null
            } // match

            if (DEBUG) println (s"join: after predicate $i: result = $result")
            resultlist = if (i == 0) result else resultlist intersect result
        } // for

        val smallmapbig = resultlist.groupBy (_._1)
        for (i <- smallmapbig.keySet.toVector.sorted) {
            val t = if (key < 0) index(KeyType(i)) else index(indextoKey(i))
            val bigindexs = smallmapbig (i).map (x => x._2)
            for (j <- bigindexs) {
                val u = if (r2.key < 0) r2.index(KeyType (j)) else r2.index(r2.indextoKey(j))
                r3.add (t ++ u)
            } // for
        } // for
        r3.materialize ()
    } // join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing an "left-join".  Rows from both
     *  relations are compared requiring 'cName1' values to equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def leftJoin (cName1: String, cName2: String, r2: Table): Relation =
    {
        leftJoin (colMap (cName1), colMap (cName2), r2.asInstanceOf [Relation])
    } // leftJoin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing a "left join".  Rows from both
     *  relations are compared requiring 'cp1' values to equal 'cp2' values.
     *  This method returns all the rows from 'this' relation, and the matched rows
     *  from relation 'r2'.  It adds a 'null' tuples for the unmatched rows of relation 'r2'
     *  FIX: It requires relations 'this' and 'r2' to be sorted on column 'cp1' and 'cp2' resp., as it uses Sort-Merge join
     *  @param cp1  the position of the join column of this relation
     *  @param cp2  the position of the join column of 'r2' relation
     *  @param r2   the rhs relation in the join operation
     */
    def leftJoin (cp1: Int, cp2: Int, r2: Relation): Relation =
    {
        val r3 = Relation (name + "_leftJoin_" + r2.name, key, domain + r2.domain,(colName ++ r2.colName):_*)
        val absentTuple = nullTuple (r2.domain)
        var j = 0
        for (i <- 0 until rows) {
            val t = row(i)
            val t_cp1 = t(cp1)
            while (j < r2.rows-1 && <(Vec (r2.col(cp2), j), t_cp1)) j += 1
            val j_aux = j
            if (t_cp1 == r2.row(j)(cp2)) {
                while (j < r2.rows && Vec (r2.col(cp2), j) == t_cp1) {
                    val u = r2.row(j)
                    r3.add_ni (t ++ u)
                    j += 1
                } // while
                j = j_aux
            } else r3.add_ni (t ++ absentTuple)
        } // for
        r3.materialize ()
    } // leftJoin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing a "right join".  Rows from both
     *  relations are compared requiring 'cp1' values to equal 'cp2' values.
     *  This method returns all the rows from 'this' relation, and the matched rows
     *  from relation 'r2'.  It adds a 'null' tuples for the unmatched rows of relation 'r2'
     *  @param cp1  the position of the join column of this relation
     *  @param cp2  the position of the join column of 'r2' relation
     *  @param r2   the rhs relation in the join operation
     */
    def rightJoin (cp1: Int, cp2: Int, r2: Relation): Relation =
    {
        r2.leftJoin (cp2, cp1, this)
    } // rightJoin


   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing an "apprimate left-join".  Rows from both
     *  relations are compared requiring 'cName1' values to apprximately equal 'cName2' values.
     *  Disambiguate column names by appending "2" to the end of any duplicate column name.
     *  All rows from the left table are maintained with missing values indicators used
     *  where needed.
     *  @param thres   the approximate equality threshold
     *  @param cName1  the join column names of this relation (e.g., the Foreign Key)
     *  @param cName2  the join column names of relation r2 (e.g., the Primary Key)
     *  @param r2      the rhs relation in the join operation
     */
    def leftJoin (thres: Double = 0.001) (cName1: String, cName2: String, r2: Table): Relation =
    {
        setThreshold (thres)
        leftJoinApx (colMap (cName1), colMap (cName2), r2.asInstanceOf [Relation])
    } // leftJoin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing a "left join".  Rows from both
     *  relations are compared requiring 'cp1' values to approximately equal 'cp2' values.
     *  This method returns all the rows from 'this' relation, and the matched rows
     *  from relation 'r2'.  It adds a 'null' tuples for the unmatched rows of relation 'r2'
     *  FIX: It requires relations 'this' and 'r2' to be sorted on column 'cp1' and 'cp2' resp.,
     *  as it uses Sort-Merge join
     *  @param cp1  the position of the join column of this relation
     *  @param cp2  the position of the join column of 'r2' relation
     *  @param r2   the rhs relation in the join operation
     */
    def leftJoinApx (cp1: Int, cp2: Int, r2: Relation): Relation =
    {
        val r3 = Relation (name + "_leftJoinApx_" + r2.name, 1, domain + r2.domain, (colName ++ r2.colName):_*)
        val absentTuple = nullTuple (r2.domain)
        var j = 0

        for (i <- 0 until rows) {
            val t = row(i)
            val t_cp1 = t(cp1)
            while (j < r2.rows-1 && !=~ (Vec (r2.col(cp2), j), t_cp1) && < (Vec (r2.col(cp2), j), t_cp1)) j += 1
            val j_aux = j
            if (=~ (t_cp1, r2.row(j)(cp2))) {
                while (j < r2.rows && =~ (Vec (r2.col(cp2), j), t_cp1)) {
                    val u = r2.row(j)
                    r3.add_ni (t ++ u)
                    j += 1
                } // while
                j = j_aux
            } else r3.add_ni (t ++ absentTuple)
        } // for
        r3.materialize ()
    } // leftJoinApx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join 'this' relation and 'r2' by performing a "right join".  Rows from both
     *  relations are compared requiring 'cp1' values to approximately equal 'cp2' values.
     *  This method returns all the rows from 'this' relation, and the matched rows
     *  from relation 'r2'.  It adds a 'null' tuples for the unmatched rows of relation 'r2'
     *  @param cp1  the position of the join column of this relation
     *  @param cp2  the position of the join column of 'r2' relation
     *  @param r2   the rhs relation in the join operation
     */
    def rightJoinApx (cp1: Int, cp2: Int, r2: Relation): Relation =
    {
        r2.leftJoinApx (cp2, cp1, this)
    } // rightJoinApx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Parallel Join 'this' relation and 'r2' by performing an equi join on cName1 = cName2 and into k -threads
     *  seperate the lhs into k part join with rhs.
     *  FIX - move to .par package
     *  @param cName1  the join column names of lhs relation
     *  @param cName2  the join column names of rhs relation
     *  @param _r2     the rhs relation in the join operation
     *  @param k       parallel run into k parts
     */
    def parjoin (cName1: Seq [String], cName2: Seq [String], _r2: Relation, k: Int): Relation =
    {
        // make the join into k parts of outer table join with inner table
        // outer table is the one without index, partition on the outer table, inner table use index to loop through

        val r2  = _r2.asInstanceOf [Relation]
        val cp1 = cName1.map (colMap (_))                            // get column positions in 'this'
        val cp2 = cName2.map (r2.colMap (_))                         // get column positions in 'r2'
        var futurelist: IndexedSeq [Future [Relation]] = null

        if (key == cp1(0)) futurelist =
            // use left table as inner table (partition on outer table)
            for (i <- 1 to k) yield Future { r2.parjoinsmall (cName1, cName2, this, i, k) }
        else if (r2.key == cp2(0)) futurelist =
            for (i <- 1 to k) yield Future { parjoinsmall (cName1, cName2, r2, i, k) }

        val waitduration = 190.millisecond                           // Need to be FIXED, should be partial to (rows /k)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        def recur (r1: Relation, kth: Int, relationList: ArrayBuffer [Relation]): Relation =
        {
            if (kth == relationList.size - 1) r1 union relationList(kth)
            else if (r1 != null) recur (r1 union relationList(kth), kth + 1, relationList)
            else                 recur (relationList(kth), kth + 1, relationList)
        } // recur

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        // union the tables together 2 as a group from result of the futurelist
        def foldhalf (fl: IndexedSeq [Future [Relation]]): Relation =
        {
            val relationList = ArrayBuffer [Relation] ()
            for (i <- 0 until fl.size) relationList += Await.result(fl(i), waitduration)
            recur (relationList(0), 1, relationList)
        } // foldhalf

        foldhalf (futurelist)
    } // parjoin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'parjoinsmall' method serves as the core part for parjoin function, do the
     *  nth part of the parellel join returning  the relation of join of two tables.
     *  FIX - move to .par package
     *  @param cName1  the join column names of lhs relation
     *  @param cName2  the join column names of rhs relation
     *  @param _r2     the rhs relation in the join operation
     *  @param nth     the nth part of the parallel join
     *  @param n       parallel run into k parts
     */
    private def parjoinsmall (cName1: Seq [String], cName2: Seq [String], _r2: Table, nth: Int, n: Int): Relation =
    {
        val r2    = _r2.asInstanceOf [Relation]
        val ncols = cols + r2.cols
        val cp1   = cName1.map (colMap (_))                        // get column positions in 'this'
        val cp2   = cName2.map (r2.colMap (_))                     // get column positions in 'r2'
        if (cp1.length != cp2.length) flaw ("join", "incompatible sizes on match columns")

        val newCName  = disambiguate (colName, r2.colName)
        val newCol    = Vector.fill [Vec] (ncols) (null)
        val newKey    = key                                        // FIX
        val newDomain = domain + r2.domain
        val r3        = new Relation (name + "_j_" + ucount (), newCName, newCol, newKey, newDomain)
        val start     = rows/n * (nth-1)
        val end       = if (nth == n) rows-1 else rows/n*nth - 1

        if (cp1.size == 1 && cp2.size == 1) {
            if (key == cp1(0) && r2.key == cp2(0)) {
                for (k <- orderedIndex.slice (start, end + 1)) {
                    val t = index(k)
                    val u = r2.index.getOrElse(k, null)
                    if (u != null) r3.add_ni (t ++ u)
                } //for
            } // if
            // partition the left table
            for (i <- start until end+1) {
                val t = row(i)
                val u = r2.index.getOrElse(new KeyType (t(cp1(0))), null)
                if (u != null) r3.add_ni (t ++ u)
                } // for
        } // if
        r3.materialize ()
    } // parjoinsmall

    // ================================================================ GROUP BY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Group 'this' relation by the specified column names, returning 'this' relation.
     *  @param cName  the group column names
     */
    def groupBy (cName: String*): Relation =
    {
        if (! cName.map (c => colName contains(c)).reduceLeft (_ && _))
            flaw ("groupBy", "groupbyName used to groupby doesn't exist in the cName")
        val equivCol = Vector.fill [Vec] (colName.length)(null)
        if (rows == 0) return this

        val cPos = cName.map (colMap (_))

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Sort on the given columns.
         *  @param sortColumn  the set of columns to sort on
         */
        def sortcol (sortColumn: Set [Any]): Vec =
        {
            var colcol: Vec = null
            val domain = null
//          for (x <- sortColumn) colcol = Vec.:+ (colcol, x, domain, 0)
            for (x <- sortColumn) colcol = Vec.:+ (colcol, x)

            colcol match {
            case _: VectoC => val sortcol = colcol.asInstanceOf [VectoC]; sortcol.sort (); sortcol
            case _: VectoD => val sortcol = colcol.asInstanceOf [VectoD]; sortcol.sort (); sortcol
            case _: VectoI => val sortcol = colcol.asInstanceOf [VectoI]; sortcol.sort (); sortcol
            case _: VectoL => val sortcol = colcol.asInstanceOf [VectoL]; sortcol.sort (); sortcol
            case _: VectoQ => val sortcol = colcol.asInstanceOf [VectoQ]; sortcol.sort (); sortcol
            case _: VectoR => val sortcol = colcol.asInstanceOf [VectoR]; sortcol.sort (); sortcol
            case _: VectoS => val sortcol = colcol.asInstanceOf [VectoS]; sortcol.sort (); sortcol
            case _: VectoT => val sortcol = colcol.asInstanceOf [VectoT]; sortcol.sort (); sortcol
            case _ => flaw ("sortcol", s"vector type ${colcol.getClass} not supported"); null.asInstanceOf [Vec]
            } // match
        } // sortcol

        var groupIndexMap = Map [Any, Vector [KeyType]] ()
        val tempIndexMap  = Map [Any, Vector [KeyType]] ()
        var sortlst: Vec  = null

        for (i <- cPos.indices) {
            if (i == 0) {
                index.foreach (indexmap => {
                    val key   = StrNum (indexmap._2(cPos(i)).toString)
                    val value = indexmap._1
                    if (groupIndexMap contains key) groupIndexMap += key -> (groupIndexMap(key) :+ value)
                    else groupIndexMap += key -> Vector(value)
                }) // foreach
            } else {
                tempIndexMap.clear ()
                groupIndexMap.foreach (groupindexmap => {
                    val tempidxlist = groupindexmap._2
                    for (idx <- tempidxlist) {
                        val key   = StrNum(groupindexmap._1.toString + "," + index(idx)(cPos(i)))
                        val value = idx
                        if (tempIndexMap.contains(key)) tempIndexMap += key -> (tempIndexMap(key) :+ value)
                        else tempIndexMap += key -> Vector(value)
                    } // for
                }) // for each
                groupIndexMap = tempIndexMap
            } // if

            if (i == cPos.size - 1) {
                orderedIndex = Vector ()
                grouplist    = Vector [Int] ()
                sortlst      = sortcol(groupIndexMap.keySet.toSet)
                for (k <- 0 until sortlst.size) {
                    val indexes  = groupIndexMap(Vec(sortlst, k))
                    orderedIndex = orderedIndex ++ indexes
                    grouplist    = grouplist :+ orderedIndex.length
                } // for
            } // if
        } // for
        this
    } // groupby

    // ================================================================= ORDER BY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (ascending) the rows in the relation by the selected columns '_cName'.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param _cName  the column names that are to be sorted
     */
    def orderBy (_cName: String*): Relation =
    {
        val cName = _cName.distinct
        if (! cName.map (c => colName contains (c)).reduceLeft (_ && _))
            flaw ("orderBy", "cName used to orderBy does not exist in relation")

        val newCol = Vector.fill [Vec] (cols)(null)
        val r2 = new Relation (name + "_j_" + ucount (), colName, newCol, key, domain)

        val perm = orderByHelper (cName.map (colMap (_)), rows)
        for (i <- perm) r2.add (row(i))
        r2.materialize ()
    } // orderBy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order (descending) the rows in the relation by the selected columns '_cName'.
     *  A stable sorting is used to allow sorting on multiple columns.
     *  @param _cName  the column names that are to be sorted
     */
    def reverseOrderBy (_cName: String*): Relation =
    {
        val cName = _cName.distinct
        if (! cName.map (c => colName contains (c)).reduceLeft (_ && _))
            flaw ("orderBy", "cName used to orderBy does not exist in relation")

        val newCol = Vector.fill [Vec] (cols) (null)
        val r2 = new Relation (name + "_j_" + ucount (), colName, newCol, key, domain)

        val perm = orderByHelper (cName.map (colMap (_)), rows)
        for (i <- perm.reverse) r2.add (row(i))
        r2.materialize ()
    } // reverseOrderBy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Helper method for 'orderBy' and 'reverseOrderBy'.  Performs indirect merge sort.
     *  @param cPos  sequence of column positions to sort
     *  @param n     total number of rows in this Relation
     */
    private def orderByHelper (cPos: Seq [Int], n: Int = rows): Array [Int] =
    {
        var perm: Array [Int] = null

        for (i <- cPos.indices) {
            val col_i: Array [Any] = col (cPos(i)) match {
                case _: VectorC => col(cPos(i)).asInstanceOf [VectorC]().toArray
                case _: VectorD => col(cPos(i)).asInstanceOf [VectorD]().toArray
                case _: VectorI => col(cPos(i)).asInstanceOf [VectorI]().toArray
                case _: VectorL => col(cPos(i)).asInstanceOf [VectorL]().toArray
                case _: VectorQ => col(cPos(i)).asInstanceOf [VectorQ]().toArray
                case _: VectorR => col(cPos(i)).asInstanceOf [VectorR]().toArray
                case _: VectorS => col(cPos(i)).asInstanceOf [VectorS]().toArray
                case _: VectorT => col(cPos(i)).asInstanceOf [VectorT]().toArray
            } // match

            perm = if (i == 0) (new MergeSortIndirect (col_i)()).isort ()
                   else        (new MergeSortIndirect (col_i)(perm)).isort ()
        }// for
        perm
    } // orderByHelper

    // ================================================================ COMPRESS 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compress the selected columns 'cName' in 'this' table.
     *  @param cName  the names of the columns to be compressed
     */
    def compress (cName: String*)
    {
        for (c <- cName) {
            val i = colMap (c)
//          col(i).compress ()      // FIX - add compress to Vec
        } // for
    } // compress

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Uncompress the selected columns 'cName' in 'this' table.
     *  @param cName  the names of the columns to be uncompressed
     */
    def uncompress (cName: String*)
    {
        for (c <- cName) {
            val i = colMap (c)
//          col(i).uncompress ()      // FIX - add uncompress to Vec
        } // for
    } // uncompress

    // ================================================================= UPDATES 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'tuple' to 'this' relation as a new row.
     *  FIX:  want an efficient, covariant, mutable data structure, but `Array` is invariant.
     *  @param tuple  an aggregation of columns values (new row)
     *
    def add (tuple: Row)
    {
        col = (for (j <- tuple.indices) yield
            try {
                Vec.:+ (col(j), tuple(j))
            } catch {
                case cce: ClassCastException =>
                    println (s"add: for column $j of tuple $tuple"); throw cce
            } // try
          ).toVector
    } // add
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'tuple' to 'this' relation as a new row.  It uses 'col2' as a temp 'col'
     *  to improve performance.
     *  @param tuple  an aggregation of columns values (new row)
     */
    @throws (classOf [Exception])
    def add (tuple: Row)
    {
        try {
            if (tuple == null) throw new Exception ("add function: tuple is null")
            val rowIdx   = col2(0).length
            val newkey   = if (key < 0) new KeyType (rowIdx) else new KeyType (tuple(key))
            index       += newkey -> tuple
            keytoIndex  += newkey -> rowIdx
            orderedIndex = orderedIndex :+ newkey
            indextoKey  += rowIdx -> newkey
            for (j <- tuple.indices) addElem (j, rowIdx, tuple(j))
        } catch {
            case ex: NullPointerException =>
                println ("tuple'size is: " + tuple.size)
                println ("col'size is:   " + col.size)
                throw ex
        } // try
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add an element into 'col2', the holding area for input.  If the types
     *  of column domains are specified, the types are checked.
     *  @param j       the j-th column of col2
     *  @param rowIdx  the row index
     *  @param elem    the element to added
     */
    private def addElem (j: Int, rowIdx: Int, elem: Any)
    {
        val typ = if (domain == null) 'X' else domain(j)
        try {
            if (typ == 'X') col2(j).asInstanceOf [ReArray [Any]](rowIdx) = elem      // no domains => assume Any type
            else { typ match {
                case 'C' => col2(j).asInstanceOf [ReArray [Complex]](rowIdx)  = elem.asInstanceOf [Complex]
                case 'D' => col2(j).asInstanceOf [ReArray [Double]](rowIdx)   = elem.asInstanceOf [Double]
                case 'I' => col2(j).asInstanceOf [ReArray [Int]](rowIdx)      = elem.asInstanceOf [Int]
                case 'L' => col2(j).asInstanceOf [ReArray [Long]](rowIdx)     = elem.asInstanceOf [Long]
                case 'Q' => col2(j).asInstanceOf [ReArray [Rational]](rowIdx) = elem.asInstanceOf [Rational]
                case 'R' => col2(j).asInstanceOf [ReArray [Real]](rowIdx)     = elem.asInstanceOf [Real]
                case 'S' => col2(j).asInstanceOf [ReArray [StrNum]](rowIdx)   = elem.asInstanceOf [StrNum]
                case 'T' => col2(j).asInstanceOf [ReArray [TimeNum]](rowIdx)  = elem.asInstanceOf [TimeNum]
                case _   => flaw ("constructor", s"unsupported column type ${domain(j)} for column $j")
                } // match
            } // if
        } catch {
            case ex: ClassCastException =>
                if (typ == 'S') {
                    println (s"warning in addElem: colIdx j = $j, rowIdx = $rowIdx, elem = $elem, class = ${elem.getClass}, typ = $typ")
                    col2(j).asInstanceOf [ReArray [StrNum]](rowIdx) = StrNum (elem.toString)                 // anything can be a string
                } else if (elem.isInstanceOf [String] || elem.isInstanceOf [Char]) {
                    println (s"warning in addElem: colIdx j = $j, rowIdx = $rowIdx, elem = $elem, class = ${elem.getClass}, typ = $typ")
                    typ match {
                    case 'C' => col2(j).asInstanceOf [ReArray [Complex]](rowIdx)  = noComplex
                    case 'D' => col2(j).asInstanceOf [ReArray [Double]](rowIdx)   = noDouble
                    case 'I' => col2(j).asInstanceOf [ReArray [Int]](rowIdx)      = noInt
                    case 'L' => col2(j).asInstanceOf [ReArray [Long]](rowIdx)     = noLong
                    case 'Q' => col2(j).asInstanceOf [ReArray [Rational]](rowIdx) = noRational
                    case 'R' => col2(j).asInstanceOf [ReArray [Real]](rowIdx)     = noReal
                    case 'T' => col2(j).asInstanceOf [ReArray [TimeNum]](rowIdx)  = noTimeNum
                    case _   => flaw ("constructor", s"unsupported column type ${domain(j)} for column $j")
                    } // match
                } else {
                    println (s"exception in addElem: colIdx j = $j, rowIdx = $rowIdx, elem = $elem, class = ${elem.getClass}, typ = $typ")
                    throw ex
                } // if
        } // try
        
    } // addElem

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'tuple' to 'this' relation as a new row.  It is slower than 'add' method.
     *  Type is determined by sampling values for columns.
     *  @param tuple  an aggregation of columns values (new row)
     *
    def add_2 (tuple: Row)
    {
        index      += new KeyType (tuple(key))-> tuple  // hashmap way
        keytoIndex += new KeyType (tuple(key)) ->rows
        col = (for (j <- tuple.indices) yield
                   try {
//                     Vec.:+ (col(j), StrNum (tuple(j).toString), domain, j)             // FIX - allow this option
                       Vec.:+ (col(j), StrNum (tuple(j).toString))
                   } catch {
                       case cce: ClassCastException =>
                           println (s"add: for column $j of tuple $tuple"); throw cce
                   } // try
              ).toVector
    } // add_2
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a tuple into the 'col2', without maintaining the index (No Index (ni),
     *  orderedIndex, keytoIndex and indextoKey.
     *  @param tuple  the tuple to add
     */
    private def add_ni (tuple: Row)
    {
        val rowIdx = col2(0).length
        for (j <- tuple.indices) addElem (j, rowIdx, tuple(j))
    } // add_ni

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Materialize the relation by copying the temporary 'col2' into 'col'.
     *  It needs to be called by the end of the relation construction.
     */
    private [columnar_db] def materialize (): Relation =
    {
        if (domain == null || domain == "") materialize1 () else materialize2 ()
    } // materialize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Materialize the relation by copying the temporary 'col2' into 'col'.
     *  It needs to be called by the end of the relation construction.
     *  This version uses the type/domain of the first value to transform the 'col2' to 'col'.
     */
    private [columnar_db] def materialize1 (): Relation =
    {
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Transform the j-th column to the appropriate vector type.
         *  @param j  the j-th column index in the relation
         */
        def transform1 (j: Int): Vec =
        {
            val first = col2(j)(0)
            if (first != null) col2(j).reduceToSize (col2(j).size)
            first match {
            case _: Complex  => val rs = VectorC (col2(j).asInstanceOf [Seq [Complex]]);  col2(j).clear (); rs
            case _: Double   => val rs = VectorD (col2(j).asInstanceOf [Seq [Double]]);   col2(j).clear (); rs
            case _: Int      => val rs = VectorI (col2(j).asInstanceOf [Seq [Int]]);      col2(j).clear (); rs
            case _: Long     => val rs = VectorL (col2(j).asInstanceOf [Seq [Long]]);     col2(j).clear (); rs
            case _: Rational => val rs = VectorQ (col2(j).asInstanceOf [Seq [Rational]]); col2(j).clear (); rs
            case _: Real     => val rs = VectorR (col2(j).asInstanceOf [Seq [Real]]);     col2(j).clear (); rs
            case _: StrNum   => val rs = VectorS (col2(j).asInstanceOf [Seq [StrNum]]);   col2(j).clear (); rs
            case _: String   => val rs = VectorS (col2(j).asInstanceOf [Seq [String]].toArray); col2(j).clear (); rs
            case _: TimeNum  => val rs = VectorT (col2(j).asInstanceOf [Seq [TimeNum]]);  col2(j).clear (); rs
            case _           => flaw ("materialize1.transform", s"($j): vector type ($first) not supported"); null
            } // match
        } // transform1

//      if (DEBUG) println (s"materialize1: col2 = $col2")
        col = (for (j <- col.indices) yield transform1(j)).toVector
        this
    } // materialize1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Materialize the relation by copying the temporary 'col2' into 'col'.
     *  It needs to be called by the end of the relation construction.
     *  This version uses 'domain' to transform the 'col2' to 'col' according to the domain indicator:
     *  <p>
     *      Dense:      'C', 'D', 'I', 'L'. 'Q', 'R', 'S', 'T'
     *      Compressed: 'c', 'd', 'i', 'l', 'q', 'r', 's', 't'
     *      Sparse:     'χ', 'δ', 'ι', 'λ', 'ϟ', 'ρ', 'σ', 'τ'
     *  <p>
     */
    private [columnar_db] def materialize2 (): Relation =
    {
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Transform the j-th column to the appropriate vector type.
         *  @param j  the j-th column index in the relation
         */
        def transform2 (j: Int): Vec =
        {
            val dj = domain(j)
            col2(j).reduceToSize (col2(j).size)
            dj match {

            // Upper case letter type/domain indictors for Dense Vectors
            case 'C' => val rs = VectorC (col2(j).asInstanceOf [Seq [Complex]]);  col2(j).clear; rs
            case 'D' => val rs = VectorD (col2(j).asInstanceOf [Seq [Double]]);   col2(j).clear; rs
            case 'I' => val rs = VectorI (col2(j).asInstanceOf [Seq [Int]]);      col2(j).clear; rs
            case 'L' => val rs = VectorL (col2(j).asInstanceOf [Seq [Long]]);     col2(j).clear; rs
            case 'Q' => val rs = VectorQ (col2(j).asInstanceOf [Seq [Rational]]); col2(j).clear; rs
            case 'R' => val rs = VectorR (col2(j).asInstanceOf [Seq [Real]]);     col2(j).clear; rs
            case 'S' => val rs = VectorS (col2(j).asInstanceOf [Seq [StrNum]]);   col2(j).clear; rs
            case 'T' => val rs = VectorT (col2(j).asInstanceOf [Seq [TimeNum]]);  col2(j).clear; rs

            // Lower case letter type/domain indictors for Compressed Vectors
            case 'c' => val rs = RleVectorC (col2(j).asInstanceOf [Seq [Complex]]);  col2(j).clear; rs
            case 'd' => val rs = RleVectorD (col2(j).asInstanceOf [Seq [Double]]);   col2(j).clear; rs
            case 'i' => val rs = RleVectorI (col2(j).asInstanceOf [Seq [Int]]);      col2(j).clear; rs
            case 'l' => val rs = RleVectorL (col2(j).asInstanceOf [Seq [Long]]);     col2(j).clear; rs
            case 'q' => val rs = RleVectorQ (col2(j).asInstanceOf [Seq [Rational]]); col2(j).clear; rs
            case 'r' => val rs = RleVectorR (col2(j).asInstanceOf [Seq [Real]]);     col2(j).clear; rs
            case 's' => val rs = RleVectorS (col2(j).asInstanceOf [Seq [StrNum]]);   col2(j).clear; rs
            case 't' => val rs = RleVectorT (col2(j).asInstanceOf [Seq [TimeNum]]);  col2(j).clear; rs

            // Lower case Greek letter type/domain indictors for Sparse Vectors
            // @see web.mit.edu/jmorzins/www/greek-alphabet.html
            // @see en.wikipedia.org/wiki/List_of_Unicode_characters#Greek_and_Coptic
            case 'χ' => val rs = SparseVectorC (col2(j).asInstanceOf [Seq [Complex]]);  col2(j).clear; rs
            case 'δ' => val rs = SparseVectorD (col2(j).asInstanceOf [Seq [Double]]);   col2(j).clear; rs
            case 'ι' => val rs = SparseVectorI (col2(j).asInstanceOf [Seq [Int]]);      col2(j).clear; rs
            case 'λ' => val rs = SparseVectorL (col2(j).asInstanceOf [Seq [Long]]);     col2(j).clear; rs
            case 'ϟ' => val rs = SparseVectorQ (col2(j).asInstanceOf [Seq [Rational]]); col2(j).clear; rs
            case 'ρ' => val rs = SparseVectorR (col2(j).asInstanceOf [Seq [Real]]);     col2(j).clear; rs
            case 'σ' => val rs = SparseVectorS (col2(j).asInstanceOf [Seq [StrNum]]);   col2(j).clear; rs
            case 'τ' => val rs = SparseVectorT (col2(j).asInstanceOf [Seq [TimeNum]]);  col2(j).clear; rs

            case  _  => flaw ("materialize2.transform", s"($j) vector type not supported domain ($dj)"); null
            } // match 
        } // transform2

//      if (DEBUG) println (s"materialize2: col2 = $col2")
        col = (for (j <- col.indices) yield transform2(j)).toVector
        this
    } // materialize2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named 'cName' using function 'func' for elements with
     *  value 'matchStr'.
     *  @param cName     the name of the column to be updated
     *  @param newVal    the value used to assign updated values
     *  @param matchVal  the value to be matched to elements
     *  @tparam T        type of the column
     */
    def update [T] (cName: String, newVal: T, matchVal: T)
    {
        val colPos = colMap(cName)
        val c = col(colPos)
        for (i <- 0 until c.size if Vec(c, i) == matchVal) Vec(c, i) = newVal
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named 'cName' using function 'func' for elements with
     *  value 'matchStr'.
     *  @param cName     the name of the column to be updated
     *  @param func      the function used to assign updated values
     *  @param matchVal  the value to be matched to elements
     *  @tparam T        type of the column
     */
    def update [T] (cName: String, func: (T) => T, matchVal: T)
    {
        val colPos = colMap (cName)
        val c = col (colPos)
        for (i <- 0 until c.size if Vec(c, i) == matchVal) Vec(c, i) = func (Vec(c, i).asInstanceOf [T])
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the column named 'cName' using function 'func' for elements where
     *  the predicate 'pred' evaluates to true.
     *  @param cName  the name of the column to be updated
     *  @param func   the function used to assign updated values
     *  @param pred   the predicated used to select elements for update
     *  @tparam T     type of the column
     */
    def update [T] (cName: String, func: (T) => T, pred: (T) => Boolean)
    {
        val colPos = colMap (cName)
        val c      = col (colPos)
        var pos    = ArrayBuffer [Int] ()
        for (i <- 0 until c.size) {
            val v_ci = Vec(c, i).asInstanceOf [T]
            if (pred (v_ci)) Vec(c, i) = func (v_ci)
        } // for
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Delete the rows from 'this' relation that satisfy the predicates.
     *  FIX - handle all 24 domain types
     *  @param  p  tuple(1): column name, tuple(2): predicate (T => `Boolean`)
     *  @tparam T  the predicate type
     */
    def delete [T] (p: Predicate [T]*): Relation =
    {
        null
/*
        var pos = ArrayBuffer [Int] ()
        for (i <- p.indices) {
            domain (colMap(p(i)._1)) match {
            case 'D' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorD].filterPos (p(i)._2.asInstanceOf [Double => Boolean])
                        if (i > 0) pos = pos intersect pos1 else pos ++= pos1
            case 'I' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorI].filterPos (p(i)._2.asInstanceOf [Int => Boolean])
                        if (i > 0) pos = pos intersect pos1 else pos ++= pos1
            case 'L' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorL].filterPos (p(i)._2.asInstanceOf [Long => Boolean])
                        if (i > 0) pos = pos intersect pos1 else pos ++= pos1
            case 'S' => val pos1 = col (colMap(p(i)._1)).asInstanceOf [VectorS].filterPos (p(i)._2.asInstanceOf [StrNum => Boolean])
                        if (i > 0) pos = pos intersect pos1 else pos ++= pos1
            case _   => flaw ("delete", "predicate type not supported")
                        null
            } // match
        } // for
        val indices = Set (0 to rows-1 :_*) diff pos.toSet
        for (i <- 0 until cols) Vec.delete (col(i), pos.asInstanceOf [Seq [Int]])
        selectAt (indices.toSeq.sorted)
*/
    } // delete

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a string column by column.
     */
    override def toString: String =
    {
        var sb = new StringBuilder ("Relation(" + name + ", " + key + ",\n" + colName + ",\n")
        for (i <- col.indices) sb.append (col(i) + "\n")
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show 'this' relation row by row.
     *  @param limit  the limit on the number of rows to display
     */
    def show (limit: Int = Int.MaxValue)
    {
        val wid   = 18                                             // column width
        val rep   = wid * colName.length                           // repetition = width * # columns
        val title = s"| Relation name = $name, key-column = $key "

        println (s"|-${"-"*rep}-|")
        println (title + " "*(rep-title.length) + "   |")
        println (s"|-${"-"*rep}-|")
        print ("| "); for (cn <- colName) print (s"%${wid}s".format (cn)); println (" |")
        println (s"|-${"-"*rep}-|")
        for (i <- 0 until MIN (rows, limit)) {
            print ("| ")
            for (cv <- row(i)) {
                if (cv.isInstanceOf [Double]) print (s"%${wid}g".format (cv))
                else                          print (s"%${wid}s".format (cv))
            } // for
            println (" |")
        } // for
        println (s"|-${"-"*rep}-|")
    } // show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show 'this' relation's foreign keys.
     */
    def showFk ()
    {
        val wid    = 18                                            // column width
        val rep    = wid * colName.length                          // repetition = width * # columns
        val title  = s"| Relation name = $name, foreign keys = "
        val fkline = s"| $fKeys "

        println (s"|-${"-"*rep}-|")
        println (title + " "*(rep-title.length) + "   |")
        println (s"|-${"-"*rep}-|")
        println (fkline + " "*(rep-fkline.length) + "   |")
        println (s"|-${"-"*rep}-|")
    } // showFk

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
        val colVec = for (x <- project (colPos).col) yield Vec.toDouble (x)
        kind match {
        case DENSE           => MatrixD (colVec)
        case SPARSE          => SparseMatrixD (colVec)
        case SYM_TRIDIAGONAL => SymTriMatrixD (colVec)
        case BIDIAGONAL      => BidMatrixD (colVec)
        case COMPRESSED      => RleMatrixD (colVec)
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
    def toMatriDD (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): (MatriD, VectorD) =
    {
        val colVec = for (x <- project (colPos).col) yield Vec.toDouble (x)
        kind match {
        case DENSE           => (MatrixD (colVec),       Vec.toDouble (col(colPosV)).toDense.asInstanceOf [VectorD])
        case SPARSE          => (SparseMatrixD (colVec), Vec.toDouble (col(colPosV)).toDense.asInstanceOf [VectorD])
        case SYM_TRIDIAGONAL => (SymTriMatrixD (colVec), Vec.toDouble (col(colPosV)).toDense.asInstanceOf [VectorD])
        case BIDIAGONAL      => (BidMatrixD (colVec),    Vec.toDouble (col(colPosV)).toDense.asInstanceOf [VectorD])
        case COMPRESSED      => (RleMatrixD (colVec),    Vec.toDouble (col(colPosV)).toDense.asInstanceOf [VectorD])
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
    def toMatriDI (colPos: Seq [Int], colPosV: Int, kind: MatrixKind = DENSE): (MatriD, VectorI) =
    {
        val colVec = for (x <- project (colPos).col) yield Vec.toDouble (x)
        kind match {
        case DENSE           => (MatrixD (colVec),       Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
        case SPARSE          => (SparseMatrixD (colVec), Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
        case SYM_TRIDIAGONAL => (SymTriMatrixD (colVec), Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
        case BIDIAGONAL      => (BidMatrixD (colVec),    Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
        case COMPRESSED      => (RleMatrixD (colVec),    Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
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
        val colVec = for (x <- project (colPos).col) yield Vec.toInt (x)
        kind match {
        case DENSE           => MatrixI (colVec)
        case SPARSE          => SparseMatrixI (colVec)
        case SYM_TRIDIAGONAL => SymTriMatrixI (colVec)
        case BIDIAGONAL      => BidMatrixI (colVec)
        case COMPRESSED      => RleMatrixI (colVec)
        } // match
    } // toMatriI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' relation into a matrix of integers.  It will convert
     *  doubles and strings to integers.
     *  <p>
     *       in the regression equation: 'xb = y' create matrix 'xy'
     *  <p>
     *  @param colPos  the column positions to use for the matrix
     *  @param kind    the kind of matrix to create
     */
    def toMatriI2 (colPos: Seq [Int] = null, kind: MatrixKind = DENSE): MatriI =
    {
        import Converter._
        val cp = if (colPos == null) Seq.range(0, cols) else colPos
        val colVec = for (x <- project (cp).col) yield {
            try {
                Vec.toInt (x)
            } catch {
                case num: NumberFormatException => map2Int (x.asInstanceOf [VectorS])._1
            } // trys
        } // for
        kind match {
        case DENSE           => MatrixI (colVec)
        case SPARSE          => SparseMatrixI (colVec)
        case SYM_TRIDIAGONAL => SymTriMatrixI (colVec)
        case BIDIAGONAL      => BidMatrixI (colVec)
        case COMPRESSED      => RleMatrixI (colVec)
        } // match
    } // toMatriI2

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
        val colVec = for (x <- project (colPos).col) yield Vec.toInt (x)
        kind match {
            case DENSE           => (MatrixI (colVec),       Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
            case SPARSE          => (SparseMatrixI (colVec), Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
            case SYM_TRIDIAGONAL => (SymTriMatrixI (colVec), Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
            case BIDIAGONAL      => (BidMatrixI (colVec),    Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
            case COMPRESSED      => (RleMatrixI (colVec),    Vec.toInt (col(colPosV)).toDense.asInstanceOf [VectorI])
        } // match
    } // toMatriII

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colPos' column of 'this' relation into a vector of doubles.
     *  @param colPos  the column position to use for the vector
     */
    def toVectorD (colPos: Int): VectorD = Vec.toDouble (col(colPos)).toDense.asInstanceOf [VectorD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colName' column of 'this' relation into a vector of doubles.
     *  @param colName  the column name to use for the vector
     */
    def toVectorD (colName: String): VectorD = Vec.toDouble (col(colMap(colName))).toDense.asInstanceOf [VectorD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colPos' column of 'this' relation into a vector of integers.
     *  @param colPos  the column position to use for the vector
     */
    def toVectorI (colPos: Int): VectorI = Vec.toInt (col(colPos)).toDense.asInstanceOf [VectorI]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colName' column of 'this' relation into a vector of integers.
     *  @param colName  the column name to use for the vector
     */
    def toVectorI (colName: String): VectorI = Vec.toInt (col(colMap(colName))).toDense.asInstanceOf [VectorI]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colPos' column of 'this' relation into a vector of integers.
     *  @param colPos  the column position to use for the vector
     */
    def toVectorS (colPos: Int): VectorS = col(colPos).asInstanceOf [VectorS]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colName' column of 'this' relation into a vector of integers.
     *  @param colName  the column name to use for the vector
     */
    def toVectorS (colName: String): VectorS = col(colMap(colName)).asInstanceOf [VectorS]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colPos' column of 'this' relation into a vector of doubles.
     *  @param colPos  the column position to use for the vector
     */
    def toRleVectorD (colPos: Int): RleVectorD = Vec.toDouble (col(colPos)).asInstanceOf [RleVectorD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colName' column of 'this' relation into a vector of doubles.
     *  @param colName  the column name to use for the vector
     */
    def toRleVectorD (colName: String): RleVectorD = Vec.toDouble (col(colMap(colName))).asInstanceOf [RleVectorD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colPos' column of 'this' relation into a vector of integers.
     *  @param colPos  the column position to use for the vector
     */
    def toRleVectorI (colPos: Int): RleVectorI = Vec.toInt (col(colPos)).asInstanceOf [RleVectorI]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colName' column of 'this' relation into a vector of integers.
     *  @param colName  the column name to use for the vector
     */
    def toRleVectorI (colName: String): RleVectorI = Vec.toInt (col(colMap(colName))).asInstanceOf [RleVectorI]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colPos' column of 'this' relation into a vector of integers.
     *  @param colPos  the column position to use for the vector
     */
    def toRleVectorS (colPos: Int): RleVectorS = col(colPos).asInstanceOf [RleVectorS]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'colName' column of 'this' relation into a vector of integers.
     *  @param colName  the column name to use for the vector
     */
    def toRleVectorS (colName: String): RleVectorS = col(colMap(colName)).asInstanceOf [RleVectorS]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the given columns within 'this' relation to a map: 'keyColPos' -> 'valColPos'.
     *  @param keyColPos  the key column positions
     *  @param valColPos  the value column positions
     */
    def toMap (keyColPos: Seq [Int], valColPos: Int): Map [Seq [Any], Any] =
    {
        val map = Map [Seq [Any], Any] ()
        for (i <- indices) {
            val tuple = row(i)
            map += keyColPos.map (tuple(_)) -> tuple(valColPos)
        } // for
        map
    } // toMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the given columns within 'this' relation to a map: 'keyColName' -> 'valColName'.
     *  @param keyColName  the key column names
     *  @param valColname  the value column names
     */
    def toMap (keyColName: Seq [String], valColName: String): Map [Seq [Any], Any] =
    {
        toMap (keyColName.map (colMap(_)), colMap(valColName))
    } // toMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save 'this' relation in a file using serialization.
     */
    def save ()
    {
        val oos = new ObjectOutputStream (new FileOutputStream (STORE_DIR + name + SER))
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
        out.println (colName.toString.drop (5).dropRight (1))
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

    // ============================================ BUILT-IN AGGREGATE FUNCTIONS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean of the values in column 'cName'.
     *  @param cName  the column name
     */
    def avg (cName: String) = Vec.mean (col(colMap(cName)))
    def mean (cName: String) = Vec.mean (col(colMap(cName)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of values in column 'cName'.
     *  @param cName  the column name
     */
    def count (cName: String) = rows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum value in column 'cName'.
     *  @param cName  the column name
     */
    def max (cName: String) = Vec.max (col(colMap(cName)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum value in column 'cName'.
     *  @param cName  the column name
     */
    def min (cName: String) = Vec.min (col(colMap(cName)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sum of the values in column 'cName'.
     *  @param cName  the column name
     */
    def sum (cName: String) = Vec.sum (col(colMap(cName)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the variance of the values in column 'cName'.
     *  @param cName  the column name
     */
    def variance (cName: String) = Vec.variance (col(colMap(cName)))

} // Relation class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationEx` object provides and example relation for testing.
 *  @see www.codeproject.com/Articles/652108/Create-First-Data-WareHouse
 */
object RelationEx
{
    val productSales = Relation ("productSales",
        Seq ("SalesInvoiceNumber", "SalesDateKey", "SalesTimeKey", "SalesTimeAltKey", "StoreID", "CustomerID",
             "ProductID", "SalesPersonID", "Quantity", "ProductActualCost", "SalesTotalCost", "Deviation"),
        Seq (Vector [Any] (1,  20130101, 44347, 121907, 1, 1, 1, 1, 2,  11.0,  13.0, 2.0),
             Vector [Any] (1,  20130101, 44347, 121907, 1, 1, 2, 1, 1,  22.5,  24.0, 1.5),
             Vector [Any] (1,  20130101, 44347, 121907, 1, 1, 3, 1, 1,  42.0,  43.5, 1.5),
             Vector [Any] (2,  20130101, 44519, 122159, 1, 2, 3, 1, 1,  42.0,  43.5, 1.5),
             Vector [Any] (2,  20130101, 44519, 122159, 1, 2, 4, 1, 3,  54.0,  60.0, 6.0),
             Vector [Any] (3,  20130101, 52415, 143335, 1, 3, 2, 2, 2,  11.0,  13.0, 2.0),
             Vector [Any] (3,  20130101, 52415, 143335, 1, 3, 3, 2, 1,  42.0,  43.5, 1.5),
             Vector [Any] (3,  20130101, 52415, 143335, 1, 3, 4, 2, 3,  54.0,  60.0, 6.0),
             Vector [Any] (3,  20130101, 52415, 143335, 1, 3, 5, 2, 1, 135.0, 139.0, 4.0),
             Vector [Any] (4,  20130102, 44347, 121907, 1, 1, 1, 1, 2,  11.0,  13.0, 2.0),
             Vector [Any] (4,  20130102, 44347, 121907, 1, 1, 2, 1, 1,  22.5,  24.0, 1.5),
             Vector [Any] (5,  20130102, 44519, 122159, 1, 2, 3, 1, 1,  42.0,  43.5, 1.5),
             Vector [Any] (5,  20130102, 44519, 122159, 1, 2, 4, 1, 3,  54.0,  60.0, 6.0),
             Vector [Any] (6,  20130102, 52415, 143335, 1, 3, 2, 2, 2,  11.0,  13.0, 2.0),
             Vector [Any] (6,  20130102, 52415, 143335, 1, 3, 5, 2, 1, 135.0, 139.0, 4.0),
             Vector [Any] (7,  20130102, 44347, 121907, 2, 1, 4, 3, 3,  54.0,  60.0, 6.0),
             Vector [Any] (7,  20130102, 44347, 121907, 2, 1, 5, 3, 1, 135.0, 139.0, 4.0),
             Vector [Any] (8,  20130103, 59326, 162846, 1, 1, 3, 1, 2,  84.0,  87.0, 3.0),
             Vector [Any] (8,  20130103, 59326, 162846, 1, 1, 4, 1, 3,  54.0,  60.0, 3.0),
             Vector [Any] (9,  20130103, 59349, 162909, 1, 2, 1, 1, 1,   5.5,   6.5, 1.0),
             Vector [Any] (9,  20130103, 59349, 162909, 1, 2, 2, 1, 1,  22.5,  24.0, 1.5),
             Vector [Any] (10, 20130103, 67390, 184310, 1, 3, 1, 2, 2,  11.0,  13.0, 2.0),
             Vector [Any] (10, 20130103, 67390, 184310, 1, 3, 4, 2, 3,  54.0,  60.0, 6.0),
             Vector [Any] (11, 20130103, 74877, 204757, 2, 1, 2, 3, 1,   5.5,   6.5, 1.0),
             Vector [Any] (11, 20130103, 74877, 204757, 2, 1, 3, 3, 1,  42.0,  43.5, 1.5)),
             0, "IIIIIIIIIDDD")

} // RelationEx object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest` object tests the operations provided by `Relation`.
 *  > runMain scalation.columnar_db.RelationTest
 */
object RelationTest extends App
{
    val weekdays = new Relation ("weekdays", Seq ("day", "time"),
                                 Vector (VectorS ("Mon", "Tue", "Wed", "Thu", "Fri"),
                                         VectorD (5.00, 8.15, 6.30, 9.45, 7.00)),
                                 0, "SD")

    val weekend = new Relation ("weekends", Seq ("day", "time"),
                                 Vector (VectorS ("Sat", "Sun"),
                                         VectorD (3.00, 4.30)),
                                 0, "SD")

    weekdays.generateIndex ()
    weekend.generateIndex ()

    banner ("weekdays")
    println ("weekdays                                  = " + weekdays)
    banner ("weekdend")
    println ("weekend                                   = " + weekend)

    banner ("Test pi")
    println ("weekdays.pi (\"day\")                     = " + weekdays.pi ("day"))
    println ("-" * 60)
    println ("weekdays.pisigmaS (\"day\", _ == \"Mon\") = " + weekdays.pisigmaS ("day", _ == "Mon"))

    banner ("Test sigma")
    println ("weekdays.sigmaS (\"day\", _ == \"Mon\")   = " + weekdays.sigmaS ("day", _ == "Mon"))
    println ("-" * 60)
    println ("weekdays.sigma (\"day\", _ == \"Mon\")    = " + weekdays.sigma ("day", (x: StrNum) => x == "Mon"))
    println ("-" * 60)
    println ("weekdays.sigma (\"time\", _ == 5.00)      = " + weekdays.sigma ("time", (x: Double) => x == 5.00))
    println ("-" * 60)
    println ("weekdays.sigmaS (\"day\", _ > \"Mon\")    = " + weekdays.sigmaS ("day", _ > "Mon"))
    println ("-" * 60)
    println ("weekdays.selectS (\"day\", _ > \"Mon\")   = " + weekdays.selectS ("day", _ > "Mon"))
    println ("-" * 60)
    println ("weekdays.sigmaSD (\"day\", \"time\")      = " + weekdays.sigmaS ("day",  _ == "Mon"))

    val week = weekdays.union (weekend)
    banner ("Test union")
    println ("weekdays.union (weekend)                  = " + week)

    weekend.add (Vector ("Zday", 1.00))
    banner ("Test add")
    println ("weekend add (\"Zday\", 1.00))             = " + weekend)

    banner ("Test -")
    println ("week - weekend                            = " + (week - weekend))

    banner ("Test join")
    println ("week.join (\"day\", \"day\" weekend)      = " + week.join ("day", "day", weekend))
    println ("-" * 60)
    println ("week join weekend                         = " + (week join weekend))

    week.writeCSV ("columnar_db" + ⁄ + "week.csv")

} // RelationTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest2` object tests the operations provided by `Relation`.
 *  The relational algebra operators are given using Unicode.
 *  @see en.wikipedia.org/wiki/List_of_Unicode_characters
 *  > runMain scalation.columnar_db.RelationTest2
 */
object RelationTest2 extends App
{
    val weekdays = new Relation ("weekdays", Seq ("day", "time"),
                                 Vector (VectorS ("Mon", "Tue", "Wed", "Thu", "Fri"),
                                         VectorD (5.00, 8.15, 6.30, 9.45, 7.00)),
                                 0, "SD")

    val weekend = new Relation ("weekends", Seq ("day", "time"),
                                Vector (VectorS ("Sat", "Sun"),
                                        VectorD (3.00, 4.30)),
                                0, "SD")

    banner ("Test π")
    println ("weekdays.π (\"day\")               = " + weekdays.π ("day"))
    println ("-" * 60)
    println ("weekdays.π (\"time\")              = " + weekdays.π ("time"))

    banner ("Test σ")
    println ("weekdays.σ (\"day\", _ == \"Mon\") = " + weekdays.σ ("day", (x: StrNum) => x == "Mon"))
    println ("-" * 60)
    println ("weekdays.σ (\"time\", _ == 5.00)   = " + weekdays.σ ("time", (x: Double) => x == 5.00))
    println ("-" * 60)
    println ("weekdays.σ (\"day\", _ > \"Mon\")  = " + weekdays.σ ("day", (x: StrNum) => x > "Mon"))
    println ("-" * 60)
    println ("weekdays.σ (\"time\", _ > 5.00)    = " + weekdays.σ ("time", (x: Double) => x > 5.00))
    println ("-" * 60)
    println ("weekdays.σ (\"day\", \"time\")     = " + weekdays.σ ("day",  (x: StrNum) => x == "Mon")
                                                               .σ ("time", (x: Double) => x == 5.00))
    val week = weekdays ⋃ weekend

    banner ("Test ⋃")
    println ("weekdays ⋃ weekend)                = " + weekdays ⋃ weekend)

    banner ("Test ⋂")
    println ("week ⋂ weekend                     = " + (week ⋂ weekend))

    banner ("Test -")
    println ("week - weekend                     = " + (week - weekend))

    banner ("Test ⋈ ")
    println ("week ⋈ weekend                     = " + (week ⋈ weekend))

} // RelationTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest3` object tests the operations provided by `Relation`.
 *  It test various aggregate/OLAP operations on a simple data warehouse fact table.
 *  @see www.codeproject.com/Articles/652108/Create-First-Data-WareHouse
 *  FIX - allow entering doubles as "13" rather than "13.0"
 *  > runMain scalation.columnar_db.RelationTest3
 */
object RelationTest3 extends App
{
    import Relation.{max, min}
    import RelationEx.productSales

    val costVprice = productSales.project ("ProductActualCost", "SalesTotalCost")

    productSales.show ()

    println ("productSales = " + productSales)
    println ("productSales.project (\"ProductActualCost\", \"SalesTotalCost\") = " + costVprice)

    banner ("Test count")
    println ("count (productSales) = " + count (productSales))
    println ("-" * 60)
    println ("count (costVprice)   = " + count (costVprice))

    banner ("Test min")
    println ("min (productSales)   = " + min (productSales))
    println ("-" * 60)
    println ("min (costVprice)     = " + min (costVprice))

    banner ("Test max")
    println ("max (productSales)   = " + max (productSales))
    println ("-" * 60)
    println ("max (costVprice)     = " + max (costVprice))

    banner ("Test sum")
    println ("sum (productSales)   = " + sum (productSales))
    println ("-" * 60)
    println ("sum (costVprice)     = " + sum (costVprice))

    banner ("Test expectation/mean")
    println ("Ɛ (productSales)     = " + Ɛ (productSales))
    println ("-" * 60)
    println ("Ɛ (costVprice)       = " + Ɛ (costVprice))

    banner ("Test variance")
    println ("Ʋ (productSales)     = " + Ʋ (productSales))
    println ("-" * 60)
    println ("Ʋ (costVprice)       = " + Ʋ (costVprice))

    banner ("Test correlation")
    println ("corr (costVprice)    = " + corr (costVprice))

} // RelationTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest4` object tests conversion `Relation` to a matrix.
 *  > runMain scalation.columnar_db.RelationTest4
 */
object RelationTest4 extends App
{
    import RelationEx.productSales

    val (mat, vec) = productSales.toMatriDD (0 to 10, 11)

    banner ("productSales")
    productSales.show ()

    banner ("mat and vec")
    println ("mat = " + mat)
    println ("vec = " + vec)

} // RelationTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest5` object tests the interoperability between Relations and Matrices.
 *  > runMain scalation.columnar_db.RelationTest5
 */
object RelationTest5 extends App
{
    val sales_item1 =  Relation ("Sales_Item1", Seq ("Date", "FL", "GA", "NC", "SC"),
        Seq (Vector [Any] ("20130101", 10, 5, 5, 4),
             Vector [Any] ("20130102", 20, 30, 40, 25),
             Vector [Any] ("20130103", 8, 6, 9, 9),
             Vector [Any] ("20130104", 6, 7, 9, 10),
             Vector [Any] ("20130105", 4, 7, 9, 10)),
        0,"SIIII")

    val price_item1 =  Relation ("Price_Item1", Seq ("Date", "FL", "GA", "NC", "SC"),
        Seq (Vector [Any] ("20130101", 1.6, 1.6, 1.5, 1.3),
             Vector [Any] ("20130102", 1.6, 1.6, 1.5, 1.2),
             Vector [Any] ("20130103", 1.5, 1.6, 1.5, 1.4),
             Vector [Any] ("20130104", 1.4, 1.7, 1.5, 1.4),
             Vector [Any] ("20130105", 1.4, 1.7, 1.4, 1.4)),
        0,"SDDDD")
    val revenue     =  Relation ("Revenue", -1, null, "Item", "FL", "GA", "NC", "SC")

    sales_item1.show ()
    price_item1.show ()

    val x = sales_item1.toMatriD (1 to 4, COMPRESSED)
    val y = price_item1.toMatriD (1 to 4, COMPRESSED)
    val z = x dot y
    revenue.add ("Item1" +: z().toVector)

    banner ("revenue")
    revenue.show ()

} // RelationTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest6` object tests 'indexjoin', 'parjoin', 'groupby' and 'aggregation'.
 *  > runMain scalation.columnar_db.RelationTest6
 */
object RelationTest6 extends App
{
    val professor = Relation ("professor", 0, "ISS", "pid", "name", "prodeptid")
    TableGen.popTable (professor, 10)
    professor.generateIndex ()

    val course = Relation ("course", 0, "ISS", "cid","crsname", "descr")
    TableGen.popTable (course, 20)
    course.generateIndex ()

    val teaching = Relation ("teaching", 0, "IISI", "tid", "cid", "semester", "pid")
    teaching.fKeys = Seq (("cid", "course", 0), ("pid", "professor", 0))
    TableGen.popTable (teaching, 50, Seq (course, professor))
    teaching.generateIndex ()

    banner ("database")
    professor.show ()
    course.show ()
    teaching.show ()
    teaching.showFk ()

    banner ("joinindex")
    teaching.joinindex (Seq ("pid"), Seq("pid"), professor).show ()
    banner ("parjoin")
    teaching.parjoin (Seq ("pid"), Seq ("pid"), professor, 4).show ()
    banner ("groupBy.eproject")
    teaching.groupBy ("cid").eproject ((count, "pid_count", "pid"))("tid", "semester").show ()

} // RelationTest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest7` object tests 'join' method.
 *  > runMain scalation.columnar_db.RelationTest7
 */
object RelationTest7 extends App
{
    val professor = Relation ("professor",
        Seq("pid", "name", "department", "title"),
        Seq (Vector [Any] (1, "jackson", "pharm", 4),
             Vector [Any] (2, "ken", "cs", 2),
             Vector [Any] (3, "pan", "pharm", 0),
             Vector [Any] (4, "yang", "gis", 3),
             Vector [Any] (5, "zhang", "cs", 0),
             Vector [Any] (6, "Yu", "cs", 0)),
        -1, "ISSI")

    val professor2 = Relation ("professor",
        Seq ("pid", "name", "department", "title"),
        Seq (Vector [Any] (7, "LiLy", "gis", 5),
             Vector [Any] (8, "Marry", "gis", 5),
             Vector [Any] (0, "Kate", "cs", 5)),
        0, "ISSI")

    professor.generateIndex ()
    professor2.generateIndex ()

    banner ("professor")
    professor.show ()
    banner ("professor2")
    professor2.show ()

    banner ("join")
    professor.join [Int] (professor2, ("pid", "pid", (x, y) => x < y)).show ()
    professor.join [Int] (professor2, ("pid", "pid", _ < _)).show ()

} // RelationTest7


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest8` object tests 'save' method.
 *  > runMain scalation.columnar_db.RelationTest8
 */
object RelationTest8 extends App
{
    val professor = Relation ("professor",
        Seq("pid", "name", "department", "title"),
        Seq (Vector [Any] (1, "jackson", "pharm", 4),
             Vector [Any] (2, "ken", "cs", 2),
             Vector [Any] (3, "pan", "pharm", 0),
             Vector [Any] (4, "yang", "gis", 3),
             Vector [Any] (5, "zhang", "cs", 0),
             Vector [Any] (6, "Yu", "cs", 0)),
        -1, "ISSI")

    professor.save ()

} // RelationTest8


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest9` object tests 'apply' method to load a saved relation.
 *  > runMain scalation.columnar_db.RelationTest9
 */
object RelationTest9 extends App
{
    Relation ("professor").show ()

} // RelationTest9


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationTest10` object tests the 'orderBy' method.
 *  > runMain scalation.columnar_db.RelationTest10
 */
object RelationTest10 extends App
{
    import RelationEx.productSales

    productSales.orderBy ("SalesTotalCost", "Deviation").show ()

} // RelationTest10

