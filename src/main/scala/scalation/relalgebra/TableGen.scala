
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Nov 17 20:55:43 EST 2016
 *  @see     LICENSE (MIT style license file).
 */
package scalation.relalgebra

import scala.util.control.Breaks.{break, breakable}

import scalation.linalgebra.Vec
import scalation.random._
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TableGen` object generates data for `Table` classes (e.g., `Relation`).
 */
object TableGen
       extends Error
{
    private val DEBUG = true                                   // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Populate the columns in columnar table 'table'.
     *  @param table     the table/relation to populate
     *  @param refTable  the tables that are referenced by 'table' via its foreign keys
     */
    def popTable (table: Relation, nRows: Int, refTables: Seq [Relation] = null)
    {
        val ranD  = RandomVecD (dim = nRows, max = 2 * nRows)
        val ranI  = RandomVecI (dim = nRows, max = 2 * nRows, unique = false)
        val ranS  = RandomVecS (dim = nRows, unique = false)
        val uranI = RandomVecI (dim = nRows, max = 2 * nRows)
        val uranS = RandomVecS (dim = nRows)

        val n     = table.cols                                 // number of columns
        val cn    = table.colName                              // column names
        val dn    = table.domain                               // columns  domains
        val pk    = table.key                                  // position of primary key
        val fk    = table.fKeys                                // sequence of foreign key specifications
        var cols  = Vector.empty [Vec]

        for (j <- cn.indices) {                                // for jth column
            val fk_i  = find (cn(j), fk)                       // which foreign key, if any
            val colj = if (j == pk) genUnique (dn(j))          // generate values for jth column
                       else if (fk_i >= 0) genFk (fk_i)
                       else genVal (dn(j))
            cols = cols :+ colj                                // FIX - need more efficient approach
        } // for

        if (DEBUG) println (s"popTable: cols = $cols")
        table.col = cols

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Generate values for column 'j' by extracting random values from
         *  the column in the referenced table specified by 'fk_j'.
         *  @param fk_i  the relevant foreign key constraint specification
         */
        def genFk (fk_i: Int): Vec =
        {
            val refTName = fk(fk_i)._2
            val refCol   = fk(fk_i)._3
            var iref     = -1
            if (DEBUG) println (s"genFk: refTName = $refTName")
            breakable { for (k <- refTables.indices if refTName == refTables(k).name) {
                iref = k
                break
            }} // breakable for
            if (iref >= 0) {  
                val refTable = refTables(iref) 
                val rCol     = refTable.col(refCol)
                val ranK     = RandomVecI (dim = nRows, max = refTable.rows - 1, unique = false)
                Vec (rCol, ranK.igen)
            } else {
                flaw ("genFk", s"reference table $refTName not matched")
                null
            } // if
        } // genFk

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Randomly Generate unique values for column 'j'.
         *  @param dn  the domain (datatype)
         */
        def genUnique (dn: Char): Vec =
        {
            dn match {
            case 'D' => { flaw ("genUnique", "type `Double` should not be a primary key"); null }
            case 'I' => uranI.igen
            case 'L' => uranI.igen
            case 'S' => uranS.sgen
            case _  => { flaw ("genUnique", "type not supported"); null }
            } // match
        } // genUnique

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Randomly Generate values for column 'j'.
         *  @param dn  the domain (datatype)
         */
        def genVal (dn: Char): Vec =
        {
            dn match {
            case 'D' => ranD.gen
            case 'I' => ranI.igen
            case 'L' => ranI.igen
            case 'S' => ranS.sgen
            case _  => { flaw ("genVal", "type not supported"); null }
            } // match
        } // genVal

    } // popTable 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the column with name 'cn' in a foreign key constraint, returning
     *  the which constraint is matched, -1 otherwise.
     *  @param cn  the name of the column
     *  @param fk  the sequence of foreign keys contraint specifications
     */
    def find (cn: String, fk: Seq [(String, String, Int)]): Int =
    {
        if (fk == null) return -1
        for (i <- fk.indices if cn == fk(i)._1) return i    // matched ith contraint
        -1
    } // find

} // TableGen object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TableGenTest` is used to test the `TableGen` object.  Create unpopulated
 *  tables and use the table generator to populate their columns.
 *  > run-main scalation.relalgebra.TableGenTest
 */
object TableGenTest extends App
{
    val student = Relation ("student", 0, "ISSS", "sid", "name", "address", "status")
    TableGen.popTable (student, 40)
    student.show ()

    val professor = Relation ("professor", 0, "ISS", "pid", "name", "deptid")
    TableGen.popTable (professor, 10)
    professor.show ()

    val course = Relation ("course", 0, "SSSS", "cid", "deptid", "crsname", "descr")
    TableGen.popTable (course, 20)
    course.show ()

    val teaching = Relation ("teaching", 0, "ISSI", "tid", "cid", "semester", "pid")
    teaching.fKeys = Seq (("cid", "course", 0), ("pid", "professor", 0))
    TableGen.popTable (teaching, 50, Seq (course, professor))
    teaching.show ()
    teaching.showFk ()

    val transript = Relation ("transript", 0, "IISSS", "trid", "sid", "trid", "grade")
    transript.fKeys = Seq (("sid", "student", 0), ("trid", "teaching", 0))
    TableGen.popTable (transript, 70, Seq (student, teaching))
    transript.show ()
    transript.showFk ()

} // TableGenTest object

