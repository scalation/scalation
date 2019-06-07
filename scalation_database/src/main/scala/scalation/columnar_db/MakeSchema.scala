
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.6
 *  @date    Tue Oct 6 12:27:00 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.columnar_db

import scala.util.control.Breaks.{break, breakable}

import scalation.linalgebra.Converter.map2Int
import scalation.linalgebra.{MatrixD, MatrixI, Vec, VectorS, mem_mapped}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MakeSchema` object attempts to infers the domain of a relation without a
 *  domain specification and creates a new relation with the inferred domains by
 *  examining the data.  Should be checked and overridden when inaccurate.
 */
object MakeSchema
{
    /** Transition matrix: state = function (state, datatype)
     */
    private val tMatrix = new MatrixI ((4, 4), 0, 1, 2, 3,    // Int
                                               1, 1, 2, 3,    // Long
                                               2, 2, 2, 3,    // Double
                                               3, 3, 3, 3)    // String

    /** Regular expression for integers
     */
    private val intPattern = "[\\-\\+]?\\d+".r.pattern

    /** Regular expression for floating point numbers
     */
    private val doublePattern = "[\\-\\+]?\\d*(\\.\\d+)?".r.pattern

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Infer the domains of a domain-less relation and create a new relation with
     *  the inferred domains (e.g., 'IIDIS').
     *  @param r  the relation to have its domain inferred
     */
    def apply (r: Relation): Relation =
    {
        var newCol = Vector [Vec] ()
        var domain = ""
        for (i <- 0 until r.cols) {
            val (c, d) = analyzeType (r.col(i).asInstanceOf [VectorS])
            newCol = newCol :+ c
            domain += d
        } // for
        new Relation (r.name + "_s", r.colName, newCol, r.key, domain)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Infer the domains of a domain-less memory mapped relation and create a new
     *  memory mapped relation with the inferred domains (e.g., 'IIDIS').
     *  @param r  the memory mapped relation to have its domain inferred
     */
    def apply (r: MM_Relation): MM_Relation =
    {
        var newCol = Vector [mem_mapped.Vec] ()
        var domain = ""
        for (i <- 0 until r.cols) {
            val (c, d) = mm_analyzeType (r.col(i).asInstanceOf [mem_mapped.VectorS])
            newCol = newCol :+ c
            domain += d
        } // for
        new MM_Relation (r.name + "_s", r.colName, newCol, r.key, domain)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Analyze the type of a given `VectorS` by using regular expression matching.
     *  Types can be Integer (I), Long (L), Double (D), etc.
     *  @param c              the VectorS to have its type analyzed
     *  @param samplePercent  the percentage of 'c' used to analyze the type of 'c'
     *  @param mapString      flag used to map each distinct string in 'c' to a distinct integer
     */
    def analyzeType (c: VectorS, samplePercent: Int = 100, mapString: Boolean = false):
                     Tuple2[Vec, String] =
    {
        val np    = c.dim * (samplePercent / 100.0)
        var state = 0

        breakable {for (i <- 0 until np.toInt) {
            val typ = if (intPattern.matcher (c(i).toString).matches ())
                          if (c(i).toLong > Int.MaxValue || c(i).toLong < Int.MinValue) 1
                          else 0
                      else if (doublePattern.matcher (c(i).toString).matches ()) 2
                      else 3

            state = tMatrix(state, typ)
            if (state == 3) break
        }} // breakable for

        state match {
        case 0 => (c.toInt, "I")
        case 1 => (c.toLong, "L")
        case 2 => (c.toDouble, "D")
        case _ => if (mapString) (map2Int(c)._1, "I") else (c, "S")
        } // match
    } // analyzeType

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Analyze the type of a given memory mapped `VectorS` by using regular expression
     *  matching.  Types can be Integer (I), Long (L), Double (D), etc.
     *  @param c              the VectorS to have its type analyzed
     *  @param samplePercent  the percentage of 'c' used to analyze the type of 'c'
     *  @param mapString      flag used to map each distinct string in 'c' to a distinct integer
     */
    def mm_analyzeType (c: mem_mapped.VectorS, samplePercent: Int = 100, mapString: Boolean = false):
                        Tuple2[mem_mapped.Vec, String] =
     {
        import scalation.linalgebra.mem_mapped.Converter.map2Int

        val np    = c.dim * (samplePercent / 100.0)
        var state = 0

         breakable {for (i <- 0 until np.toInt) {
             val typ = if (intPattern.matcher (c(i).toString).matches ())
                           if (c(i).toLong > Int.MaxValue || c(i).toLong < Int.MinValue) 1
                           else 0
                       else if (doublePattern.matcher (c(i).toString).matches ()) 2
                       else 3

             state = tMatrix(state, typ)
             if (state == 3) break
         }} // breakable for

         state match {
             case 0 => (c.toInt, "I")
             case 1 => (c.toLong, "L")
             case 2 => (c.toDouble, "D")
             case _ => if (mapString) (map2Int(c)._1, "I") else (c, "S")
         } // match
    } // mm_analyzeType

} // MakeSchema object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MakeSchemaTest` object is used to test the `MakeSchema` object.
 *  > runMain scalation.columnar_db.MakeSchemaTest
 */
object MakeSchemaTest extends App
{
    val productSales = Relation ("productSales",
        Seq ("SalesInvoiceNumber", "SalesDateKey", "SalesTimeKey", "SalesTimeAltKey", "StoreID", "CustomerID",
             "ProductID", "SalesPersonID", "Quantity", "ProductActualCost", "SalesTotalCost", "Deviation"),
        Seq (Vector [Any] (1,  20130101, 44347, 121907, 1, 1, 1, 1, 2,  11.0,  13, 2.0),
             Vector [Any] (1,  20130101, 44347, 121907, 1, 1, 2, 1, 1,  22.5,  24.0, 1.5),
             Vector [Any] (1,  20130101, "s44347", 121907, 1, 1, 3, 1, 1,  42.0,  43.5, 1.5),   // made a string for testing
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
             Vector [Any] (11, 20130103000L, 74877, 204757, 2, 1, 3, 3, 1,  42.0,  43.5, 1.5)),
        0)
 
    println ("initial domain = " + productSales.domain)
    val productSales_s = MakeSchema (productSales)
    productSales_s.show ()
    println ("final domain = " + productSales_s.domain)

} // MakeSchemaTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MakeSchemaTest2` object is used to test the `MakeSchema` object.
 *  > runMain scalation.columnar_db.MakeSchemaTest2
 */
object MakeSchemaTest2 extends App
{
    val url = "https://raw.githubusercontent.com/scalation/analytics/develop/examples/auto_mpg.csv"
    val auto_mpg = Relation (url, "auto_mpg", -1, null, ",", null)

    println ("initial domain = " + auto_mpg.domain)

    val auto_mpg_s = MakeSchema (auto_mpg)
    auto_mpg_s.show ()
    println ("final domain = " + auto_mpg_s.domain)

    val xy = auto_mpg_s.toMatriDD (1 until 8, 0)
    println ("xy = " + xy)

} // MakeSchemaTest2 object

