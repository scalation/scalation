//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller, Santosh Uttam Bobade
 *  @version 1.6
 *  @date    Sat Aug  4 16:05:08 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package columnar_db

import scala.reflect.ClassTag

import scalation.linalgebra.Vec
import scalation.math.{noDouble, noInt}
import scalation.math.StrO.StrNum
import scalation.random.Random
import scalation.stat.vectorD2StatVector
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MissingValues` object is used to replace missing values in a dataset.
 *  @see www.utexas.edu/cola/prc/_files/cs/Missing-Data.pdf
 */
object MissingValues
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Replace missing values in column 'missingCol' of table 'xy'.
     *  @param xy          the table/relation with missing values
     *  @param missingCol  the name of column where missing values are to be replaced
     *  @param missingVal  the value used to denote a missing value (e.g. "?" for string)
     *  @param funcVal     the imputation technique for imputing missing values
     *  @param fraction    the maximum allowed proportion of missing values, e.g., 0.2
     *  @tparam T          type of missingVal
     */
    def replaceMissingValues [T <: Any : ClassTag] (xy: Table, missingCol: String, missingVal: T,
                                                    funcVal: Imputation = ImputeMean, fraction: Double = 0.2)
    {
        val c = xy.column (missingCol)
        val maxCount = (fraction * xy.rows).toInt
        var count = 0                              // counts the number of missing values
        for (i <- 0 until xy.rows) {
            if (Vec (c, i) equals missingVal) {
                count += 1
                if (count > maxCount) {
                    println (s"Number of missing values exceeded the threshold in column ${missingCol}. Consider removal.")
                    return
                } // if
                Vec (c, i) = funcVal.impute (c, i)
            } // if
        } // for
    } // replaceMissingValues

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Replace missing strings in column 'missingCol' of table 'xy'.
     *  @param xy          the table/relation with missing values
     *  @param missingCol  the name of column where missing values are to be replaced
     *  @param missingStr  the string used to denote a missing value (e.g. "?")
     *  @param funcVal     the imputation technique for imputing missing values
     *  @param fraction    the maximum allowed proportion of missing values, e.g., 0.2
     */
    def replaceMissingStrings (xy: Table, missingCol: String, missingStr: String = "?",
                              funcVal: Imputation = ImputeMean, fraction: Double = 0.2)
    {
        val c = xy.column (missingCol)
        val maxCount = (fraction * xy.rows).toInt
        var count = 0                              // counts the number of missing values
        for (i <- 0 until xy.rows) {
            if (Vec (c, i).toString equals missingStr) {
                count += 1
                if (count > maxCount) {
                    println (s"Number of missing values exceeded the threshold in column ${missingCol}. Consider removal.")
                    return
                } // if
                Vec (c, i) = funcVal.impute (c, i)
            } // if
        } // for
    } // replaceMissingStrings

} // MissingValues object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MissingValuesTable` object build a data realtion containing missing values.
 */
object MissingValuesTable
{
    val productSales = Relation ("productSales",
                                 Seq ("SalesInvoiceNumber", "SalesDateKey", "SalesTimeKey", "SalesTimeAltKey", "StoreID", "CustomerID",
                                      "ProductID", "SalesPersonID", "Quantity", "ProductActualCost", "SalesTotalCost", "Deviation"),
                                 Seq (Vector [Any] (1,  20130101, 44347, 121907, 1, 1, 1, 1, 2,  11.0,  13.0, 2.0),
                                      Vector [Any] (1,  20130101, 44347, 121907, 1, 1, 2, 1, 1,  22.5,  24.0, 1.5),
                                      Vector [Any] ("?",  20130101, 44347, 121907, 1, 1, 3, 1, 1,  42.0,  43.5, 1.5),         // missing value 1
                                      Vector [Any] (2,  20130101, 44519, 122159, 1, 2, 3, 1, 1,  42.0,  43.5, 1.5),
                                      Vector [Any] (2,  20130101, 44519, 122159, 1, 2, 4, 1, 3,  54.0,  60.0, 6.0),
                                      Vector [Any] (3,  20130101, 52415, 143335, 1, 3, 2, 2, 2,  11.0,  13.0, 2.0),
                                      Vector [Any] (3,  20130101, 52415, 143335, 1, 3, 3, 2, 1,  42.0,  43.5, 9.0),
                                      Vector [Any] (3,  20130101, 52415, noInt, 1, 3, 4, 2, 3,  54.0,  60.0, 6.0),            // missing value 2
                                      Vector [Any] (3,  20130101, 52415, 143335, 1, 3, 5, 2, 1, 135.0, 139.0, 4.0),
                                      Vector [Any] (4,  20130102, 44347, 121907, 1, 1, 1, 1, 2,  noDouble,  13.0, 2.0),       // missing value 3
                                      Vector [Any] (4,  20130102, 44347, 121907, 1, 1, 2, 1, 1,  22.5,  24.0, 1.5),
                                      Vector [Any] (5,  20130102, 44519, 122159, 1, 2, 3, 1, 1,  42.0,  43.5, 1.5),
                                      Vector [Any] (5,  20130102, 44519, 122159, 1, 2, 4, 1, 3,  54.0,  60.0, 6.0),
                                      Vector [Any] (6,  20130102, 52415, 143335, 1, 3, 2, 2, 2,  11.0,  13.0, 2.0),
                                      Vector [Any] (6,  20130102, 52415, 143335, 1, 3, 5, 2, 1, 135.0, 139.0, 4.0),
                                      Vector [Any] (7,  20130102, 44347, 121907, 2, 1, 4, 3, 3,  54.0,  60.0, 6.0),
                                      Vector [Any] (7,  20130102, 44347, 121907, 2, 1, 5, 3, 1, 135.0, 139.0, 4.0),
                                      Vector [Any] (8,  20130103, 59326, 162846, 1, 1, 3, 1, 2,  84.0,  87.0, 3.0),
                                      Vector [Any] (8,  20130103, 59326, 162846, 1, 1, 4, 1, 3,  54.0,  'X', 3.0),            // missing value 4
                                      Vector [Any] (9,  20130103, 59349, 162909, 1, 2, 1, 1, 1,   5.5,   6.5, 1.0),
                                      Vector [Any] (9,  20130103, 59349, 162909, 1, 2, 2, 1, 1,  22.5,  24.0, 1.5),
                                      Vector [Any] (10, 20130103, 67390, 184310, 1, 3, 1, 2, 2,  11.0,  13.0, 2.0),
                                      Vector [Any] (10, 20130103, 67390, 184310, 1, 3, 4, 2, 3,  54.0,  60.0, 6.0),
                                      Vector [Any] (11, 20130103, 74877, 204757, 2, 1, 2, 3, 1,   5.5,   6.5, 1.0),
                                      Vector [Any] (11, 20130103, 74877, 204757, 2, 1, 3, 3, 1,  42.0,  43.5, 1.5)),
                                 0, "IIIIIIIIIDDD")
//                               0, "SIIIIIIIIDDD")                                                                            // treat first column as StrNum

} // MissingValuesTable object

import MissingValues.replaceMissingValues
import MissingValuesTable.productSales

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MissingValuesTest` object is used to test the `MissingValues` object.
 *  > runMain scalation.columnar_db.MissingValuesTest
 */
object MissingValuesTest extends App
{
    banner ("productSales - before replacing missing values in column SalesInvoiceNumber")
    productSales.show ()

    replaceMissingValues (productSales, "SalesInvoiceNumber", noInt, Interpolate)

    banner ("productSales - after replacing missing values using Interpolate")
    productSales.show ()

} // MissingValuesTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MissingValuesTest2` object is used to test the `MissingValues` object.
 *  > runMain scalation.columnar_db.MissingValuesTest2
 */
object MissingValuesTest2 extends App
{
    banner ("productSales - before replacing missing values in column SalesTimeAltKey")
    productSales.show ()

    replaceMissingValues (productSales, "SalesTimeAltKey", noInt, ImputeMean)

    banner ("productSales - after replacing missing values using ImputeMean")
    productSales.show ()

} // MissingValuesTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MissingValuesTest3` object is used to test the `MissingValues` object.
 *  > runMain scalation.columnar_db.MissingValuesTest3
 */
object MissingValuesTest3 extends App
{
    banner ("productSales - before replacing missing values in column ProductActualCost")
    productSales.show ()

    replaceMissingValues (productSales, "ProductActualCost", noDouble, ImputeNormal)

    banner ("productSales - after replacing missing values using ImputeNormal")
    productSales.show ()

} // MissingValuesTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MissingValuesTest4` object is used to test the `MissingValues` object.
 *  > runMain scalation.columnar_db.MissingValuesTest4
 */
object MissingValuesTest4 extends App
{
    banner ("productSales - before replacing missing values in column SalesTotalCost")
    productSales.show ()

    replaceMissingValues (productSales, "SalesTotalCost", noDouble, ImputeMovingAverage)

    banner ("productSales - after replacing missing values using ImputeMovingAverage")
    productSales.show ()

} // MissingValuesTest4 object

