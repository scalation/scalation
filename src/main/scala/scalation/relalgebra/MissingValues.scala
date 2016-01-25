//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller
 *  @version 1.2
 *  @date    Sun Dec 13 19:22:42 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.relalgebra

import scalation.math.StrO.StrNum
import scalation.random.Random
import scalation.stat.vectorD2StatVector
import scalation.util.SEP

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MissingValues` object is used to replace missing values in a dataset.
 *  @see www.utexas.edu/cola/prc/_files/cs/Missing-Data.pdf
 */
object MissingValues
{
    /** Random number generator
     */
    private val stream = 0   

    /** Random number generator
     */
    private val rng = Random ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the mean and variance for the column in table 'xy' having missing values.
     *  @param xy          the table/relation with missing values
     *  @param missingCol  the name of column having missing values
     *  @param missingStr  the string used to denote a missing value (defaults to "?")
     */
    def estimateStats (xy: Table, missingCol: String, missingStr: String = "?"): (Double, Double) =
    {
        val mTable = xy.pisigmaS (missingCol, (x: StrNum) => x != missingStr)
        val mCol   = mTable.asInstanceOf [Relation].toVectorD(0)
        (mCol.mean, mCol.stddev)
    } // estimateStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Replace missing values in column 'missingCol' of table 'xy' and return a new
     *  table 'xy2' with the replaced values and excluded columns removed.
     *  @param xy          the table/relation with missing values
     *  @param ignore      the columns to ignore/exclude
     *  @param missingCol  the name of column having missing values
     *  @param missingStr  the string used to denote a missing value (defaults to "?")
     */
    def replaceMissingValues (xy: Table, ignore: Seq [Int], missingCol: String, missingStr: String = "?"): Table =
    {
        val (mMean, mStddev) = estimateStats (xy, missingCol, missingStr)
        def est () = (mMean + (rng.gen-0.5)*mStddev).toString               // FIX - use a better estimation technique

        val keep = (0 until xy.cols) diff ignore
        val xy2 = xy.pi (keep).asInstanceOf [Relation]

        xy2.update (missingCol, est, missingStr)
//      xy2.toMatriD (0 until xy2.cols).asInstanceOf [MatrixD]              // to return a matrix
        xy2                                                                 // return new table
    } // replaceMissingValues

} // MissingValues object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MissingValuesTest` object is used to test the `MissingValues` object
 *  > run-main scalation.relalgebra.MissingValuesTest
 */
object MissingValuesTest extends App {

    import MissingValues.replaceMissingValues

    val fname = "analytics" + SEP + "reaction_network.csv"
    val seq = (0 to 28).map (_.toString ())
    val tab1 = Relation (fname, "Reaction Network", seq, 0, null)
    val tab2 = replaceMissingValues (tab1, Seq (0), "4")
    println ("tab2 = " + tab2)

} // MissingValuesTest object

