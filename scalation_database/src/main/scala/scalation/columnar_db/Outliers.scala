
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Santosh Uttam Bobade, John Miller
 *  @version 1.6
 *  @date    Sat Jun 9 14:09:25 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.columnar_db

import scala.math.sqrt

import scalation.linalgebra.Vec
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Outlier` trait specifies an outlier detection operation to be defined by
 *  the objects implementing it, i.e.,
 *      `DistanceOutlier`  - outlier = beyond 'STDDEV_CUTOFF' units from mean
 *      `QuantileOutlier`  - outlier = in the 'PERCENTILE' tails of the distribution
 *      `QuartileOutlier`  - outlier = 'X_MULTIPLIER' times beyond the middle two quartiles
 */
trait Outlier
{
    protected val DEBUG = true                                  // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find outliers in column/vector 'c' and mark them as missing values.
     *  @param c     the column/vector with the possible outlier values
     *  @param args  optional double paramters
     */
    def rmOutliers (c: Vec, args: Double*)

} // Outlier trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Detect outliers in the vector by treating anything that falls outside of some distance
 *  from the mean as an outlier.
 */
object DistanceOutlier extends Outlier
{
    private val STDDEV_CUTOFF = 1.86                          // default stddev cutoff value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Detect outliers in the column/vector by treating anything that falls outside
     *  of the distance 'multipler * stddev' from the mean as an outlier.
     *  @param c     the column/vector with the possible outliers
     *  @param args  if present, args(0) gives stddev cutoff value
     */
    def rmOutliers (c: Vec, args: Double*)
    {
        val mult   = if (args.length > 0) args(0) else STDDEV_CUTOFF
        val v      = Vec.toDouble (c)
        val meanV  = v.mean
        val stddev = sqrt (v.variance)

        val low    = meanV - (stddev * mult)
        val high   = meanV + (stddev * mult)
        if (DEBUG) println (s"(low, high) = ($low, $high)")
        for (i <- v.range if v(i) < low || v(i) > high) Vec (c, i) = Vec.noValue (c)
    } // rmOutliers

} // DistanceOutlier object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Detect outliers in the vector by treating anything that falls outside 10th
 *  or 90th percentile.  Other percentiles may be passed in 'args'.
 */
object QuantileOutlier extends Outlier
{
    private val PERCENTILE    = 0.10                          // default percentile as fraction

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Detect outliers in the column/vector by treating anything that falls outside
     *  'PERCENTILE' or '1-PERCENTILE' percentiles as outliers.  Other percentiles may
     *  be passed in 'args'.
     *  @param c     the column/vector with the possible outliers
     *  @param args  if present, args(0) gives the percentile (e.g., .05, .10, .15, .20, .25)
     */
    def rmOutliers (c: Vec, args: Double*)
    {
        val frac = if (args.length > 0) args(0).toDouble else PERCENTILE
        val v    = Vec.toDouble (c)
        val n    = v.dim
        val cp_v = v.copy; cp_v.sort

        val low  = cp_v ((n * frac).toInt)
        val high = cp_v ((n * (1.0 - frac)).toInt)
        if (DEBUG) println (s"(low, high) = ($low, $high)")
        for (i <- v.range if v(i) < low || v(i) > high) Vec (c, i) = Vec.noValue (c)
    } // rmOutliers

} // QuantileOutlier


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Detect outliers in the vector by treating anything that falls below the 1st Quartile
 *  or above the 3rd Quartile as an Outlier.
 */
object QuartileXOutlier extends Outlier
{
    private val QUARTILE      = 0.25                          // value for quartile as fraction
    private val X_MULTIPLIER  = 1.50                          // default eXpansion multipier

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Detect outliers in the vector by treating anything that falls X times below
     *  the 1st Quartile or above the 3rd Quartile as an Outlier.
     *  @param c     the column/vector with the possible outliers
     *  @param args  if present, args(0) gives the eXpansion multiplier
     */
    def rmOutliers (c: Vec, args: Double*)
    {
        val xmult = if (args.length > 0) args(0).toDouble else X_MULTIPLIER
        val v     = Vec.toDouble (c)
        val n     = v.dim
        val cp_v  = v.copy; cp_v.sort
        val quar1 = cp_v ((n * QUARTILE).toInt)
        val quar3 = cp_v ((n * (1.0 - QUARTILE)).toInt)
        val iQR   = quar3 - quar1                             // inter-quartile range

        val low   = quar1 - iQR * xmult
        val high  = quar3 + iQR * xmult
        if (DEBUG) println (s"(low, high) = ($low, $high)")
        for (i <- v.range if v(i) < low || v(i) > high) Vec (c, i) = Vec.noValue (c)
    } // rmOutliers

} // QuartileXOutlier object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OutlierTest` object is used to test the Outliers Detection techniques
 *  presented in Outliers trait for doubles.
 *  > runMain scalation.columnar_db.OutlierTest
 */
object OutlierTest extends App
{
    import scalation.linalgebra.VectorD

    var p = VectorD ("10.0","20.0","40.0","50.0","5.0","5.0","5.0","5.0","5.0","5.0","-50.0","5.0","5.0")
    val ps = p.copy ()
    banner ("DistanceOutlier")
    println (s"Original: $p")
    DistanceOutlier.rmOutliers (p)
    println (s"After:    $p")

    p = ps.copy ()
    banner ("QuantileOutlier")
    println (s"Original: $p")
    QuantileOutlier.rmOutliers (p)
    println (s"After:    $p")

    p = ps.copy ()
    banner ("QuartileXOutlier")
    println (s"Original: $p")
    QuartileXOutlier.rmOutliers (p)
    println (s"After:    $p")

} // OutlierTest object

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OutlierTest2` object is used to test the Outliers Detection techniques
 *  presented in Outliers trait for integers.
 *  > runMain scalation.columnar_db.OutlierTest2
 */
object OutlierTest2 extends App
{
    import scalation.linalgebra.VectorI

    var p = VectorI ("10","20","40","50","5","5","5","5","5","5","-50","5","5")
    val ps = p.copy ()
    banner ("DistanceOutlier")
    println (s"Original: $p")
    DistanceOutlier.rmOutliers (p)
    println (s"After:    $p")

    p = ps.copy ()
    banner ("QuantileOutlier")
    println (s"Original: $p")
    QuantileOutlier.rmOutliers (p)
    println (s"After:    $p")

    p = ps.copy ()
    banner ("QuartileXOutlier")
    println (s"Original: $p")
    QuartileXOutlier.rmOutliers (p)
    println (s"After:    $p")

} // OutlierTest2 object

