
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Santosh Uttam Bobade, Vinay Kumar Bingi, John Miller
 *  @version 1.6
 *  @date    Sat Jun 9 14:09:25 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.columnar_db

import scalation.linalgebra.{Vec, Vec_Elem, VectorD}
import scalation.math.noDouble
import scalation.random.Normal

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Imputation` trait specifies an imputation operation to be defined by
 *  the objects implementing it, i.e.,
 *      `Interpolate`          - Uses Linear Interpolation technique to impute the missing value
 *      `ImputeMean`           - Estimates the mean of the vector filtered from missing values
 *      `ImputeNormal`         - Estimates the mean and variance of the vector filtered from missing values
 *      `ImputeMovingAverage`  - Imputes the missing value in the vector by taking historical Moving Average
 *                               of last 'n' seasonal values.
 */
trait Imputation
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for a missing value in column/vector 'c'
     *  @param c      the column/vector with missing values
     *  @param args   an optional paramter which takes variable no. of numeric values,
     *                args(0) - index at which missing value needs to be imputed
     *                args(1) - distance around index to consider when imputing from subset of values
     *                args(2) - season 'period' to consider for taking historical average
     */
    def impute (c: Vec, args: Int*): Any

} // Imputation trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Interpolate` object imputes the missing value in the vector using Linear Interpolation
 *  based on the previous and next values.
 */
object Interpolate extends Imputation
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for a missing value in column/vector 'c' using Linear Interpolation
     *  based on the previous and next values.
     *  @param c     the column/vector with missing values
     *  @param args  an optional paramter which takes variable no. of numeric values,
     *               args(0) - index at which missing value needs to be imputed
     *               args(1) - no. of values around index 'i' to consider for interpolation
     */
    def impute (c: Vec, args: Int*): Any =
    {
        val cd = Vec.toDouble (c)
        val n = args.length
        val i = if (n >= 1) args(0) else 0       // impute at index 0, by default
        val d = if (n >= 2) args(1) else 1       // interpolate using 1 prev. and next value, by default
        var sum = 0.0
        var count = 0
        for (k <- 1 to d if i >= k) { sum += cd(i-k); count += 1; }
        var j = 0                                // count of missing value places to skip forward
        for (k <- 1 to d) {
            while (i+j+k < c.size && Vec(c, i+j+k).equals (Vec.noValue (c))) j += 1
            if (i+j+k < c.size) { sum += cd(i+j+k); count += 1;}
        } // for
        Vec.fromDouble (c, sum / count)
    } // impute

} // Interpolate object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeMean` object uses the mean of the vector filtered of missing values
 *  as estimates for missing values.
 */
object ImputeMean extends Imputation
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for a missing value in column/vector 'c' using the filtered mean.
     *  @param c     the column/vector with missing values
     *  @param args  an optional paramter which is not required for this imputation method
     */
    def impute (c: Vec, args: Int*): Any = 
    {
        val cf = Vec.filterMissing (c)                // filter out missing values
        Vec.fromDouble (c, Vec.mean (cf))             // the filtered mean
    } // impute

} // ImputeMean object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeNormal` object uses a Normal distribution estimated from the filtered
 *  mean and variance as estimates for missing values
 */
object ImputeNormal extends Imputation
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for a missing value in column/vector 'c' using a Normally distributed
     *  random value.  Compute the mean and variance of the filtered column/vector.
     *  @param c     the column/vector with missing values
     *  @param args  an optional paramter which is not required for this imputation method
     */
    def impute (c: Vec, args: Int*): Any =
    {
        val cf  = Vec.filterMissing (c)                // filter out missing values
        val mu  = Vec_Elem.toDouble (Vec.mean (cf))     
        val sig = Vec.toDouble (cf).variance           // convert to VectorD to prevent integer underflow
        val rn  = Normal (mu, sig)                     // Normal random variate generator
        Vec.fromDouble (c, rn.gen)                     // random value of appropriate type
    } // impute

} // ImputeNormal


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeMovingAverage` object imputes the missing value in the vector by taking
 *  the historical Moving Average of last 'n' seaonal values.
 */
object ImputeMovingAverage extends Imputation
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute a value for a missing value in column/vector 'c' using a historical
     *  Moving Average of the last 'n' seasonal values.
     *  @param c     the column/vector with the missing values
     *  @param args  an optional paramter which takes variable no. of numeric values,
     *               args(0) - index at which missing value needs to be imputed
     *               args(1) - no. of values before index 'i' to consider for taking historical average
     *               args(2) - seaonal 'period' value for the vector 'c'
     */
    def impute (c: Vec, args: Int*): Any =
    {
        val cd = Vec.toDouble (c)
        val n = args.length
        val i = if (n >= 1) args(0) else 0
        val d = if (n >= 2) args(1) else 3
        val period = if (n >= 3) args(2) else 1
        var sum = 0.0
        var count = 0
        for (k <- 1 to d if i >= k * period) {
            sum += cd(i - (k * period)); count += 1
        } // for
        Vec.fromDouble (c, sum / count)
    } // impute

} // ImputeMovingAverage object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImputeNormalWin` object imputes the missing values in the vector using
 *  Normal Distribution for a sliding window.
 */
object ImputeNormalWin
{
    import scalation.util.CircularQueue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute all the missing values in vector 'c' using Normal Distribution for
     *  a sliding window.
     *  @param n  size of the sliding window
     */
    def imputeAll (c: Vec, args: Int*): VectorD =
    {
        val n          = if (args.length > 0) args(0) else 5
        val queue      = new CircularQueue [Double](n)
        val result     = new VectorD (c.size)
        var isFullOnce = false
        var sumSq      = 0.0
        var sum        = 0.0

        for (i <- c.indices) {
            if (Vec (c,i) equals noDouble) {
                val mean = sum / n
                var variance = (sumSq - sum * sum / n) / (n-1.0)
                if (variance < 0) variance = 0.0
                val normal = Normal (mean, variance)
                result(i)  = normal.gen
            } else {
                result(i) = Vec (c, i).asInstanceOf [Double]
            } // if
            if (i == n) isFullOnce = true
            tally (result(i))
            queue += result(i)
        } // for

        def tally (x: Double)
        {
            if (isFullOnce) {
                val first = queue.dequeue
                sumSq -= (first*first)
                sum -= first
            } // if
            sumSq += (x * x)
            sum += x
            sum = math.max(sum,0.0)
            sumSq = math.max(sumSq,0.0)
        } // tally

        result
    } // imputeAll

} // ImputeNormalWin object

