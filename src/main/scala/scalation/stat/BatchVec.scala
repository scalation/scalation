
/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Thu May 12 11:56:13 EDT 2011
 * @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.abs

import scalation.linalgebra.VectorD

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** This class contains a Random Vector (RVec) for maintaining experimental
 *  data in multiple batches and methods for computing statistics on these data.
 *  Ex: It may be used to implement the Method of Batch Means (MBM) in simulation.
 *  @param name       name of the batch statistic
 *  @param _bSize     size of each batch
 *  @param _nBatches  number of batches
 */
class BatchVec (name: String, private var _bSize: Int = 10, private var _nBatches: Int = 10)
{ 
    /** The vector containing all the elements from all the batches.
      */
    private var x = new VectorD (bSize * nBatches, null)  // FIX: change to RVec

    /** The index in the x vector of the next element to add.
      */
    private var next = 0

    private var ac = .4   // FIX: remove after fixing autoCorr

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Get the total length of the batched vector.
     */
    def len = _nBatches * _bSize 

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Get the batch size.
     */
    def bSize = _bSize

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Get the number of batches.
     */
    def nBatches = _nBatches

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Allocate additional batches for this batched vector.
     *  @param more  the number of additional batches to allocate
     */
    def allocBatches (more: Int = 1) { x.expand (more * _bSize); _nBatches += more }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Add the given value in the next index position in the batched vector.
     *  @param value  the given value to add
     */
    def tally (value: Double)
    {
        if (next == x.dim) { x = x.expand (); _bSize += _bSize }   // double bSize
        x(next) = value
        next += 1
    } // tally

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Compute the average autocorrelation between all of the batches.
     *  FIX: implement correctly using Rvec methods
     */
    def autoCorr = { ac -= .1; ac }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Determine if the batches are sufficiently uncorrelated.
     *  @param threshold  the cut-off value to be considered uncorrelated
     */
    def uncorrelated (threshold: Double = .2) = abs (autoCorr) <= threshold

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Determine if the Confidence Interval (CI) on the grand mean is tight enough.
     *  FIX: implement correctly using Rvec methods
     *  @param precision  the cut-off value for CI to be considered tight
     */
    def precise (precision: Double = .2) = true // x.interval / x.mean <= precision

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Convert the batched vector into a string showing each of the batches.
     */
    override def toString =
    {
        var s = name
        for (i <- 0 until nBatches) {
            s += "\nBatch_" + i + ": \t" + x(i * _bSize until (i+1) * _bSize - 1)
        } // for
        s
    } // toString

} // BatchVec class


/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** This object tests the BatchVec class.
 */
object BatchVecTest extends App
{
    val rt   = new BatchVec ("ResponseTime")
    var from = 0
    var till = rt.len

    // increase the batch size until the batches are sufficiently uncorrelated
    do {
        for (i <- from until till) rt.tally (i)
        from = till
        till += till
        println (rt)
    } while (! rt.uncorrelated ())

    // increase the number of batches until the confidence interval is tight enough
    while (! rt.precise ()) {
        from = till
        till += rt.bSize
        rt.allocBatches ()
        for (i <- from until till) rt.tally (i)
        println (rt)
    } // while

} // BatchVecTest object

