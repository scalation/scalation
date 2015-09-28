
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu May 12 11:56:13 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.{abs, cos}

import scalation.random.Random

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BatchVector` class contains a `StatVector` for maintaining experimental
 *  data in multiple batches and methods for computing statistics on these data.
 *  Ex:  It can be used to support the Method of Batch Means (MBM).
 *  @param name       name of the batch statistic
 *  @param _bSize     size of each batch
 *  @param _nBatches  number of batches
 */
class BatchVector (name: String, private var _bSize: Int = 10, private var _nBatches: Int = 10)
{ 
    /** The vector containing all the elements from all the batches.
     *  FIX: with more clever coding the 'y' vector would not be necessary
     */
    private var y = new StatVector (bSize * nBatches)

    /** The vector containing the means from all the batches.
     */
    private var yb = new StatVector (nBatches)

    /** The index in the y vector of the next element to add.
     */
    private var next = 0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the total length of the batched vector.
     */
    def len = _nBatches * _bSize 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the batch size.
     */
    def bSize = _bSize

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the number of batches.
     */
    def nBatches = _nBatches

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate additional batches for this batched vector.
     *  @param more  the number of additional batches to allocate
     */
    def allocBatches (more: Int = 1) { y.expand (more * _bSize); _nBatches += more }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute means for each batch and store them in the 'yb' stat vector.
     */
    def computeMeans ()
    {
        for (i <- 0 until nBatches) yb(i) = y(i * _bSize until (i+1) * _bSize-1).sum / bSize.toDouble
    } // computeMeans

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the given value in the next index position in the batched vector.
     *  @param value  the given value to add
     */
    def tally (value: Double)
    {
        if (next == y.dim) { y = y.expand (); _bSize += _bSize }     // double bSize
        y(next) = value
        next += 1
    } // tally

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the lag 1 autocorrelation of the batch means.  Be sure to run
     *  'computeMeans' first.
     */
    def acorr: Double = yb.acorr ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the batches are sufficiently uncorrelated.
     *  @param threshold  the cut-off value to be considered uncorrelated
     */
    def uncorrelated (threshold: Double = .2) = abs (acorr) <= threshold

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the relative precision, i.e., the ratio of the confidence interval
     *  half-width and the mean.
     *  @param p  the confidence level
     */
    def precision (p: Double = .95): Double = yb.precision ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the Confidence Interval (CI) on the mean is tight enough.
     *  @param precision  the cut-off value for CI to be considered tight
     */
    def precise (threshold: Double = .2, p: Double = .95) = yb.precise (threshold, p)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the batched vector into a string showing each of the batches.
     */
    override def toString =
    {
        var s = name
        for (i <- 0 until nBatches) {
            s += "\nBatch_" + i + ": \t" + y(i * _bSize until (i+1) * _bSize-1)
        } // for
        s
    } // toString

} // BatchVector class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BatchVectorTest` object provides an example of how to use the `BatchVector`
 *  class to implement the Method of Batch Means (MBM).
 */
object BatchVectorTest extends App
{
    val bat  = new BatchVector ("ResponseTime")
    var from = 0
    var till = bat.len
    val rng  = Random ()

    // increase the batch size until the batches are sufficiently uncorrelated
    do {
        for (i <- from until till) bat.tally (75.0 * rng.gen + 12.5 * (1.0 + cos (i/10.0)))
        from = till
        till += till
        bat.computeMeans ()
        println ("acorr for " + bat.nBatches + " batches of size " + bat.bSize + " = " + bat.acorr)
    } while (! bat.uncorrelated ())

    // increase the number of batches until the confidence interval is tight enough
    while (! bat.precise ()) {
        from = till
        till += bat.bSize
        bat.allocBatches ()
        for (i <- from until till) bat.tally (75.0 * rng.gen + 12.5 * (1.0 + cos (i/10.0)))
        bat.computeMeans ()
        println ("precision for " + bat.nBatches + " batches of size " + bat.bSize + " = " + bat.precision ())
    } // while

    println ("final precision for " + bat.nBatches + " batches of size " + bat.bSize + " = " + bat.precision ())

} // BatchVectorTest object

