
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Khalid Jahangeer, John Miller
 *  @version 1.5
 *  @date    Fri Aug  10 20:26:34 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.recommender

import scala.math.{abs, round, sqrt}

import scalation.linalgebra.{MatrixD, MatrixI, VectorD, VectorI}
import scalation.stat.Statistic
import scalation.util.Sorting

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Recommender` trait serves as a template for recommender algorithms.
 */
trait Recommender
{
    private val DEBUG      = false                                // debug flag
    private val MAX_RATING = 5                                    // maximum allow score
    private val s_diff     = new Statistic ()                     // statistic to tally the actual difference between scores
    private val s_rdiff    = new Statistic ()                     // statistic to tally the rounded difference between scores
    private val s_hit      = new Statistic ()                     // statistic to tally the number of correct predictions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert an original 4-column 'input' integer matrix (i, j, value, timestamp) into
     *  a two-dimensional 'ratings' double matrix with 'm' rows and 'n' columns.
     *  The 'input' matrix has type `MatrixI`, while the 'ratings' matrix has type `MatrixD`.
     *  @param input  the original 4-column input data matrix containing ratings, e.g., from a file
     *  @param m      the number of rows for the ratings matrix
     *  @param n      the number of columns for the ratings matrix
     */
    def makeRatings (input: MatrixI, m: Int, n: Int): MatrixD =
    {
        val ratings = new MatrixD (m, n)
        for (k <- input.range1) ratings(input(k, 0), input(k, 1)) = input(k, 2)
        ratings
    } // makeRatings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the final rating for a given '(i, j)' cell, e.g.,  (user, item).
     *  @param i  the ith row, e.g., user
     *  @param j  the jth column, e.g., item
     */
    def rate (i: Int, j: Int): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Phase 3: Test the accuracy of the predictions and add it to the statistics vector.
     *  @param istart  the start point
     *  @param iend    the end point
     *  @param input   the original 4-column input matrix
     */
    def test (istart : Int, iend: Int, input: MatrixI)
    {
        for (i <- istart until iend) {
            var p = rate (input(i, 0), input(i, 1))               // predicted rating
            val a = input(i, 2).toDouble                          // actual rating
            if (DEBUG) print (s"for ($a, $p): a - p = ${a - p} \t")
            if (p != 0.0) {
                if (p > 0.0 && p < 1.0) p = 1.0                   // since ratings must be {1, 2, ... MAX_RATING}
                if (p > MAX_RATING) p = MAX_RATING.toDouble 
                s_diff.tally (a - p)
                s_rdiff.tally (a - round(p))
                s_hit.tally (if (a - round(p) == 0.0) 1.0 else 0.0)
            } // if
        } // for
    }// test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Phase 2: Cross validate the final predictions against the test dataset.
     *  @param tester  testing data matrix
     */
    def crossValidate (tester: MatrixD)
    {
        for (i <- tester.range1; j <- tester.range2){
            if (tester(i, j) != 0.0){
                val a = tester(i, j)
                var p = rate (i, j)
                if (DEBUG) print (s"($a, $p) ${a-p} \t")
                if (p != 0.0) {
                    s_diff.tally (a - p)
                    s_rdiff.tally (a - round(p))
                    s_hit.tally (if (a - round(p) == 0.0) 1.0 else 0.0)
                } // if
            } // if
        } // for
        println ()
    } //crossValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Phase 1: Print MAE and RMSE metrics based on the final predictions for
     *  the test dataset.
     *  @param input  the test portion of the original 4-column input matrix
     */
    def error_metrics (input: MatrixI)
    {
        var sum1, sum2, sum3, sum4 = 0.0
        for (i <- input.range1) {
            val a = input(i, 2).toDouble
            val p = rate (input(i, 0), input(i, 1))
            if (DEBUG) print (s"Predicted = $p, actual = $a; \t")
            if (! p.isNaN) {
                sum1 += abs (a - p)                               // non rounded MAE
                sum2 += abs (a - round (p))                       // rounded MAE
                sum3 += (a - p) * (a - p)                         // non rounded RMSE
                sum4 += (a - round (p)) * (a - round (p))         // rounded RMSE
            } // if
        } //for
        println ("MAE  Non-Rounded = " + sum1 / input.dim1)
        println ("MAE      Rounded = " + sum2 / input.dim1)
        println ("RMSE Non-Rounded = " + sqrt (sum3 / input.dim1))
        println ("RMSE     Rounded = " + sqrt (sum4 / input.dim1))
    } // error metrics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the variables for the statistics vectors.
     */
    def getStats = Array (s_diff, s_rdiff, s_hit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the indices of the 'k' largest values in vector 'x'.
     *  FIX - replace with more efficient top-k algorithm
     *  @param x  the input vector
     *  @param k  the number of values to be returned
     */
    def topk (x: VectorD, k: Int): VectorI =
    {
        val x_arr   = x.copy ()()                      // FIX - why make a copy
        val indices = new VectorI (x.dim)
        val zipidx  = x_arr.zipWithIndex.sorted
        var count   = 0
        for ((num, index) <- zipidx) {
            indices(count) = index
            count += 1
        } // for
        indices.slice (x.dim - k, x.dim)
    } //topk


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the indices of the 'k' largest values in vector 'x'.
     *  @param x  the input vector
     *  @param k  the number of values to be returned
     */
    def topk2 (x: VectorD, k: Int): Array [Int] = (new Sorting (x.toArray)).iselsort (k, false)

} // Recommender trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RecommenderTest` is used to test the `Recommender` trait.
 *  > runMain scalation.analytics.recommender.RecommenderTest
 */
object RecommenderTest extends App with Recommender
{
    def rate (i: Int, j: Int): Double = -1.0

    val x = VectorD (2.2, 4.4, 1.1, 3.3, 5.5)
    println (topk (x, 3))
    println (topk2 (x, 3).deep)

} // RecommenderTest object

