
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sat Aug 13 15:35:14 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.pow

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.random.Quantile

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Anova class provides the functions necessary for performing a one-way
 *  ANalysis Of VAriance (ANOVA) on the input data matrix x.
 *  The data matrix: row i    - i-th group/treatment
 *                   column j - j-th replica
 *  @param x  the data matrix
 */
class Anova (x: MatrixD)
{
    private val m   = x.dim1                    // m rows (groups)
    private val n   = x.dim2                    // n columns (replicas)
    private val md  = m.toDouble                // m as a Double
    private val nd  = n.toDouble                // n as a Double
            val dfg = md - 1.0                  // degrees of freedom, group
            val dfe = md * nd - md              // degrees of freedom, error
            val mui = mu_i                      // vector of group means
            val gmu = g_mu                      // grand mean

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the group means.
     */
    def mu_i: VectorD =
    {
        val c = new VectorD (m)
        for (i <- 0 until m) c(i) = x(i).sum / nd
        c
    } // mu_i

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the grand mean.
     */
    def g_mu = mui.sum / md
        
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the between-groups sum of squares.
     */
    def ssg = nd * (mui - gmu).sq.sum

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the within-groups (error) sum of squares.
     */
    def sse = 
    {
        var sum = 0.0
        for (i <- 0 until m) sum += (x(i) - mui(i)).sq.sum
        sum
    } // sse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the total sum of squares.
     */
    def sst = ssg + sse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the mean square, groups
     */
    def msg = ssg / dfg

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the mean square, error
     */
    def mse = sse / dfe
        
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the F-statistic.
     */
    def f_stat = msg / mse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the critcal value from the Fisher (F) Distribution.
     */
    def f_crit = Quantile.fisherInv (.95, (dfg.toInt, dfe.toInt))
        
} // Anova class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Anova class.
 *  @see http://wiki.uiowa.edu/display/bstat/One-way+fixed+effects+ANOVA
 */
object AnovaTest extends App
{
    val data = new MatrixD ((3, 4), 52.0, 48.0, 46.0, 35.0,
                                    41.0, 50.0, 44.0, 37.0,
                                    51.0, 32.0, 40.0, 31.0)
    val an = new Anova (data)

    println ("ANOVA data matrix = " + data)
    println ("mui    = " + an.mui)
    println ("gmu    = " + an.gmu)
    println ("ssg    = " + an.ssg)
    println ("sse    = " + an.sse)
    println ("sst    = " + an.sst)
    println ("dfg    = " + an.dfg)
    println ("dfe    = " + an.dfe)
    println ("msg    = " + an.msg)
    println ("mse    = " + an.mse)
    println ("f_stat = " + an.f_stat)
    println ("f_crit = " + an.f_crit)

} // AnovaTest object

