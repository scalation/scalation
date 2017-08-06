
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Mustafa Nural, John Miller
 *  @version 1.3
 *  @date    Fri Jul 24 14:35:58 EDT 2015
 *  @see     Sun Jul 30 15:56:21 EDT 2017
 *
 *  @see     www.itl.nist.gov/div898/handbook/eda/section3/eda353.htm
 */

package scalation.stat

import scala.math.{abs, sqrt}

import scalation.math.double_exp
import scalation.linalgebra.VectorD
import scalation.random.CDF.studentTCDF
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TTest` class is used to test whether the means, 'μ1' and 'μ2', of two
 *  data samples, 'x' and 'y', are different, 'μ1 ≠ μ2', using a Two-Sample,
 *  Independent t-test.  Assumes samples are drawn from Normal distributions.
 *  The error in the test is measured by the conditional probability 'p' that
 *  a "difference is detected" when there "actually is none".
 *  <p>
 *     p = P(different | μ1 = μ2)
 *  <p>
 *  The power of the test is the ability to detect actual differences.  '1 - power' is
 *  measured by the conditional probability 'q' that a "difference is not detected"
 *  when there "actually is one".
 *  <p>
 *     q = P(! different | μ1 ≠ μ2)
 *  <p>
 *  These are called type I (measured by p) and type II (measured by q) errors.
 *  @see en.wikipedia.org/wiki/Type_I_and_type_II_errors
 *-----------------------------------------------------------------------------
 *  @param x       the first sample's vector of data 
 *  @param y       the second sample's vector of data
 *  @param pooled  whether the variances can be assumed to be the same (hence pooled)
 */
class TTest (x: VectorD, y: VectorD, pooled: Boolean = true)
{
    private val DEBUG = true                              // debug flag
    private val n1    = x.dim                             // sample size for x
    private val n2    = y.dim                             // sample size for y
    private val n1_   = n1 - 1                            // sample size for x less one (df1)
    private val n2_   = n2 - 1                            // sample size for y less one (df2)
    private val dfp   = n1_ + n2_                         // pooled degrees of freedom

    private val μ1    = x.mean                            // sample mean of x
    private val μ2    = y.mean                            // sample mean of y
    private val diff  = abs (μ1 - μ2)                     // absolute difference in sample means

    private val sv1   = x.variance                        // sample variance of x
    private val sv2   = y.variance                        // sample variance of y
    private val sv_μ1 = sv1 / n1                          // sample variance of mean of x
    private val sv_μ2 = sv2 / n2                          // sample variance of mean of y
    private val svp   = (n1_ * sv1 + n2_ * sv2) / dfp     // pooled sample variance

    if (DEBUG) {
        println (s"μ1 = $μ1")
        println (s"μ2 = $μ2")
        println (s"diff = $diff")
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the standard error for mean difference.
     */
    def se: Double =
    {
        if (pooled) {
            sqrt (svp) * sqrt (1.0 / n1 + 1.0 / n2)
        } else {
            sqrt (sv_μ1 + sv_μ2)
        } // if
    } // se

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Student's t statistic.
     */
    def t (se: Double): Double = diff / se

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the effective degrees of freedom:  For pooled, it is the sum of
     *  the two degrees of freedom of the samples after their means are estimated.
     *  <p>
     *      df = df1 + df2 = n1 + n2 - 2
     *  <p>
     *  while for unequal variances, it is the scaled weighted harmonic mean of
     *  the two degrees of freedom.
     *  @see stats.stackexchange.com/questions/116511/
     *       explanation-for-non-integer-degrees-of-freedom-in-t-test-with-unequal-variances/116556#116556
     */
    def df: Double =
    {
        if (pooled) dfp
        else        (sv_μ1 + sv_μ2)~^2 / (sv_μ1~^2 / n1_ + sv_μ2~^2 / n2_)
    } // df

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability of a type I error using the Student's t distribution.
     *  @see scalation.random.CDF.studentTCDF
     *  @param t   the value of the Student's t statistic
     *  @param df  the effective degrees of freedom
     */
    def p (t: Double, df: Double): Double = 2.0 * (1.0 - studentTCDF (t, df))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability of a type II error using the Noncentral t distribution.
     *  @see scalation.random.CDF.noncentralTCDF
     *  @param t   the value of the Student's t statistic
     *  @param df  the effective degrees of freedom
     */
    def q (t: Double, df: Double): Double =
    {
        throw new UnsupportedOperationException ("method q in TTest not yet implemented")   // FIX
    } // q

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the difference in the two means is statistically
     *  significant.
     *  @param p  the probability of a type I error
     *  @param α  the desired sigificance level
     */
    def different (p: Double, α: Double = 0.05): Boolean = p <= α

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the difference in the two means is statistically
     *  insignificant.  Note, typically this is a weaker test than 'different'.
     *  @param q  the probability of a type II error
     *  @param β  the desired power level (β  = 1 - power)
     */
    def same (q: Double, β: Double = 0.1): Boolean = q <= β

} //  TTest class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TTest` companion object provides a convenient method for performing t-test.
 */
object TTest
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a t-test on two samples to detect if their means differ. 
     *  @param x       the first sample's vector of data 
     *  @param y       the second sample's vector of data
     *  @param pooled  whether the variances can be assumed to be the same (hence pooled)
     */
    def test (x: VectorD, y: VectorD, pooled: Boolean = true)
    {
        if (pooled) banner ("t-test - pooled")
        else        banner ("t-test - unequal variance")

        val ttest = new TTest (x, y, pooled)    // construct a t-test

        val se   = ttest.se                     // standard error
        val t    = ttest.t (se)                 // Student's t statistic
        val df   = ttest.df                     // degrees of freedom
        val p    = ttest.p (t, df)              // probability 
        val diff = ttest.different (p)          // are the means different?

        println (s"TTest se   = $se")
        println (s"TTest t    = $t")
        println (s"TTest df   = $df")
        println (s"TTest p    = $p")
        println (s"TTest diff = $diff")
    } // test

} // TTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TTestTest` object is used to test the `TTest` class.
 *  @see www.isixsigma.com/tools-templates/hypothesis-testing/making-sense-two-sample-t-test/
 *  > run-main scalation.stat.TTestTest
 */
object TTestTest extends App
{
    val x = VectorD (20.4, 24.2, 15.4, 21.4, 20.2, 18.5, 21.5)
    val y = VectorD (20.2, 16.9, 18.5, 17.3, 20.5)

    TTest.test (x, y)                          // equal variance case (pooled)
    TTest.test (x, y, false)                   // unequal variance case

} // TTestTest object

