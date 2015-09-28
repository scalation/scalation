
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Oct 22 17:15:33 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

// U N D E R   D E V E L O P M E N T

package apps.optimization

import math.{min, sqrt}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math._
import scalation.minima.ConjGradient
import scalation.stat.vectorD2StatVector
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to solve Portfilio Optimization Problems.
 *  @param r      the return matrix as in revenue/profit
 *  @param label  the label vector
 */
class PortfolioOpt (r: MatrixD, label: Array [String])
      extends Error
{
    private val m      = r.dim1               // number of time points
    private val n      = r.dim2               // the number of instruments
    private val r_mean = new VectorD (n)      // mean vector
    private val r_cov  = new MatrixD (n, n)   // covariance matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate basis statistics (mean and covariance).
     */
    def calcStats ()
    {
        for (j <- 0 until n) {
            val r_j = r.col(j)                      // data for the j-th instrument
            r_mean(j) = r_j.mean                    // mean for instrument j
            for (k <- 0 until n) {
                r_cov(j, k) = r_j cov r.col(k)      // covariance for instrument j with k
            } // for
        } // for
        println ("r_mean = " + r_mean)
        println ("r_cov  = " + r_cov)
    } // calcStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal solution to the portfolio optimization problem, i.e., find
     *  a vector x, indicating to fraction of each instrument to invest in that
     *  that minimizes the risk.
     */
    def opt (): Tuple2 [VectorD, Double] =
    {
        val x0 = new VectorD (r.dim2); x0.set (1.0 / r.dim2)   // initial guess, all equal

        // negativity function
        def neg (x: VectorD): Double = 
        { 
            var sum = 0.0
            for (i <- 0 until x.dim) sum += min (0.0, x(i))
            sum
        } // neg

        // objective function
        def f (x: VectorD) = -(r_mean dot x) / sqrt((r_cov * x) dot x)

        // constraint function
        // require the vector to sum to 1 and each element to be non-negative
        def g (x: VectorD) = (1.0 - x.sum)~^2 + neg (x)~^2

        val solver = new ConjGradient (f, g, true)        // quadratic optimizer
        val x   = solver.solve (x0, .1)                   // optimal x vector
        val f_x = f(x)                                    // optimal objective function value
        (x, f_x)                                          // return optimal (x, f(x))
    } // opt

} // PortfolioOpt class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the PortfolioOpt class.
 *  @see http://www.math.uni-magdeburg.de/~girlich/preprints/preprint0906.pdf
 */
object PortfolioOptTest extends App
{
    val labels = Array ("Blue Chips", "Hi-Tech Shares", "Real Estate Market", "Treasury Bonds")

    val r = new MatrixD ((6, 4), 18.24,  12.24, 8.23, 8.12,
                                 12.12,  19.16, 8.96, 8.26,
                                 15.23,  35.07, 8.35, 8.34,
                                  5.26,  23.46, 9.16, 9.01,
                                  2.62, -10.62, 8.05, 9.11,
                                 10.42,  -7.43, 7.29, 8.95)

    val portOpt = new PortfolioOpt (r, labels)
    portOpt.calcStats ()
    val res = portOpt.opt ()

    println ("optimal solution x = " + res._1 + " with an objective value f(x) = " + res._2)

} // PortfolioOptTest object

