
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Jan  5 14:10:45 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     reference.wolfram.com/mathematica/tutorial/ConstrainedOptimizationLocalNumerical.html
 */

package apps.optimization

import math.{abs, sqrt}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.stat.StatVector
import scalation.minima.ConjugateGradient

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PortfolioExOpt` object solves the following Quadratic Programming 'QP' problem:
 *  Given 'n' investment instruments, find a combination/portfolio that
 *  minimizes risk for a given expected return.
 *  > run-main apps.optimization.PortfolioExOpt
 */
object PortfolioExOpt extends App
{
     val label = Array ("3m Tbill", "long Tbond", "SP500", "Wilt.5000", "Corp. Bond", "NASDQ", "EAFE", "Gold")

     val r =  new MatrixD ((22, 8),
	1.075,	0.942,	0.852,	0.815,	0.698,	1.023,	0.851,	1.677,
	1.084,	1.02,	0.735,	0.716,	0.662,	1.002,	0.768,	1.722,
	1.061,	1.056,	1.371,	1.385,	1.318,	0.123,	1.354,	0.76,
	1.052,	1.175,	1.236,	1.266,	1.28,	1.156,	1.025,	0.96,
	1.055,	1.002,	0.926,	0.974,	1.093,	1.03,	1.181,	1.2,
	1.077,	0.982,	1.064,	1.093,	1.146,	1.012,	1.326,	1.295,
	1.109,	0.978,	1.184,	1.256,	1.307,	1.023,	1.048,	2.212,
	1.127,	0.947,	1.323,	1.337,	1.367,	1.031,	1.226,	1.296,
	1.156,	1.003,	0.949,	0.963,	0.99,	1.073,	0.977,	0.688,
	1.117,	1.465,	1.215,	1.187,	1.213,	1.311,	0.981,	1.084,
	1.092,	0.985,	1.224,	1.235,	1.217,	1.08,	1.237,	0.872,
	1.103,	1.159,	1.061,	1.03,	0.903,	1.15,	1.074,	0.825,
	1.08,	1.366,	1.316,	1.326,	1.333,	1.213,	1.562,	1.006,
	1.063,	1.309,	1.186,	1.161,	1.086,	1.156,	1.694,	1.216,
	1.061,	0.925,	1.052,	1.023,	0.959,	1.023,	1.246,	1.244,
	1.071,	1.086,	1.165,	1.179,	1.165,	1.076,	1.283,	0.861,
	1.087,	1.212,	1.316,	1.292,	1.204,	1.142,	1.105,	0.977,
	1.08,	1.054,	0.968,	0.938,	0.83,	1.083,	0.766,	0.922,
	1.057,	1.193,	1.304,	1.342,	1.594,	1.161,	1.121,	0.958,
	1.036,	1.079,	1.076,	1.09,	1.174,	1.076,	0.878,	0.926,
	1.031,	1.217,	1.1,	1.113,	1.162,	1.11,	1.326,	1.146,
	1.045,	0.889,	1.012,	0.999,	0.968,	0.965,	1.078,	0.99)

/*
	90,  	60,  	90, 	
	90, 	90, 	30,
	60, 	60, 	60,
	60, 	60,	90,
	30, 	30, 	30)
*/

    val mean = new VectorD (r.dim2)
    for (j <- 0 until r.dim2) mean(j) = r.col(j).sum / r.dim1
    val cova = new MatrixD (r.dim2, r.dim2)
    for (j <- 0 until r.dim2) {
        val x = new StatVector (r.col(j))
        for (k <- 0 until r.dim2) cova (j, k) = x.cov (r.col(k))
    } // for
    println ("mean = " + mean)
    println ("cova = " + cova)

    val x0 = new VectorD (r.dim2); x0.set (1.0 / r.dim2)
//  def f (x: VectorD) = (cova * x) dot x
//  def g (x: VectorD) = abs((mean dot x) - 1.25) + abs(x.sum - 1.) 

    def f (x: VectorD) = -(mean dot x) / sqrt((cova * x) dot x) 
    def g (x: VectorD) = (x.sum - 1.0) + (1.0 - x.sum)

    val solver = new ConjugateGradient (f, g, false)
    var x = solver.solve (x0)

    println ("optimal solution x = " + x + " with an objective value f(x) = " + f(x))

    //average	1.078	1.093	1.120	1.124	1.121	1.046	1.141 	1.130

} // PortfolioExOpt

