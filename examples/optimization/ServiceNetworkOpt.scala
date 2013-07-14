
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Mar  4 14:56:27 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes ServiceNetwork.scala
 *  @run     scala -cp ../../classes:classes optimization.ServiceNetwork
 */

package optimization

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.minima.Simplex2P

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Optimize the service network by solving the following Linear Programming
 *  Problem.
 */
object ServiceNetworkOpt extends App
{
    // Linear Program:  min c*x s.t. a*x <= b, x >= 0
    //
    //  x = [x_i y_i z_i]           x1,  x2,  x3,  x4,  x5,  x6,  y1,  y2,  y3,  y4,  y5,  z1,  z2
    val a = new MatrixD ((16, 13), -1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,   // row 1
                                    0., -1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  2.,  0.,   // row 2
                                    0.,  0., -1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  3.,  0.,   // row 3
                                    0.,  0.,  0., -1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  2.,   // row 4
                                    0.,  0.,  0.,  0., -1.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  3.,   // row 5
                                    0.,  0.,  0.,  0.,  0., -1.,  0.,  0.,  0.,  0.,  0.,  0.,  4.,   // row 6
                                    0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,  0.,  0., -1.,  0.,   // row 7
                                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,  0., -1.,  0.,   // row 8
                                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0., -1.,  0.,   // row 9
                                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0., -1.,   // row 10
                                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0., -1.,   // row 11
                                    0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,  0.,  0.,   // row 12
                                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,  0.,   // row 13
                                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,  0.,  0.,   // row 14
                                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.,  0.,   // row 15
                                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  1.,  0.,  0.)   // row 16

     val b = new VectorD (0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 5., 6., 4., 3., 4.)

     val c = new VectorD (11., 12., 13., 14., 15., 16., -81., -82., -83., -84., -85., 1., 2.)

     val lp = new Simplex2P (a, b, c)
    
     lp.solve2P ()                         // find a minima

     println ("opt = " + lp.primal)        // optimal values for variables x, y, z
     println ("obj = " + lp.objective)     // optimal value for objective function (- profit)

/** Solution:
    opt = VectorD (6.0	12.0	18.0	6.0	9.0	12.0	5.0	6.0	4.0	3.0	3.0	6.0	3.0)
    obj = -868.9999999999999
*/

} // ServiceNetworkOpt

