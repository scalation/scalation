
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Mar  4 14:56:27 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes ServiceNetwork.scala
 *  @run     scala -cp ../../classes:classes optimization.ServiceNetwork
 */

package apps.optimization

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.minima.Simplex

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Optimize the service network by solving the following Linear Programming (LP)
 *  Problem.
 */
object ServiceNetworkOpt extends App
{
    // Linear Program:  min c x  s.t.  a x <= b, x >= 0
    //
    //  x = [x_i y_i z_i]            x1,   x2,   x3,   x4,   x5,   x6,   y1,   y2,   y3,   y4,   y5,   z1,   z2
    val a = new MatrixD ((16, 13), -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,   // row 1
                                    0.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  2.0,  0.0,   // row 2
                                    0.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  3.0,  0.0,   // row 3
                                    0.0,  0.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  2.0,   // row 4
                                    0.0,  0.0,  0.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  3.0,   // row 5
                                    0.0,  0.0,  0.0,  0.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  4.0,   // row 6
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0, -1.0,  0.0,   // row 7
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0, -1.0,  0.0,   // row 8
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0, -1.0,  0.0,   // row 9
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0, -1.0,   // row 10
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0, -1.0,   // row 11
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,   // row 12
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0,   // row 13
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0,   // row 14
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0,   // row 15
                                    0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0)   // row 16
     val c = VectorD              (11.0, 12.0, 13.0, 14.0, 15.0, 16.0,-81.0,-82.0,-83.0,-84.0,-85.0,  1.0,  2.0)

     val b = VectorD (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 6.0, 4.0, 3.0, 4.0)


     val lp = new Simplex (a, b, c)        // solve the above LP using the Simplex Algorithm
    
     val x = lp.solve ()                   // find a minima for decision vector
     val f = lp.objF (x)                   // find a minima for objective function

     println ("x = " + x)                  // optimal values for variables x = [x_i, y_i, z_i]
     println ("f = " + f)                  // optimal value for objective function (- profit)

/** Solution:
    x = VectorD (6.0	12.0	18.0	6.0	9.0	12.0	5.0	6.0	4.0	3.0	3.0	6.0	3.0)
    f = -868.9999999999999
*/

} // ServiceNetworkOpt

