
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Sep 14 20:47:08 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://en.wikipedia.org/wiki/Canonical_correlation
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{Eigenvalue, Eigenvector, MatrixD, VectorD}
import scalation.linalgebra.MatrixD.outer
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CanCorrelation` class performs Canonical Correlation Analysis 'CCA'
 *  on two random vectors.  Samples for the first one are stored in the 'x'
 *  data matrix and samples for the second are stored in the 'y' data matrix.
 *  Find vectors a and b that maximize the correlation between x * a and y * b.
 *  <p>
 *      max {rho (x * a, y * b)}
 *  <p>
 *  Additional vectors orthogonal to a and b can also be found.
 *  @param x  the x data matrix
 *  @param y  the y data matrix
 */
class CanCorrelation (x: MatrixD, y: MatrixD)
      extends Reducer with Error                   // FIX - reducer or predictor?
{
    if (! x.isRectangular) flaw ("constructor", "x matrix must be rectangular")
    if (! y.isRectangular) flaw ("constructor", "y matrix must be rectangular")

    private val MAX_ITER = 200                               // the maximum number of iterations
    private val EPSILON  = 1E-8                              // a value close to zero

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reduce the original data ...
     */
    def reduce (): MatrixD =
    {
        null    // FIX - to be implemented
    } // reduce

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Approximately recover the original data ...
     */
    def recover (): MatrixD =
    {
        null    // FIX - to be implemented
    } // recover

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reduce the original data ...
     */
    def reduce2 (): Tuple2 [MatrixD, MatrixD] =
    {
        (null, null)    // FIX - to be implemented
    } // reduce

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Approximately recover the original data ...
     */
    def recover2 (): Tuple2 [MatrixD, MatrixD] =
    {
        (null, null)    // FIX - to be implemented
    } // recover

} // CanCorrelation class

