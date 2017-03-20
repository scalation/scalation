
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Oct 24 18:32:24 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This trait specifies the pattern for Line Search (LS) algorithms that perform
 *  line search on f(x) to find an x-value that minimizes a function f.
 */
trait LineSearch
{
    protected val EPSILON = 1E-7             // number close to zero

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform Line Search (LS) using a line search algorithm.
     *  @param step  the initial step size
     */
    def search (step: Double): Double

} // LineSearch trait

