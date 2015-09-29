
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Jul 24 14:35:58 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation

/** The `random` package contains classes, traits and objects for
 *  the generation of random numbers.
 */
package object random
{
    /** The function type for distribution functions, including
     *  (1) Cumulative Distribution Function (CDF)
     *  (2) inverse Cumulative Distribution Function (iCDF)
     *  The parameters are Double for coordinate 'x' or probability 'p'
     *  and a array of degrees of freedom.
     */
    type Distribution = (Double, Array [Int]) => Double

} // random package object 

