
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Fri Jul 24 14:35:58 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation

/** The `random` package contains classes, traits and objects for
 *  the generation of random numbers.
 */
package object random
{
    /** Type definition for parameters to a distribution. `Vector` is used instead
     *  of `Array` since they are covariant, while Scala arrays are not.
     */
    type Parameters = Vector [Double]

    /** The function type for distribution functions, including
     *  (1) Cumulative Distribution Function (CDF)
     *  (2) inverse Cumulative Distribution Function (iCDF)
     *  The arguments are `Double` for coordinate 'x' or probability 'p' and a
     *  `Vector` of parameters, e.g., degrees of freedom.
     */
    type Distribution = (Double, Parameters) => Double

} // random package object 

