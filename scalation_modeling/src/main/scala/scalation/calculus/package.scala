
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat Oct  8 14:15:14 EDT 2016
 *  @see     LICENSE (MIT style license file)
 */

package scalation

import scala.language.implicitConversions

import scalation.math.FunctionS2S

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `calculus` package contains classes with methods for computing
 *  derivatives, gradient vectors, Jacobian matrices, integrals and basic
 *  operators in Functional Analysis.
 */
package object calculus
{ 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from 'FunctionS2S' to 'Hilbert', which supports 
     *  functional operators.
     *  @param f  the function to turn into a Hilbert function
     */
    implicit def functionS2S2Hilbert (f: FunctionS2S) = new Hilbert (f)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Zero function.
     */
    def _0f (x: Double): Double = 0.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** One function.
     */
    def _1f (x: Double): Double = 1.0

} // calculus package object

