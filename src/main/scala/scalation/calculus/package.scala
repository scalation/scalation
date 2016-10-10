
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
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

} // calculus package object

