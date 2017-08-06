
package scalation

/** The `minima` package contains classes, traits and objects for
 *  optimization to find minima.
 */
package object minima
{
    object FunctionSelector extends Enumeration
    {
        type FunctionSelector = Value
        val Function, Derivative, FiniteDifference = Value
    } // FunctionSelector object

} // minima package object 

