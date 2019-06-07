
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, VectoD, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NetParam` class bundles parameter weights and biases together.
 *  @param w  the weight matrix
 *  @param b  the optional bias/intercept vector (null => not used)
 */
case class NetParam (w: MatriD, var b: VectoD = null)
{
    if (b == null) b = new VectorD (w.dim2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a copy of the network parameters.
     */
    def copy: NetParam = NetParam (w.copy, b.copy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update/assign 'c' to this.
     */
    def set (c: NetParam) { w.set (c.w); b.set (c.b()) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update/assign '(cw, cb)' to this.
     */
    def set (cw: MatriD, cb: VectoD = null)
    {
        w.set (cw)
        if (cb != null) b.set (cb())
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add and assign the changes to the parameters (weights and biases).
     *  @param c  the change to the parameters
     */
    def += (c: NetParam) { w += c.w; b += c.b }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add and assign the changes to the parameters (weights and biases).
     *  @param cw  the change to the weights
     *  @param cb  the change to the baises
     */
    def += (cw: MatriD, cb: VectoD = null)
    {
        w += cw
        if (cb != null) b += cb
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract and assign the changes to the parameters (weights and biases).
     *  @param c  the change to the parameters
     */
    def -= (c: NetParam) { w -= c.w; b -= c.b }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract and assign the changes to the parameters (weights and biases).
     *  @param cw  the change to the weights
     *  @param cb  the change to the baises
     */
    def -= (cw: MatriD, cb: VectoD = null)
    {
        w -= cw
        if (cb != null) b -= cb
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply matrix 'x' by the weight matrix 'w' and add the bias vector 'b'.
     *  @param x  the matrix to multiply by
     */
    def * (x: MatriD): MatriD = x * w + b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply vector 'x' by the weight matrix 'w' and add the bias vector 'b'.
     *  @param x  the vector to multiply by
     */
    def dot (x: VectoD): VectoD = (w dot x) + b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this `NetParam` object to a string showing this ('b') parameter's
     *  weight matrix 'b.w' and bias vector 'b.b'.
     */
    override def toString: String = s"b.w = $w \n b.b = $b"

} // NetParam

