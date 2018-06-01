
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Dec 28 12:00:07 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{exp, log, max, tanh}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math._
import scalation.plot.Plot

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ActivationFun` object contains common Activation functions and provides
 *  both scalar and vector versions.
 *  @see en.wikipedia.org/wiki/Activation_function
 *  Convention: fun    activation function (e.g., sigmoid)
 *              funV   vector version of activation function (e.g., sigmoidV)
 *              funM   matrix version of activation function (e.g., sigmoidM)
 *              funDV  vector version of dervivative (e.g., sigmoidDV)
 *              funDM  matrix version of dervivative (e.g., sigmoidDM)
 *------------------------------------------------------------------------------
 * Supports: id, reLU, tanh, sigmoid, gaussain, softmax
 * Related functions: logistic, logit
 */
object ActivationFun
{
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // id: identity functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the identity 'id' function at scalar 't'.
     *  @param t  the id function argument
     */
    def id (t: Double): Double = t

    def idV (tt: VectoD): VectoD = tt                              // vector version
    def idM (tt: MatriD): MatriD = tt                              // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'id' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = idV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def idDV (yp: VectoD): VectoD = VectorD.one (yp.dim)

    val idDM: FunctionM_2M = matrixize (idDV _)                    // matrix version

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // reLU: Rectified linear unit functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the identity 'reLU' function at scalar 't'.
     *  @param t  the id function argument
     */
    def reLU (t: Double): Double = max (0.0, t)

    val reLUV: FunctionV_2V = vectorize (reLU _)                   // vector version
    val reLUM: FunctionM_2M = matrixize (reLUV)                    // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'id' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = idV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def reLUDV (yp: VectoD): VectoD = yp.map (y => if (y >= 0.0 ) 1.0 else 0.0)

    val reLUDM: FunctionM_2M = matrixize (reLUDV _)                // matrix version

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // tanh: hyperbolic tangent functions
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Compute the value of the 'tanh' function at scalar 't'.
     *  @param t  the tanh function argument
     */
    //  @see scala.math.tanh

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the vector of values of the 'tanh' function applied to vector 'tt'.
     *  @param tt  the tanh function vector argument
     */
    def tanhV (tt: VectoD): VectoD = tt.map (t => tanh (t))

    val tanhM: FunctionM_2M = matrixize (tanhV _)                  // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'tanh' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = tanhV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def tanhDV (yp: VectoD): VectoD = VectorD.one (yp.dim) - yp~^2

    val tanhDM: FunctionM_2M = matrixize (tanhDV _)                // matrix version

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // sigmoid: sigmoid functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the 'sigmoid' function at 't'.  This is a special case of
     *  the logistic function, where 'a = 0' and 'b = 1'.  It is also referred to as
     *  the standard logistic function.  It is also the inverse of the logit function.
     *  @param t  the sigmoid function argument
     */
    def sigmoid (t: Double): Double = 1.0 / (1.0 + exp (-t))

    val sigmoidV: FunctionV_2V = vectorize (sigmoid _)             // vector version
    val sigmoidM: FunctionM_2M = matrixize (sigmoidV)              // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'sigmoid' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = sigmoidV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def sigmoidDV (yp: VectoD): VectoD = yp * (VectorD.one (yp.dim) - yp)

    val sigmoidDM: FunctionM_2M = matrixize (sigmoidDV _)          // matrix version

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // logistic: logistic functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the 'logistic' function at scalar 't'.
     *  With the default settings, it is identical to 'sigmoid'.
     *  Note, it is not typically used as an activation function
     *  @see www.cs.xu.edu/math/math120/01f/logistic.pdf
     *  @param t  the logistic function argument
     *  @param a  the shift parameter (1 => mid at 0, <1 => mid shift left, >= mid shift right
     *  @param b  the spread parameter (1 => sigmoid rate, <1 => slower than, >1 => faster than)
     *            althtough typically positive, a negative b will cause the function to decrease
     *  @param c  the scale parameter (range is 0 to c)
     */
    def logistic (t: Double, a: Double = 1.0, b: Double = 1.0, c: Double = 1.0): Double =
    {
        c / (1.0 + a * exp (-b*t))
    } // logistic

    def logisticV (tt: VectoD, a: Double = 1.0, b: Double = 1.0, c: Double = 1.0): VectoD = 
    {
        tt.map (t => c / (1.0 + a * exp (-b*t)))
    } // logisticV

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // logit: logit functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of the odds of an event occurring (e.g., success, 1).
     *  The inverse of the 'logit' function is the standard logistic function
     *  (sigmoid function).
     *  Note, it is not typically used as an activation function
     *  @param p  the probability, a number between 0 and 1.
     */
    def logit (p: Double): Double = log (p / (1.0 - p))

    val logitV: FunctionV_2V = vectorize (logit _)                 // vector version

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // gaussian: gaussian functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the 'gaussian' function at scalar 't'.
     *  @param t  the gaussian function argument
     */
    def gaussian (t: Double): Double = exp (-t * t)

    val gaussianV: FunctionV_2V = vectorize (gaussian _)           // vector version
    val gaussianM: FunctionM_2M = matrixize (gaussianV)            // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'gaussian' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = gaussianV (tt)'.
     *  @param yp  the derivative function vector argument
     *  @param tt  the domain value for the function
     */
    def gaussianDV (yp: VectoD, tt: VectoD): VectoD = tt * yp * -2.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative matrix for 'sigmoid' function at matrix 'yp' where
     *  'yp' is pre-computed by 'yp = gaussianM (tt)'.
     *  @param yp  the derivative function vector argument
     *  @param tt  the domain value for the function
     */
    def gaussianDM (yp: MatriD, tt: MatriD): MatriD = 
    {
        MatrixD (for (i <- yp.range1) yield tt(i) * yp(i) * -2.0)
    } // gaussianDM

    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    // softmax: softmax functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the vector of values of the 'softmax' function applied to vector 'tt'.
     *  @see https://en.wikipedia.org/wiki/Softmax_function
     *  Note, scalar function version is not needed.
     *  @param tt  the softmax function vector argument
     */
    def softmaxV (tt: VectoD): VectoD = 
    {
        val et  = tt.map (exp (_))
        val sum = et.sum
        et.map (_ / sum)
    } // softmaxV

    val softmaxM: FunctionM_2M = matrixize (softmaxV _)            // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'softmax' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = softmaxV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def softmaxDM (yp: VectoD): MatriD = 
    {
        val z = new MatrixD (yp.dim, yp.dim)
        for (i <- yp.range; j <- yp.range) z(i, j) = if (i == j) yp(i) * (1.0 - yp(j))
                                                     else        -yp(i) * yp(j)
        z
     } // softmaxDM

} // ActivationFun object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ActivationFunTest` is used to test the `ActivationFun` object.
 *  > runMain scalation.analytics.ActivationFunTest
 */
object ActivationFunTest extends App
{
    import ActivationFun._

    val t = VectorD.range (-30, 30) / 6.0
    val p = VectorD.range (1, 59) / 60.0

    // Test the vector version of activation functions
    val ident  = idV (t);       new Plot (t, ident,  null, "t vs. ident") 
    val reluf  = reLUV (t);     new Plot (t, reluf,  null, "t vs. reluf") 
    val tanhh  = tanhV (t);     new Plot (t, tanhh,  null, "t vs. tanhh") 
    val sigmo  = sigmoidV (t);  new Plot (t, sigmo,  null, "t vs. sigmo")
    val gauss  = gaussianV (t); new Plot (t, gauss,  null, "t vs. gauss")
    val softmo = softmaxV (t);  new Plot (t, softmo, null, "t vs. softmo")

    // Test the vector version of related functions
    val logit  = logitV (p);    new Plot (p, logit,  null, "p vs. logit")
    val logist = logisticV (t); new Plot (t, logist, null, "t vs. logist")

    // Test the vector version of activation function derivatives
    val identD = idDV (ident);          new Plot (t, identD, null, "t vs. identD") 
    val relufD = reLUDV (ident);        new Plot (t, relufD, null, "t vs. relufD") 
    val tanhhD = tanhDV (tanhh);        new Plot (t, tanhhD, null, "t vs. tanhhD") 
    val sigmoD = sigmoidDV (sigmo);     new Plot (t, sigmoD, null, "t vs. sigmoD")
    val gaussD = gaussianDV (gauss, t); new Plot (t, gaussD, null, "t vs. gaussD")
//  val softmD = softmaxDV (softmo);    new Plot (t, softmD, null, "t vs. softmD")

} // ActivationFunTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ActivationFunTest2` is used to test the `ActivationFun` object.
 *  @see en.wikipedia.org/wiki/Softmax_function
 *  > runMain scalation.analytics.ActivationFunTest2
 */
object ActivationFunTest2 extends App
{
    import ActivationFun.softmaxV

    val t = VectorD (1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0)
    println (s"softmaxV ($t) = \n ${softmaxV (t)}")

} // ActivationFunTest2

