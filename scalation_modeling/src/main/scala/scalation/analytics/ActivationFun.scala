
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Dec 28 12:00:07 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{exp, log, max, tanh}

import scalation.linalgebra.{FunctionM_2M, FunctionV_2V, matrixize, vectorize}
import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math.FunctionS2S
import scalation.plot.Plot

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AFF` class holds an Activation Function Family (AFF).
 *  @param f       the activation function itself
 *  @param fV      the vector version of the activation function
 *  @param fM      the matrix version of the activation function
 *  @param dV      the vector version of the activation function derivative
 *  @param dM      the matrix version of the activation function derivative
 *  @param bounds  the (lower, upper) bounds on the range of the activation function
 */
case class AFF (f: FunctionS2S, fV: FunctionV_2V, fM: FunctionM_2M,
                dV: FunctionV_2V, dM: FunctionM_2M,
                bounds: PairD = null)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ActivationFun` object contains common Activation functions and provides
 *  both scalar and vector versions.
 *  @see en.wikipedia.org/wiki/Activation_function
 *  Convention: fun    activation function (e.g., sigmoid)
 *              funV   vector version of activation function (e.g., sigmoidV)
 *              funM   matrix version of activation function (e.g., sigmoidM)
 *              funDV  vector version of dervivative (e.g., sigmoidDV)
 *              funDM  matrix version of dervivative (e.g., sigmoidDM)
 *----------------------------------------------------------------------------------
 * Supports: id, reLU, lreLU, eLU, tanh, sigmoid, gaussian, softmax
 * Related functions: logistic, logit
 */
object ActivationFun
{
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// id: Identity functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Identity 'id' function at scalar 't'.
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

    val f_id = AFF (id, idV, idM, idDV, idDM)                      // id family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// reLU: Rectified Linear Unit functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Rectified Linear Unit 'reLU' function at scalar 't'.
     *  @param t  the reLU function argument
     */
    def reLU (t: Double): Double = max (0.0, t)

    val reLUV: FunctionV_2V = vectorize (reLU _)                   // vector version
    val reLUM: FunctionM_2M = matrixize (reLUV)                    // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'reLU' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = reLUV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def reLUDV (yp: VectoD): VectoD = yp.map (y => if (y >= 0.0 ) 1.0 else 0.0)

    val reLUDM: FunctionM_2M = matrixize (reLUDV _)                // matrix version

    val f_reLU = AFF (reLU, reLUV, reLUM, reLUDV, reLUDM)          // reLU family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// lreLU: Leaky Rectified Linear Unit functions

    private var a = 0.01             // the lreLU alpha parameter (0, 1] indicating how leaky the function is

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the lreLU 'a' (alpha) parameter for the Leaky Rectified Linear Unit functions.
     *  @param a  the rleLU alpha parameter (0, 1] indicating how leaky the function is
     */
    def setA (a_ : Double)
    {
         if (a > 1.0) println ("setA: the lreLU 'a' (alpha) parameter cannot be greater than 1")
         else a = a_ 
    } // setA

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Leaky Rectified Linear Unit 'lreLU' function at scalar 't'.
     *  @param t  the lreLU function argument
     */
    def lreLU (t: Double): Double = max (a * t, t)

    val lreLUV: FunctionV_2V = vectorize (lreLU _)                 // vector version
    val lreLUM: FunctionM_2M = matrixize (lreLUV)                  // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'lreLU' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = lreLUV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def lreLUDV (yp: VectoD): VectoD = yp.map (y => if (y >= 0.0 ) 1.0 else a)

    val lreLUDM: FunctionM_2M = matrixize (lreLUDV _)              // matrix version

    val f_lreLU = AFF (lreLU, lreLUV, lreLUM, lreLUDV, lreLUDM)    // lreLU family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// eLU: Exponential Linear Unit functions
// @see arxiv.org/pdf/1511.07289.pdf

    private var a2 = 1.0             // the eLU alpha parameter (0, infinity) indicating how leaky the function is

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the eLU 'a2' (alpha) parameter for the Exponential Linear Unit functions.
     *  @param a_  the eLU alpha parameter (0, infinity) indicating how leaky the function is
     */
    def setA2 (a_ : Double)
    {
         a2 = a_ 
    } // setA2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Exponential Linear Unit 'eLU' function at scalar 't'.
     *  @param t  the eLU function argument
     */
    def eLU (t: Double): Double = if (t > 0.0 ) t else a2 * (exp (t) - 1)

    val eLUV: FunctionV_2V = vectorize (eLU _)                     // vector version
    val eLUM: FunctionM_2M = matrixize (eLUV)                      // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for 'eLU' function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = eLUV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def eLUDV (yp: VectoD): VectoD = yp.map (y => if (y > 0.0 ) 1.0 else y + a2)

    val eLUDM: FunctionM_2M = matrixize (eLUDV _)                  // matrix version
 
    val f_eLU = AFF (eLU, eLUV, eLUM, eLUDV, eLUDM)                // eLU family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// tanh: Hyperbolic Tangent functions
  
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

    val f_tanh = AFF (tanh, tanhV, tanhM, tanhDV, tanhDM, (-1, 1))    // tanh family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// sigmoid: Sigmoid functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Sigmoid function at 't'.  This is a special case of
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

    val f_sigmoid = AFF (sigmoid, sigmoidV, sigmoidM, sigmoidDV, sigmoidDM, (0, 1))   // sigmoid family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// gaussian: Gaussian functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Gaussian function at scalar 't'.
     *  @param t  the Gaussian function argument
     */
    def gaussian (t: Double): Double = exp (-t * t)

    val gaussianV: FunctionV_2V = vectorize (gaussian _)           // vector version
    val gaussianM: FunctionM_2M = matrixize (gaussianV)            // matrix version

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for Gaussian function at vector 'yp' where
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

//  val f_gaussain = AFF (guassian, gaussianV, gaussianM, auassianDV, gaussianDM)     // gaussian family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// softmax: Softmax functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the vector of values of the Softmax function applied to vector 'tt'.
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
    /** Compute the derivative vector for Softmax function at vector 'yp' where
     *  'yp' is pre-computed by 'yp = softmaxV (tt)'.
     *  @param yp  the derivative function vector argument
     */
    def softmaxDM (yp: VectoD): MatriD = 
    {
        val z = new MatrixD (yp.dim, yp.dim)
        for (i <- yp.range; j <- yp.range) z(i, j) = if (i == j) yp(i) * (1.0 - yp(j))
                                                     else       -yp(i) * yp(j)
        z
     } // softmaxDM

//   val f_softmax = AFF (softmax, sofmaxV, softmaxM, softmaxDV, softmaxDM)     // softmax family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// logistic: Logistic functions - related function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Logistic function at scalar 't'.
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

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// logit: Logit functions - related function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of the odds (Logit) of an event occurring (e.g., success, 1).
     *  The inverse of the 'logit' function is the standard logistic function
     *  (sigmoid function).
     *  Note, it is not typically used as an activation function
     *  @param p  the probability, a number between 0 and 1.
     */
    def logit (p: Double): Double = log (p / (1.0 - p))

    val logitV: FunctionV_2V = vectorize (logit _)                 // vector version

} // ActivationFun object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
    val lreluf = lreLUV (t);    new Plot (t, lreluf, null, "t vs. lreluf") 
    val eluf   = eLUV (t);      new Plot (t, eluf,   null, "t vs. eluf") 
    val tanhh  = tanhV (t);     new Plot (t, tanhh,  null, "t vs. tanhh") 
    val sigmo  = sigmoidV (t);  new Plot (t, sigmo,  null, "t vs. sigmo")
    val gauss  = gaussianV (t); new Plot (t, gauss,  null, "t vs. gauss")
    val softmo = softmaxV (t);  new Plot (t, softmo, null, "t vs. softmo")

    // Test the vector version of related functions
    val logit  = logitV (p);    new Plot (p, logit,  null, "p vs. logit")
    val logist = logisticV (t); new Plot (t, logist, null, "t vs. logist")

    // Test the vector version of activation function derivatives
    val identD = idDV (ident);          new Plot (t, identD, null, "t vs. identD") 
    val relufD = reLUDV (reluf);        new Plot (t, relufD, null, "t vs. relufD") 
    val lrlufD = lreLUDV (lreluf);      new Plot (t, lrlufD, null, "t vs. lrlufD") 
    val elufD  = eLUDV (eluf);          new Plot (t, elufD, null,  "t vs. elufD") 
    val tanhhD = tanhDV (tanhh);        new Plot (t, tanhhD, null, "t vs. tanhhD") 
    val sigmoD = sigmoidDV (sigmo);     new Plot (t, sigmoD, null, "t vs. sigmoD")
    val gaussD = gaussianDV (gauss, t); new Plot (t, gaussD, null, "t vs. gaussD")
//  val softmD = softmaxDV (softmo);    new Plot (t, softmD, null, "t vs. softmD")

} // ActivationFunTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

