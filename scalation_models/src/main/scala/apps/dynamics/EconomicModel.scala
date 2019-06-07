
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Sep 25 22:39:34 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  www.princeton.edu/~dixitak/Teaching/ArtOfModeling.pdf
 */

package apps.dynamics

import scalation.linalgebra.VectorI
import scalation.math.{double_exp}
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EconomicModel` class provides a simple model of can economy.
 *  @param n   the ???
 *  @param l0  the labor capacity at time 't0'
 *  @param k0  the capital capacity at time 't0'
 *  @param f   the production function
 *  @param g   the growth function
 */
class EconomicModel (n: Int, l0: Int, k0: Int, f: Int => Int, g: Int => Int)
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative/difference of the production function 'f'.
     *  @param x  the current value
     */
    def df(x: Int) = f(x) - f(x-1)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the labor capacity/number of young (workers) at time 't'
     *  @param t  the current time
     */
    def l(t: Int): Int = if (t <= 0) l0 else l0 * (1 + n) * t

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the output per worker: y(t) = f(k(t)) where f(k) is the production
     *  function.
     *  @param t  the current time
     */
    def y(t: Int): Int = f(k(t))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the marginal product of capital reflected as an interest rate.
     *  May be viewed as a derivative/difference 'fk(k(t), l(t)) = fʹ(k(t))'.
     *  @param t  the current time
     */
    def r(t: Int) = df(k(t))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the wage.
     *  @param t  the current time
     */
    def w(t: Int) = f(k(t)) - k(t) * r(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the personal consumption amount when young (wages - savings).
     *  @param t  the current time
     */
    def cY(t: Int) = w(t) - s(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the personal savings amount when the person is young.
     *  @param t  the current time
     */
    def s(t: Int) = h(r(t))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the savings function based on wages and interest rate.
     *  @param rt  the interest rate at time t+1
     */
    def h(rt: Int) = (1 + n) * g(rt)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the total earned return when the person is old.
     *  @param t  the current time
     */
    def e(t: Int) = (1 + r(t) + 1) * (w(t) - cY(t))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the consumption amount when the person is old.  Bequests are ignored,
     *  so this equals his/her consumption in the next period when old by.
     *  Thus, the intertemporal budget constraint is relates cO to cY.
     *  @param t  the current time
     */
    def cO(t: Int) = (1 + r(t)) * (w(t-1) - cY(t-1))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the amount of productive capital at time 't'.  Initially k(t) = k0'.
     *  Subsequently, it is paid for by workers: There are 'l(t)' workers and total
     *  savings must finance the capital next period.
     *  @param t  the current time
     */
    def k(t: Int): Int = if (t <= 0) k0 else l(t-1) * s(t-1)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the capital per worker.
     *  @param t  the current time
     */
    def kp(t: Int) = k(t) / l(t)

} // EconomicModel class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EconomicModelTest` object is used to test the `EconomicModel` class.
 *  > runMain apps.dynamics.EconomicModelTest
 */
object EconomicModelTest extends App
{
    val n  = 1
    val l0 = 10
    val k0 = 10
    val a  = 2
    val θ  = 0.9
    def f(k: Int) = (a * k.toDouble~^θ).toInt
    def g(k: Int) = (θ * a * k.toDouble~^θ).toInt

    val em = new EconomicModel (n, l0, k0, f, g)

    val tv = VectorI.range (0, 100)
    val lv = new VectorI (100)
    val kv = new VectorI (100)
    for (t <- tv) {
        lv(t) = em.l(t)
        kv(t) = em.k(t)
        println (s"(t, l, k) = ($t, ${lv(t)}, ${kv(t)})")
    } // for
    new Plot (tv.toDouble, lv.toDouble, kv.toDouble)

} // EconomicModelTest

