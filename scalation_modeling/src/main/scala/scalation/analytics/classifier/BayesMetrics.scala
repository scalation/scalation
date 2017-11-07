
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhe Jin
 *  @version 1.4
 *  @date    Mon Jul 27 01:27:00 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */
package scalation.analytics.classifier

import scala.math.log

import scalation.linalgebra.VectoI
import scalation.linalgebra.gen.HMatrix5

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesMetrics` trait provides scoring methods.
 */
trait BayesMetrics
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Log-Likelihood for the given Bayesian Network structure and data.
     *  @param vc    the value count
     *  @param vcp1  the value count for parent 1
     *  @param vcp2  the value count for parent 2
     *  @param popX  the population counts
     *  @param k     the number of classes
     *  @param me    the m-estimate value
     */
    def logLikelihood (vc: VectoI, vcp1: VectoI, vcp2: VectoI, popX: HMatrix5 [Int], k: Int, me: Float = me_default): Double =
    {
        var ll = 0.0
        // loop through all features, and all parent configurations
        for (j <- 0 until vc.dim; i <- 0 until k) {
            val me_vc = me / vc(j).toDouble

            for (xp1 <- 0 until vcp1(j); xp2 <- 0 until vcp2(j)) {
                var sum = 0.0
                for (xj <- 0 until vc(j)) sum += popX(i, j, xj, xp1, xp2)
                for (xj <- 0 until vc(j)) ll  += popX(i, j, xj, xp1, xp2) * log ((popX(i, j, xj, xp1, xp2) + me_vc) / (sum + me))
            } // for
        } // for
        ll
    } // logLikelihood

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'AIC' for the given Bayesian Network structure and data.
     *  @param vc    the value count
     *  @param vcp1  the value count for parent 1
     *  @param vcp2  the value count for parent 2
     *  @param popX  the population counts
     *  @param k     the number of classes
     *  @param me    the m-estimate value
     */
    def aic (vc: VectoI, vcp1: VectoI, vcp2: VectoI, popX: HMatrix5 [Int], k: Int, me: Float = me_default): Double =
    {
        var sum = 0.0
        for (j <- 0 until vc.dim) sum += (vc(j) - 1) * (vcp1(j) * vcp2(j))
        logLikelihood (vc, vcp1, vcp2, popX, k, me) - sum
    } // aic

} // BayesMetrics trait

