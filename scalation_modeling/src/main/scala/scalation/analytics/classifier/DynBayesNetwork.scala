
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Tue Sep 13 17:37:39 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math.log

import scalation.linalgebra.{MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.random.ProbabilityVec

// U N D E R   D E V E L O P M E N T

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DynBayesNetwork` class provides Dynamic Bayesian Network (DBN) models.
 */
class DynBayesNetwork
      extends Classifier
{
    def train (testStart: Int, testEnd: Int): Unit = ???
    def classify (z: VectoD): (Int, String, Double) = ???
    def classify (z: VectoI): (Int, String, Double) = ???
    def reset () {}
    def size: Int = ???
    def test (testStart: Int, testEnd: Int): Double = ???

} // DynBayesNetwork class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DynBayesNetworkTest` object is used to test the `DynBayesNetwork` class.
 *  > run-main scalation.analytics.classifier.DynBayesNetworkTest
 */
object DynBayesNetworkTest extends App
{
    println ("run DynBayesNetworkTest")

} // DynBayesNetworkTest object

