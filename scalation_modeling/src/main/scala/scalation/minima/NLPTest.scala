
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Oct  2 22:43:44 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import scala.math.pow

import scalation.linalgebra.VectorD
import scalation.math.FunctionV2S

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NLPTest1` object used to test several Non-Linear Programming (NLP) algorithms
 *  on unconstrained problems.
 *  Algorithms:
 *      'sdcs' - Gradient Descent with Custom Line Search
 *      'sdgs' - Gradient Descent with Golden Section Line Search
 *      'prcg' - Polak-Ribiere Conjugate Gradient with Golden Section Line Search
 *      'sdws' - Gradient Descent with Wolfe Line Search
 *      'bfgs' - Broyden–Fletcher–Goldfarb–Shanno with Wolfe Line Search
 */
object NLPTest1 extends App
{
    println ("NLPTest")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the NLP algorithms on objective function f.
     *  @param f  the objective function to minimize 
     *  @param n  the dimensionality of the problem
     */
    def test (f: FunctionV2S, n: Int)
    {
        val x0   = new VectorD (n)          // zero vector
        var x    = x0

        val sdgs = new GradientDescent (f)
        x = sdgs.solve (x0)
        println ("sdgs: optimal solution x = " + x + " with an objective value f(x) = " + f(x))

        val prcg = new ConjugateGradient (f)
        x = prcg.solve (x0)
        println ("prcg: optimal solution x = " + x + " with an objective value f(x) = " + f(x))

        val sdws = new QuasiNewton (f); sdws.setSteepest ()
        x = sdws.solve (x0)
        println ("sdws: optimal solution x = " + x + " with an objective value f(x) = " + f(x))

        val bfgs = new QuasiNewton (f)
        x = bfgs.solve (x0)
        println ("bfgs: optimal solution x = " + x + " with an objective value f(x) = " + f(x))
    } // test

    def test1 ()
    {
        println ("\nMinimize f(x)  = (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
        def f (x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
        test (f, 2)
    } // test1

    def test2 ()
    {
        println ("\nMinimize f(x)  = (x_0 - 30)^2 + (x_1 - 40)^2 + 1")
        def f (x: VectorD): Double = (x(0) - 30.0) * (x(0) - 30.0) + (x(1) - 40.0) * (x(1) - 40.0) + 1.0
        test (f, 2)
    } // test2

    def test3 ()
    {
        println ("\nMinimize f(x)  = x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
        def f (x: VectorD): Double = pow (x(0), 4.0) + (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
        test (f, 2)
    } // test3

    // @see http://math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
    def test4 ()
    {
        println ("\nMinimize f(x)  = x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
        def f (x: VectorD): Double = x(0)/4.0 + 5.0*x(0)*x(0) + pow(x(0),4) -
                                     9.0*x(0)*x(0)*x(1) + 3.0*x(1)*x(1) + 2.0*pow(x(1),4)
        test (f, 2)
    } // test4

    test1 ()
    test2 ()
    test3 ()
    test4 ()

} // NLPTest1 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NLPTest2` object used to test several Non-Linear Programming (NLP) algorithms
 *  on constrained problems.
 */
object NLPTest2 extends App
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the NLP algorithms on objective function f with constraint function g.
     *  @param f  the objective function to minimize 
     *  @param g  the constraint function to be satisfied
     *  @param n  the dimensionality of the problem
     */
    def test (f: FunctionV2S, g: FunctionV2S, n: Int)
    {
        val x0   = new VectorD (n)          // zero vector
        var x    = x0

        val sdcs = new GradientDescent (f)
        x = sdcs.solve (x0)
        println ("sdgs: optimal solution x = " + x + " with an objective value f(x) = " + f(x))

        val prcg = new ConjugateGradient (f, g)
        x = prcg.solve (x0)
        println ("prcg: optimal solution x = " + x + " with an objective value f(x) = " + f(x))

        val sdws = new QuasiNewton (f, g); sdws.setSteepest ()
        x = sdws.solve (x0)
        println ("sdws: optimal solution x = " + x + " with an objective value f(x) = " + f(x))

        val bfgs = new QuasiNewton (f, g)
        x = bfgs.solve (x0)
        println ("bfgs: optimal solution x = " + x + " with an objective value f(x) = " + f(x))
    } // test

    def test1 ()
    {
        println ("\nMinimize   f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1" +
                 "\nSubject to g(x) = x_0 <= 1")
        def f (x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
        def g (x: VectorD): Double = x(0) - 1.0
        test (f, g, 2)
    } // test1

    test1 ()

} // NLPTest2

