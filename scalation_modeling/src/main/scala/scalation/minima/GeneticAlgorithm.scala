
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Wed Nov  9 22:07:41 EST 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

// U N D E R   D E V E L O P M E N T

import scala.math.max
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.VectorI
import scalation.random.Randi

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GeneticAlgorithm` class performs local search to find minima of functions
 *  defined on integer vector domains (z^n).
 *
 *  minimize    f(x)
 *  subject to  g(x) <= 0, x in Z^n
 *
 *  @param x0       the starting point for the search (seed for GA)
 *  @param f        the objective function to be minimize (f maps an integer vector to a double)
 *  @param g        the constraint function to be satisfied, if any
 *  @param maxStep  the maximum/starting step size (make larger for larger domains)
 */
class GeneticAlgorithm (f:       VectorI => Double,
                        x0:      VectorI,
                        vMax:    Int = 100,
                        g:       VectorI => Double = null,
                        maxStep: Int = 5)
{
    /** Pair consisting of an integer vector and its functional value (a double)
     */
    type Vec_Func = Tuple2 [VectorI, Double]

    /** Debug flag
     */
    private val DEBUG = true

    /** Weight on penalty for constraint violation
     */
    private val WEIGHT = 1000

    /** Maximum number of iterations allowed in total (4 maxStep^2)
     */
    private val TOLERANCE = .001

    /** Maximum number of generations
     */
    private val MAX_GEN = 30

    /** Size of the population
     */
    private val POP_SIZE = 10

    /** The current population
     */
    private val pop = new Array [VectorI] (POP_SIZE)

    /** Random number generator for selecting an individual (represented by vector x)
     */
    private val rIndiv = Randi (0, POP_SIZE - 1)

    /** Random number generator for value for an individual (value for x(k))
     */
    private val rVal = Randi (0, vMax)

    /** Random number generator for an index position for an individual (index k)
     */
    private val rIndex = Randi (0, x0.dim - 1)

    /** The fittest individual
     */
    private var best = (x0, fg(x0))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function f re-scaled by a weighted penalty, if constrained.
     *  @param x  the coordinate values of the current point
     */
    def fg (x: VectorI): Double =
    {
        if (g == null) {                  // unconstrained
            f(x)
        } else {                          // constrained, g(x) <= 0
            val penalty = max (g(x), 0.0)
            f(x) * (1.0 + WEIGHT * penalty * penalty)
        } // if
    } // fg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the current population
     */
    def printPopulation ()
    {
        for (i <- 0 until POP_SIZE) println (pop(i))
    } // printPopulation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate an initial population of individuals.
     */
    def genPopulation ()
    {
        for (i <- 0 until POP_SIZE) {
            val pop_i = new VectorI (x0.dim)
            for (k <- 0 until x0.dim) pop_i(k) = rVal.igen
            pop(i) = pop_i
        } // for
    } // genPopulation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the fittest individual (smallest value of objective function).
     */
    def fittest: Vec_Func =
    {
        best = (pop(0), fg(pop(0)))
        for (i <- 1 until POP_SIZE) {
            val x   = pop(i)
            val f_x = fg(x)
            if (f_x < best._2) best = (x, f_x)
        } // for
        if (DEBUG) println ("fittest: best = " + best)
        best
    } // fittest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For each individual in the population, cross it with some other individual.
     */
    def crossOver ()
    {
        pop(0) = best._1                  // keep the fittest individual
        for (i <- 1 until POP_SIZE) {
            var j = rIndiv.igen
            if (i != j) {
                val k = rIndex.igen
                pop(i) = pop(i)(0 until k) ++ pop(j)(k until x0.dim) 
            } // if
        } // for
    } // crossOver

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For each individual in the population, cross it with some other individual.
     *  Let the crossover be dependent of the fitness of the individual.
     */
    def fitnessCrossOver ()
    {
        // FIX - implement
    } // fitnessCrossOver

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly select individuals for mutation (change a value at one position).
     */
    def mutate ()
    {
        for (i <- 0 until POP_SIZE / 2) {
            var j = rIndiv.igen              // random individual to mutate
            if (j == 0) j = 1                // don't mutate the fittest individual
            val k = rIndex.igen              // random index position to change
            pop(j)(k) = rVal.igen            // random value
        } // for
    } // mutate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the minimization problem using a genetic algorithm.
     */
    def solve (): Vec_Func =
    {
        var noImprovement = 0
        genPopulation ()
        var best = fittest
        breakable { for (g <- 1 to MAX_GEN) {
            println ("solve: starting generation " + g)
            crossOver ()
            mutate ()
            printPopulation
            val cand = fittest
            if (cand._2 + TOLERANCE < best._2) {
                best = cand
                noImprovement = 0
            } else {
                noImprovement += 1
            } // if
            if (noImprovement == 10) break 
        }} // for
        best
    } // solve

} // GeneticAlgorithm


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GeneticAlgorithmTest` object is used to test the `GeneticAlgorithm` class
 *  (unconstrained).
 */
object GeneticAlgorithmTest extends App
{
    def f (x: VectorI): Double = (x(0) - 10) * (x(0) - 10) + (x(1) - 20) * (x(1) - 20) + 1
    val x0  = new VectorI (2)
    val ga = new GeneticAlgorithm (f, x0)
    println ("optimal solution = " + ga.solve)

} // GeneticAlgorithmTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GeneticAlgorithmTest2` object is used to test the `GeneticAlgorithm` class
 *  (constrained).
 */
// object GeneticAlgorithmTest2 extends App
// {
//     def f (x: VectorI): Double = (x(0) - 10) * (x(0) - 10) + (x(1) - 20) * (x(1) - 20) + 1
//     def g (x: VectorI): Double = -max (x(0) - 1, x(1) - 1)    // require x(i) >= 1
//     val x0  = new VectorI (2); x0.set (1)
//     val ils = new GeneticAlgorithm (f, g)
//     println ("optimal solution = " + ils.solve (x0))
//
// } // GeneticAlgorithmTest2 object

