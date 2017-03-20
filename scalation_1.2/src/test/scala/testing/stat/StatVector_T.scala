
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Mar 29 16:54:57 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing.stat

import org.junit.Test

import scala.math._

import scalation.linalgebra.VectorD
import scalation.random._
import scalation.stat.vectorD2StatVector

import testing.Tester

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVector_T` driver class conducts unit testing on the `StatVector` class
 *  by invoking the StatVector_T testing object.  Run 'test-only' to test `StatVector`
 *  or 'test' to run all unit tests.
 *------------------------------------------------------------------------------
 *  > test-only testing.stat.StatVector_T
 *  > test
 */
class StatVector_T { @Test def testAll () { StatVector_T } }


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVector_T` testing object conducts unit testing on the `StatVector` class
 *  using the `Tester` trait.  It compares correctness/performance of a method/operator
 *  'call' to an 'oracle' and optionally a 'contender'.
 *------------------------------------------------------------------------------
 *  All methods except 'this', 'apply', 'update', 'foreach' and 'hashCode' should be tested.
 *  May skip '=op' if 'op' is tested, e.g., skip '+=' if '+' is tested.
 *  Also the 'equals' and 'toString' are tested implicitly.
 *  Depending on the 'CORRECT' flag, it will either test correctness or performance.
 *  Note, if the code for the 'contender' or even the 'oracle' is significantly faster,
 *  the method/operator may need be to re-coded.
 *------------------------------------------------------------------------------
 *  To run directly, uncomment "// with App" and run 'test:runMain'.
 *  > test:runMain testing.stat.StatVector_T
 */
object StatVector_T extends Tester // with App
{
    // Reassign parameters from `Tester` trait as needed

    DEBUG   = false                                                // debug flag
    CORRECT = true                                                 // test correctness/performance
    FOCUS   = ""                                                   // method/operator to focus on, "" => all
    KLASS   = "StatVector"                                         // the class under test
    ITER    = 100                                                  // number of test iterations

    // Size parameter(s) used for variables in 'test' (customize per class)

    private val dim = 10                                           // vector size

    // Random variate generators (customize per class)

    private val rv  = RandomVecD (dim)                             // random vector generator
    private val rn  = Uniform (0.0, 100.0)                         // random double generator
    private val rj  = Randi0 (0, dim - 1)                          // random integer/index generator

    // Variables used in 'test' (customize per class)

    private val x   = new VectorD (dim)                            // first vector
    private val y   = new VectorD (dim)                            // second vector
    private var s   = 0.0                                          // scalar value
    private var j   = 0                                            // first integer/index value
    private var k   = 0                                            // second integer/index value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomize all variables used in `Tester`s 'test' method.
     */
    def randomize ()
    {
        x set rv.gen ()                                            // randomly reset variables
        y set rv.gen ()
        s = rn.gen
        j = rj.igen
        k = rj.igen
    } // randomize

    testClass ()

    println ("\nTest no argument methods/unary operators")
 
    test ("stddev",    x.stddev,
                       sqrt (x.variance))

    println ("\nTest methods/operators that take parameters")

} // StatVector_T object

