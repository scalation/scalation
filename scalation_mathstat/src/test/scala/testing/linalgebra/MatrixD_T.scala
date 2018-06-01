
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Apr 10 13:27:53 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing.linalgebra

import org.junit.Test

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.random.{Randi0, RandomMatD}

import testing.Tester

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixD_T` driver class conducts unit testing on the `MatrixD` class
 *  by invoking the `MatrixD_T` testing object.  Run 'test-only' to test `MatrixD`
 *  or 'test' to run all unit tests.
 *------------------------------------------------------------------------------
 *  > test-only testing.linalgebra.MatrixD_T
 *  > test
 */
class MatrixD_T { @Test def testAll () { MatrixD_T } }


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixD_T` object conducts unit testing on the `MatrixD` class using the
 *  `Tester` trait.  It compares correctness/performance of a method/operator 'call'
 *  to an 'oracle' and optionally a 'contender'.
 *------------------------------------------------------------------------------
 *  All methods except 'this', 'apply', 'update', 'foreach' and 'hashCode' should be tested.
 *  May skip '=op' if 'op' is tested, e.g., skip '+=' if '+' is tested.
 *  Also the 'equals' and 'toString' are tested implicitly.
 *  Depending on the 'CORRECT' flag, it will either test correctness or performance.
 *  Note, if the code for the 'contender' or even the 'oracle' is significantly faster,
 *  the method/operator may need be to re-coded.
 *------------------------------------------------------------------------------
 *  To run directly, uncomment "// with App" and run 'test:runMain'.
 *  > test:runMain testing.linalgebra.MatrixD_T
 */
object MatrixD_T extends Tester // with App
{
    // Reassign parameters from `Tester` trait as needed

    DEBUG   = false                                                // debug flag
    CORRECT = true                                                 // test correctness/performance
    FOCUS   = ""                                                   // method/operator to focus on, "" => all
    KLASS   = "MatrixD"                                            // the class under test
    ITER    = 100                                                  // number of test iterations

    // Size parameter(s) used for variables in 'test' (customize per class)

    private val dim1 = 1000                                        // # matrix rows
    private val dim2 = 1000                                        // # matrix columns

    // Random variate generators (customize per class)

    private val rmg  = RandomMatD (dim1, dim2)                     // random matrix generator
    private val rig1 = Randi0 (0, dim1 - 1)                        // random integer/index generator
    private val rig2 = Randi0 (0, dim2 - 1)                        // random integer/index generator

    // Variables used in 'test' (customize per class)

    private val x = new MatrixD (dim1, dim2)                       // first matrix
    private val y = new MatrixD (dim1, dim2)                       // second matrix
    private var j = 0                                              // first integer/index value
    private var k = 0                                              // second integer/index value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomize all variables used in `Tester`s 'test' method.
     */
    def randomize ()
    {
        x set rmg.gen ()                                           // randomly reset variables
        y set rmg.gen ()
        j = rig1.igen
        k = rig2.igen
    } // randomize

    testClass ()

    println ("\nTest no argument methods/unary operators")
 
    test ("t",         x.t,            
                       MatrixD (for (j <- x.range2) yield VectorD (for (i <- x.range1) yield x(j, i))))

} // MatrixD_T object

