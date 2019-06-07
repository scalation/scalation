
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Apr 10 13:27:53 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing.linalgebra

import org.junit.Test

import scalation.linalgebra.{Fac_QR_H2, MatrixD, VectorD}
import scalation.linalgebra.MatrixD.eye
import scalation.math.ExtremeD.TOL
import scalation.random.{RandomMatD, RandomVecD}

import testing.Tester

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixD_T` driver class conducts unit testing on the `MatrixD` class
 *  by invoking the `MatrixD_T` testing object.  Run 'test-only' to test `MatrixD`
 *  or 'test' to run all unit tests.
 *------------------------------------------------------------------------------
 *  > test-only testing.linalgebra.MatrixD_T
 *  > test
 */
class Fac_QR_H2_T { @Test def testAll () { Fac_QR_H2_T } }


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
 *  > test:runMain testing.linalgebra.Fac_QR_H2_T
 */
object Fac_QR_H2_T extends Tester with App
{
    // Reassign parameters from `Tester` trait as needed

    DEBUG   = true                                                 // debug flag
    CORRECT = true                                                 // test correctness/performance
    FOCUS   = ""                                                   // method/operator to focus on, "" => all
    KLASS   = "Fac_QR_H2"                                          // the class under test
    ITER    = 10                                                   // number of test iterations

    // Size parameter(s) used for variables in 'test' (customize per class)

    private val dim1 = 12                                          // # matrix rows
    private val dim2 = 10                                          // # matrix columns

    // Random variate generators (customize per class)

    private val rmg  = RandomMatD (dim1, dim2)                     // random matrix generator
    private val rvg  = RandomVecD (dim1)                           // random vector generator

    // Variables used in 'test' (customize per class)

    private val x = new MatrixD (dim1, dim2)                       // first matrix
    private val y = new VectorD (dim1)                             // first vector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomize all variables used in `Tester`s 'test' method.
     */
    def randomize ()
    {
        x set rmg.gen ()                                           // randomly reset variables
        y set rvg.gen ()
    } // randomize

    testClass ()

    println ("\nTest no argument methods/unary operators")
 
    test ("factor",    { val (q, r) = (new Fac_QR_H2 (x)).factor12 (); q * r },
                       x)
    test ("q.t * q",   { val q = (new Fac_QR_H2 (x)).factor1 (); (q.t * q).asInstanceOf [MatrixD].clean (TOL) },
                       eye (dim2))
    test ("solve",     { val qr = new Fac_QR_H2 (x); qr.factor (); qr.solve (y) },
                       { val (q, r) = (new Fac_QR_H2 (x)).factor12 (); r.bsolve (q.t * y) })

} // Fac_QR_H2_T object

