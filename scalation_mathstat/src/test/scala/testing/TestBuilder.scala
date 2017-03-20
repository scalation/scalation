
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Fri Apr  1 12:26:23 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing

import java.io.{File, PrintWriter}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TestBuilder` object builds unit tests for a given class.
 *  > test:runMain testing.TestBuilder
 */
object TestBuilder extends App
{
    /** Shorthand for universal file separator ('/' or '\\').
     */
    val _l = java.io.File.separator

    /** Directory in which to place the generated code.
     */
    val DIR = s"src${_l}test${_l}scala${_l}testing"

    new TestBuilder ("scalation.stat", "StatVector")

} // TestBuilder object

import TestBuilder._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TestBuilder` class builds unit tests for each of the methods/operators of
 *  a given class.
 *  @param klass  the class for which the unit tests are to be built (class under test)
 */
class TestBuilder (PACKAGE: String, CLASS: String)
{
    private val SUBPACKAGE = PACKAGE.slice (PACKAGE.lastIndexOf ('.')+1, PACKAGE.length)
    private val klass      = Class.forName (s"$PACKAGE.$CLASS")

    println (s"TestBuilder: generate unit test code for class: $PACKAGE.$CLASS")
    println (s"TestBuilder: unit test: testing.$SUBPACKAGE.${CLASS}_T")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build unit tests for each of the methods/operators in the class under test.
     */
    def buildTests: String =
    {
        val methods = klass.getMethods.map (_.getName).filterNot (_ contains "$")
        println ("methods" + methods.deep)
        val s = new StringBuilder ()
        for (m <- methods) {
            val pad = if (m.length > 4) "" else "\t"
            s.append (s"    test ($m,$pad\t x.$m,\n\t\t\t <oracle>)\n")
        } // for
        s.toString
    } // buildTests

// Beginning of string holding code template -----------------------------------

        val code = raw"""
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Tue Mar 29 16:54:57 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing.$SUBPACKAGE

import org.junit.Test

import scala.math._

import $PACKAGE.$CLASS
import scalation.random._

import testing.Tester

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `${CLASS}_T` driver class conducts unit testing on the `$CLASS` class
 *  by invoking the ${CLASS}_T testing object.  Run 'test-only' to test `$CLASS`
 *  or 'test' to run all unit tests.
 *------------------------------------------------------------------------------
 *  > test-only testing.$SUBPACKAGE.${CLASS}_T
 *  > test
 */
class ${CLASS}_T { @Test def testAll () { ${CLASS}_T } }


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `${CLASS}_T` testing object conducts unit testing on the `$CLASS` class
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
 *  > test:runMain testing.$SUBPACKAGE.${CLASS}_T
 */
object ${CLASS}_T extends Tester // with App
{
    // Reassign parameters from `Tester` trait as needed

    DEBUG   = false                                                // debug flag
    CORRECT = true                                                 // test correctness/performance
    FOCUS   = ""                                                   // method/operator to focus on, "" => all
    KLASS   = "$CLASS"                                             // the class under test
    ITER    = 100                                                  // number of test iterations

    // Size parameter(s) used for variables in 'test' (customize per class)

    private val dim = 10                                           // vector size

    // Random variate generators (customize per class)

    private val rv  = RandomVecD (count = dim, density = 1.0)      // random vector generator
    private val rn  = Uniform (0.0, 100.0)                         // random double generator
    private val rj  = Randi0 (0, dim)                              // random integer/index generator

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

    println ("\nTest methods/unary operators")

$buildTests
} // ${CLASS}_T object

"""

// Ending of string holding code template --------------------------------------

//  println (code)

    val writer = new PrintWriter (new File (DIR + _l + CLASS + "_T.scalaa"))
    writer.write (code)
    writer.close ()

} // TestBuilder class

