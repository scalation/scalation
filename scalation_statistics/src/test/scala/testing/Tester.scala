
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Mar 29 13:31:28 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing

import scalation.util.gauge

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tester` trait servers as a foundation for unit testing of class `KLASS`.
 *  For correctness, it compares the results of a method/operator 'call' to an 'oracle'.
 *  If the 'call' and the 'oracle' disagree, an assertion failure exception will be thrown.
 *  For performance, it compares the time taken by the method/operator 'call' to those
 *  taken by the 'oracle' and an optional 'contender'.  The 'contender' need not produce
 *  identical results.
 *------------------------------------------------------------------------------
 *  All methods except 'this', 'apply', 'update', 'foreach' and 'hashCode' should be tested.
 *  May skip '=op' if 'op' is tested, e.g., skip '+=' if '+' is tested.
 *  Also the 'equals' and 'toString' are tested implicitly.
 *  Depending on the 'CORRECT' flag, it will either test correctness or performance.
 *  Note, if the code for the 'contender' or even the 'oracle' is significantly faster,
 *  the method/operator may need be to re-coded.
 *------------------------------------------------------------------------------
 *  To test a class directly, create an object, e.g., "object VectorD_T extends Tester with App".
 *  To test using JUnit, also make a class, e.g., "class VectorD_T" that invokes the object.
 *  Note, the "with App" must be commented out "//" for this to work with JUnit.
 */
trait Tester
{
    protected var DEBUG   = false                                  // debug flag
    protected var CORRECT = true                                   // test correctness/performance
    protected var FOCUS   = ""                                     // method/operator to focus on, "" => all
    protected var KLASS   = ""                                     // the class under test
    protected var ITER    = 100                                    // number of test iterations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate the type of testing.
     */
    def testClass ()
    {
        val n = 38 + KLASS.size
        println ("-" * n)
        println ("| Test the " + (if (CORRECT) "correctness" else "performance") + " of the " + KLASS + " class |")
        println ("-" * n)
    } // testClass

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test a method/operator from class `KLASS`.
     *  @param name       the name for the method/operator to test
     *  @param call       the invocation code for the method/operator
     *  @param oracle     independent code used to certify correctness
     *  @param contender  independent code used to certify correctness
     */
    def test (name: String, call: => Any, oracle: => Any, contender: => Any = null)
    {
        if (FOCUS != "" && name != FOCUS) return                   // FOCUS == "" => test all
        println (s"testing $KLASS.$name")

        var t_call      = 0.0                                      // time accumulator for call (e.g., method to test)
        var t_oracle    = 0.0                                      // time accumulator for oracle (correctness)
        var t_contender = 0.0                                      // time accumulator for competitor (performance)

        for (it <- 0 until ITER) {
            randomize ()                                           // randomize the variables used
            if (DEBUG) {
                println (s"test case $it for $KLASS.$name")
                println (s"call      = $call")
                println (s"oracle    = $oracle")
                println (s"contender = $contender")
            } // if
            if (CORRECT) {
                assert (call == oracle, name)                      // test correctness
            } else {
                t_call      += gauge { call }                      // test performance
                t_oracle    += gauge { oracle }
                if (contender != null) t_contender += gauge { contender }
            } // if
        } // for
        if (! CORRECT) println (s"time: call = \t $t_call \n oracle = \t $t_oracle \n contender = \t $t_contender")
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomize all variables used in 'test'.
     */
    def randomize ()

} // Tester trait

