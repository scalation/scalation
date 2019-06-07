
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vishnu Gowda Harish, Vinay Kumar Bingi
 *  @version 1.6
 *  @date    Thu Dec 15 12:47:37 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing.linalgebra

import org.junit.Test

import scalation.linalgebra.{MatrixD, RleMatrixD, VectorD}
import scalation.random.{Randi0, RandomMatD, RandomVecD}

import testing.Tester

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RleMatrixD_T` driver class conducts unit testing on the `RleMatrixD` class
 *  by invoking the `RleMatrixD_T` testing object.  Run 'test-only' to test `RleMatrixD`
 *  or 'test' to run all unit tests.
 *------------------------------------------------------------------------------
 *  > test-only testing.linalgebra.RleMatrixD_T
 *  > test
 */
class RleMatrixD_T { @Test def testAll () { RleMatrixD_T } }


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RleMatrixD_T` object conducts unit testing on the `RleMatrixD` class using the
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
 *  > test:runMain testing.linalgebra.RleMatrixD_T
 */
object RleMatrixD_T extends Tester with App
{
    // Reassign parameters from `Tester` trait as needed

    DEBUG   = false                                                // debug flag
    CORRECT = true                                                 // test correctness/performance
    FOCUS   = ""                                                   // method/operator to focus on, "" => all
    KLASS   = "RleMatrixD"                                         // the class under test
    ITER    = 10                                                    // number of test iterations

    // Size parameter(s) used for variables in 'test' (customize per class)

    private val dim1 = 50                                        // # matrix rows
    private val dim2 = 50                                        // # matrix columns

    // Random variate generators (customize per class)

    private val rmg  = RandomMatD (dim1, dim2)                      // random matrix generator
    private val rvg1  = RandomVecD (dim1)                           // random vector generator
    private val rvg2  = RandomVecD (dim2)                           // random vector generator
    private val rig1 = Randi0 (0, dim1 - 1)                         // random integer/index generator
    private val rig2 = Randi0 (0, dim2 - 1)                         // random integer/index generator
    private val rig3 = Randi0 (math.min (dim1 - 1, dim2 - 1))       // random integer/index generator

    // Variables used in 'test' (customize per class)

    private var x =  rmg.rlegenc                                    // first Rle matrix
    private var y =  rmg.rlegenc                                    // second Rle matrix
    private var z =  RleMatrixD (rmg.gen)                           // third Rle matrix
    private var xD = x.toDense                                      // first dense matrix
    private var yD = y.toDense                                      // second dense matrix
    private var zD = z.toDense                                      // third dense matrix
    private var u =  rvg1.repgen                                    // first Rle vector (Row Vector)
    private var v =  rvg2.repgen                                    // second Rle vector (Column Vector)
    private var uD =  u.toDense                                     // first dense vector (Row Vector)
    private var vD =  v.toDense                                     // second dense vector (Column Vector)
    private var j = 0                                               // first integer/index value
    private var k = 0                                               // second integer/index value
    private var s = 2                                               // third integer/non zero value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomize all variables used in `Tester`s 'test' method.
     */
    def randomize ()
    {
        x = rmg.rlegenc                                             // randomly reset variables
        y = rmg.rlegenc
        z = RleMatrixD (rmg.gen)
        xD = x.toDense
        yD = y.toDense
        zD = z.toDense
        j = rig1.igen
        k = rig2.igen
        s = rig3.igen 
    } // randomize

    testClass ()

    println ("\nTest no argument methods/unary operators")

    test ("t",              x.t.toDense,            
                            xD.t)
    test ("sum",            x.sum,
                            xD.sum)
//  test ("det",            xD.det,
//                          x.det)                                  // WORKS BUT VERY HIGH COMPLEXITY. DO IT AS DET L * DET U
    test ("sumLower",       x.sumLower,
                            xD.sumLower)
    test ("sumAbs",         x.sumAbs,
                            xD.sumAbs)
    test ("reduce",         z.reduce.toDense,
                            zD.reduce)
    test ("nullspace_ip",   z.nullspace_ip.toDense,
                            zD.nullspace_ip)
    test ("nullspace",      z.nullspace.toDense,
                            zD.nullspace)
    test ("isRectangular",  x.isRectangular,
                            xD.isRectangular)   
    test ("inverse",        z.inverse.toDense,
                            zD.inverse)
    test ("inverse_ip",     z.inverse_ip.toDense,
                            zD.inverse_ip) 
    test ("upperT",         x.upperT.toDense,
                            xD.upperT)
    test ("lowerT",         x.lowerT.toDense,
                            xD.lowerT)
    test ("toInt",          x.toInt.toDense,
                            xD.toInt)
    test ("trace",          x.trace,
                            xD.trace)
                            
    println ("\nTest methods that take scalar arguments")

    test ("+s",             (x + s).toDense,
                            xD + s)
    test ("+=s",            (x += s).toDense,
                            xD += s)
    test ("-s",             (x - s).toDense,
                            xD - s)
    test ("-=s",            (x -= s).toDense,
                            xD -= s)                       
    test ("*s",             (x * s).toDense,
                            xD * s)
    test ("*=s",            (x *= s).toDense,
                            xD *= s)
    test ("/s",             (x / s).toDense,
                            xD / s)
    test ("/=s",            (x /= s).toDense,
                            xD /= s)
    test ("~^",             (x ~^ 5).toDense,
                            xD ~^ 5)
    test ("col",            (x.col (s)).toDense,
                            xD.col (s))
    test ("diag",           (x.diag (s, s)).toDense,
                            xD.diag (s, s))
    test ("getDiag",        (x.getDiag (s)).toDense,
                            xD getDiag (s))
    test ("max",            x max s,  
                            xD max s)      
    test ("min",            (x min s),
                            xD min s)       
    test ("sliceCol",       (x.sliceCol (math.max(0, s - 1), s)).toDense,
                            xD.sliceCol (math.max(0, s - 1), s))
    test ("sliceEx",        (x.sliceEx (s, s)).toDense,
                            xD.sliceEx (s, s))
                            
    println ("\nTest vector argument methods")

    test ("+v",             (x + v).toDense,
                            xD + v)
    test ("+=v",            (x += v).toDense,
                            xD += v)
    test ("-v",             (x - v).toDense,
                            xD - v)
    test ("-=v",            (x -= v).toDense,
                            xD -= v)
    test ("*v",             (x * v).toDense,
                            xD * v)
    test ("**v",            (x ** v).toDense,
                            xD ** v)
    test ("**=v",           (x **= v).toDense,
                            xD **= v)    
    test ("+:",             (x.+: (u)).toDense,
                            xD.+: (uD))
    test ("+:^",            (x.+^: (v)).toDense,
                            xD.+^: (vD))
    test (":+",             (x.:+ (u)).toDense,
                            xD.:+ (uD))
    test (":+^",            (x.:^+ (v)).toDense,
                            xD.:^+ (vD))
    test ("bsolve",         (x bsolve v).toDense,
                            xD bsolve v)
    test ("dot v",          (x dot u).toDense,
                            xD dot u)

//  test ("solve v",        (z solve (v)).toDense,           // TODO FLOATING POINT ERRORS
//                          zD solve (v))    

    println ("\nTest matrix argument methods")
    
    test ("+",              (x + y).toDense,
                            xD + yD)
    test ("++",             (x ++ y).toDense,
                            xD ++ yD)
    test ("++^",            (x ++^ y).toDense,
                            xD ++^ yD)
    test ("+dense",         (x + yD).toDense,
                            xD + yD)
    test ("+=",             (x += y).toDense,
                            xD += yD)
    test ("-",              (x - y).toDense,
                            xD - yD)
    test ("-dense",         (x - yD).toDense,
                            xD - yD)
    test ("-=",             (x -= y).toDense,
                            xD -= yD)
    test ("*",              (x * y).toDense,
                            xD * yD)
    test ("*=",             (x *= y).toDense,
                            xD *= yD)
    test ("diag",           (x diag y).toDense,
                            xD diag yD)
    test ("dot",            (x dot y).toDense,
                            xD dot yD)
    test ("mdot",           (x mdot y).toDense,
                            xD mdot yD)                        

} // RleMatrixD_T object

