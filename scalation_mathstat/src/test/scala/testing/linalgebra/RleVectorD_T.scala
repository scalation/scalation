
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vishnu Gowda Harish
 *  @version 1.3
 *  @date    Tue Mar  8 18:30:36 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing.linalgebra

import org.junit.Test

import scala.math.{abs, max, min, sqrt}

import scalation.linalgebra.{VectorD, VectorI, VectorL,RleVectorD}
import scalation.math.double_exp
import scalation.random.{Randi0, RandomVecD, Uniform}

import testing.Tester

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RleVectorD_T` driver class conducts unit testing on the `RleVectorD` class
 *  by invoking the RleVectorD_T testing object.  Run 'test-only' to test `RleVectorD`
 *  or 'test' to run all unit tests.
 *------------------------------------------------------------------------------
 *  > test-only testing.linalgebra.RleVectorD_T
 *  > test
 */
class RleVectorD_T { @Test def testAll () { RleVectorD_T } }


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RleVectorD_T` object conducts unit testing on the `RleVectorD` class using the
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
 *  > test:runMain testing.linalgebra.RleVectorD_T
 */
object RleVectorD_T extends Tester //with App
{
    // Reassign parameters from `Tester` trait as needed

    DEBUG   = false                                               // debug flag
    CORRECT = true                                                // test correctness/performance
    FOCUS   = ""                                                  // method/operator to focus on, "" => all
    KLASS   = "RleVectorD"                                        // the class under test
    ITER    = 10                                                  // number of test iterations

    // Size parameter(s) used for variables in 'test' (customize per class)

    private val dim = 100                                          // vector size

    // Random variate generators (customize per class)

    private val rv  = RandomVecD (dim = dim, density = 1.0)        // random vector generator
    private val rn  = Uniform (0.0, 100.0)                         // random double generator
    private val rj  = Randi0 (0, dim)                              // random integer/index generator

    // Variables used in 'test' (customize per class)
    private val x   = rv.repgen                                    // first rle vector
    private val y   = rv.repgen                                    // second rle vector
    private var t   = rn.gen                                       // random value 
    private var z   = x.toDense                                    // dense version of first rle vector
    private var s   = 0.0                                          // scalar value
    private var j   = 0                                            // first integer/index value
    private var k   = 0                                            // second integer/index value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomize all variables used in `Tester`s 'test' method.
     */
    def randomize ()
    {   
        x set rv.repgen ()
        y set rv.repgen ()
        s = rn.gen
        j = rj.igen
        k = rj.igen
        t = rn.gen
        z = x.toDense
    } // randomize

    testClass ()

    println ("\nTest no argument methods/unary operators")
 
    test ("unary-",         -x,            
                            { val c = for (i <- x.indices) yield -x(i); RleVectorD (c) })
    test ("abs",            x.abs,
                            RleVectorD (for (i <- x.indices) yield abs (x(i))))
    test ("argmax",         x.argmax (),
                            x().indexOf (x().max))
    test ("argmaxPos",      x.argmaxPos (),
                            x().filter (_ >= 0.0).indexOf (x().max))
    test ("argmin",         x.argmin (),
                            x().indexOf (x().min))
    test ("argminNeg",      x.argminNeg (),
                            x().filter (_ <= 0.0).indexOf (x().min))
    test ("countNeg",       x.countNeg, 
                            x().filter (_ < 0.0).size)
    test ("countPos",       x.countPos,
                            x().filter (_ > 0.0).size)
    test ("cumulate",       x.cumulate,
                            { var sum = 0.0; VectorD (for (i <- x.indices) yield { sum += x(i); sum }) })
    test ("distinct",       x.distinct.toDense,
                            z.distinct)
    test ("countinct",      x.countinct,
                            x().distinct.length)
    test ("expand",         x.expand (),
                            x ++ new VectorD (x.dim))
    test ("firstNeg",       x.firstNeg (),
                            x().indexWhere (_ < 0.0))
    test ("firstPos",       x.firstPos (),
                            x().indexWhere (_ > 0.0))
    test ("isNonnegative",  x.isNonnegative,
                            ! x().exists (_ < 0.0))
    test ("isSorted",       x.isSorted,
                            { def isSo: Boolean = { for (i <- 1 until x.dim if x(i) < x(i-1)) return false; true }; isSo })
    test ("max",            x.max (),
                            x().max)
    test ("min",            x.min (),
                            x().min)
    test ("normSq",         x.normSq,
                            x dot x)
    test ("norm",           x.norm, 
                            sqrt (x.normSq))
    test ("norm1",          x.norm1,
                            x.abs.sum)
    test ("normalize",      x.normalize,
                            x * (1.0 / x().sum))
    test ("normalizeU",     x.normalizeU,
                            x * (1.0 / x.norm))
    test ("normalize1",     x.normalize1,
                            x * (1.0 / x().max))
//  test ("rank",           x.rank,
//                          null)                 
    test ("recip",          x.recip,
                            new RleVectorD (x.one (x.dim) / x))
    test ("reverse",        x.reverse.reverse,
                            x)
    test ("size",           x.size,
                            x().size)
    test ("sum",            x.sum,
                            x().sum)
    test ("sumPos",         x.sumPos,
                            (for (i <- x.indices) yield max (x(i), 0.0)).sum)
    test ("swap",           { x.swap (j, k); x },
                            { val t = x(k); x(k) = x(j); x(j) = t; x })
    test ("toInt",          x.toInt,
                            VectorI (x ().map (_.toInt)))
    test ("toLong",         x.toLong,
                            VectorL (x ().map (_.toLong)))
    test ("toDouble",       x.toDouble,
                            VectorD (x ().map (_.toDouble)))

    println ("\nTest one argument methods/binary operators")

    test ("++",             x ++ y,
                            VectorD (x() ++ y()))
    test ("++",             x ++ s,
                            RleVectorD (x() :+ s))
    test ("+",              x + y,
                            RleVectorD (for (i <- x.indices) yield x(i) + y(i)))
    test ("+",              x + s,
                            RleVectorD (for (i <- x.indices) yield x(i) + s))
    test ("+",              x + (1, s),
                            { x(1) += s; x.toDense })
    test ("-",              x - y,
                            RleVectorD (for (i <- x.indices) yield x(i) - y(i)))
    test ("-",              x - s,
                            RleVectorD (for (i <- x.indices) yield x(i) - s))
    test ("-",              x - (1, s),
                            { x(1) -= s; x.toDense })
    test ("*",              x * y,
                            RleVectorD (for (i <- x.indices) yield x(i) * y(i)))
    test ("*",              x * s,
                            RleVectorD (for (i <- x.indices) yield x(i) * s))
    test ("/",              x / y,
                            RleVectorD (for (i <- x.indices) yield x(i) / y(i)))
    test ("/",              x / s,
                            RleVectorD (for (i <- x.indices) yield x(i) / s))
    test ("~^",             x ~^ s,
                            RleVectorD (for (i <- x.indices) yield x(i) ~^ s))        
    test ("contains",       x contains s,
                            x() contains s)
    test ("dot",            x dot y,
                            (x * y).sum)
    test ("exists",         x.exists (_ > s),
                            x().exists (_ > s))
    test ("filter",         x.filter (_ > s),
                            RleVectorD(z.filter (_ > s)))
    test ("filterPos",      VectorI (x.filterPos (_ > s)),
                            VectorI (z.filterPos (_ > s)))
    test ("indexOf",        x indexOf j,
                            x() indexOf j)
    test ("indexWhere",     x.indexWhere (_ > j),
                            x().indexWhere (_ > j))
    test ("map",            x.map ((z: Double) => z * s),
                            RleVectorD (x().map (_ * s)))
    test ("max",            x max y,
                            VectorD (for (i <- x.indices) yield x(i) max y(i)))
    test ("min",            x min y,
                            VectorD (for (i <- x.indices) yield x(i) min y(i)))
    test ("oneAt",          x oneAt j,
                            { val z = new VectorD (x.dim); z(j) = 1.0; RleVectorD (z) })
    test ("_oneAt",         x _oneAt j,
                            { val z = new VectorD (x.dim); z(j) = -1.0; RleVectorD (z) })
    test ("sameDimensions", x sameDimensions y,
                            x().length <=  y().length)
    test ("set",            { x set s; x },
                            RleVectorD (Array.fill (x.dim)(s)))
    test ("set",            { x set y(); x },
                            RleVectorD (y()))
    test ("sumNE",          x sumNE 1,
                            x().sum - x(1))
    test ("slice",          x.slice (j, k),
                            VectorD (x().slice (j, k)))
    test ("select",         x.select (Array (j, k)),
                            { val idx = Array (j, k); VectorD (for (i <- idx) yield x(i)) })
} // RleVectorD_T object

