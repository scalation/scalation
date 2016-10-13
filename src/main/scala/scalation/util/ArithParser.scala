
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Aug 23 15:38:11 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Code adapted from
 *  @see labun.com/fh/ma.pdf
 */

package scalation.util

import scala.util.parsing.combinator.JavaTokenParsers

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ArithParser` trait illustrates how to use Scala's Parser Combinators.
 *  The code is used to parser a simple arithmetic expression language.
 *  Rewriting/evaluation is also performed via the '^^^' operator.
 */
trait ArithParser extends JavaTokenParsers
{
    val Add = (a: Double , b: Double) => a + b
    val Sub = (a: Double , b: Double) => a - b
    val Mul = (a: Double , b: Double) => a * b
    val Div = (a: Double , b: Double) => a / b
    val Number = (a: String) => a.toDouble

    def expr: Parser [Double] = chainl1 (term, "+" ^^^ Add | "-" ^^^ Sub)

    def term = chainl1 (factor , "*" ^^^ Mul | "/" ^^^ Div)

    def factor = floatingPointNumber ^^ Number | "(" ~> expr <~ ")"

} // ArithParser trait

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ArithParserTest` object is used to test the `ArithParser` trait.
 *  > run-main scalation.util.ArithParserTest
 */
object ArithParserTest extends App with ArithParser
{
    val exprString = "1 + 2*3"
    println ("exprString = " + exprString)
    println ("result     = " + parseAll (expr, exprString))

} // ArithParserTest object

