
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Sep 12 21:48:35 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see docs.scala-lang.org/overviews/quasiquotes/setup.html
 *  
 *  Add the following to build.sbt:
 *  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
 *  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
 */

package scalation.util

import scala.reflect.runtime.universe._
// import c.universe._

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuasiQuoteTest` object is used to test how Quasi-Quotes can be used for
 *  code generation.
 *  FIX: generate class files (.class) rather than execute the code
 *  > run-main scalation.util.QuasiQuoteTest 
 */
object QuasiQuoteTest extends App
{
    val BASE = Array (tq"String",      tq"Int")
    val ZERO = Array (q""" "zero" """, q"0")
    val ONE  = Array (q""" "one" """,  q"1")

    for (i <- 0 to 1) {

        val code = q"""

        val label = Array.ofDim [${BASE(i)}] (2)
        label(0) = ${ZERO(i)}
        label(1) = ${ONE(i)}
        println (label.deep)

        """

        println ("code = " + code)
        print ("executing ... ")
        val toolbox = currentMirror.mkToolBox ()
        val compiledCode = toolbox.compile (code)
        val result = compiledCode ()

    } // for
   
} // QuasiQuoteTest object

