
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Aravind Kalimurthy
 *  @version 1.3
 *  @date    Mon Aug 29 12:31:10 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see docs.oracle.com/javase/8/docs/api/java/util/regex
 *  @see www.brics.dk/automaton/doc/index.html
 */

package scalation
package util

import java.util.regex.MatchResult

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PatMatcher` class provides a simple means for using Java regular expressions.
 *  Following the Facade design pattern, it combines functionality from both
 *  `Pattern` and `Matcher`.  A faster alternative is `PatMatcherB`, see below.
 *  @param regex  the regular expression pattern to be matched
 */
class PatMatcher (regex: String)
      extends MatchResult with Error
{
    import java.util.regex.{Matcher, Pattern}

    /** The compiled regular expression, i.e., the pattern/machine to recognize the 'regex'.
     */
    private val compliedPat = Pattern.compile (regex)

    /** The pattern matcher for a given 'input' string (must call matcher).
     */
    private var jMatcher: Matcher = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matcher that will match the given 'input' against the regex pattern.
     *  @param input  the string to be matched
     */
    def matcher (input: String): Matcher =
    {
        jMatcher = compliedPat.matcher (input)
        jMatcher
    } // matcher

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the next occurrence of 'regex' in the 'input' string.
     *  Must call 'matcher' method first.
     */
    def find (): Boolean = jMatcher.find ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Resets this matcher and then attempts to find the next subsequence of the input
     *  sequence that matches the pattern, starting at the specified index.
     *  Must call 'matcher' method first.
     *  @param start  the index to start searching for a match
     */
    def find (start: Int): Boolean = jMatcher.find (start)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Match the input sequence, starting at the beginning of the region, against the pattern.
     */
    def lookingAt (): Boolean = jMatcher.lookingAt ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Match the entire region against the pattern.
     */
    def matches (): Boolean = jMatcher.matches ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the start index of the match.
     */
    def start (): Int = jMatcher.start ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the start index of the subsequence captured by the given group during this match.
     *  @param group  the index of a capturing group in this matcher's pattern
     */
    def start (group: Int): Int = jMatcher.start (group)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the offset after the last character matched.
     */
    def end (): Int = jMatcher.end ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the offset after the last character of the subsequence captured by the
     *  given group during this match.
     *  @param group  the index of a capturing group in this matcher's pattern
     */
    def end (group: Int): Int = jMatcher.end (group)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the input subsequence matched by the previous match.
     */
    def group (): String = jMatcher.group ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the input subsequence captured by the given group during the previous match operation.
     *  @param group
     */
    def group (group: Int): String = jMatcher.group (group)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of capturing groups in this match result's pattern.
     */
    def groupCount (): Int = jMatcher.groupCount ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this `PatMatcher` object to its underlying regex string.
     */
    override def toString: String = regex

} // PatMatcher class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PatMatcherB` class provides a simple means for using Brics regular expressions.
 *  FIX:  Brics results often do not agree with Java regex results, so still searching
 *  for a good alternative to 'java.util.regex'.  The following benchmark shows that
 *  Brics is fast, but only agrees with 'java.util.regex' in 2 of 5 test cases.
 *   @see www.javaadvent.com/2015/12/java-regular-expression-library-benchmarks-2015.html
 *  @param regex  the regular expression pattern to be matched
 *
class PatMatcherB (regex: String)
      extends MatchResult with Error
{    
    import dk.brics.automaton._

    /** The compiled regular expression, i.e., the pattern/machine to recognize the 'regex'.
     */
    private val compliedPat = new RunAutomaton (new RegExp (regex).toAutomaton)

    /** The current 'input' string
     */
    private var input: String = null 

    /** The current pattern matcher for a given 'input' string (must call matcher).
     */
    private var bMatcher: AutomatonMatcher = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matcher that will match the given 'input' against the regex pattern.
     *  @param input  the string to be matched
     */
    def matcher (_input: String): AutomatonMatcher = 
    {
        input = _input
        bMatcher = compliedPat.newMatcher (input)
        bMatcher
    } // matcher

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the next occurrence of 'regex' in the 'input' string.
     *  Must call 'matcher' method first.
     */
    def find (): Boolean = bMatcher.find ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Resets this matcher and then attempts to find the next subsequence of the input
     *  sequence that matches the pattern, starting at the specified index.
     *  Must call 'matcher' method first.
     *  @param start  the index to start searching for a match
     */
    def find (start: Int): Boolean = compliedPat.run (input, start) > 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Match the input sequence, starting at the beginning of the region, against the pattern.
     */
    def lookingAt (): Boolean = compliedPat.run (input, 0) > 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Match the entire region against the pattern.
     */
    def matches (): Boolean =  compliedPat.run (input)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the start index of the match.
     */
    def start (): Int = bMatcher.start ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the start index of the subsequence captured by the given group during this match.
     *  @param group
     */
    def start (group: Int): Int = bMatcher.start (group)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the offset after the last character matched.
     */
    def end (): Int = bMatcher.end ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the offset after the last character of the subsequence captured by the
     *  given group during this match.
     *  @param group  the index of a capturing group in this matcher's pattern
     */
    def end (group: Int): Int = bMatcher.end (group)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the input subsequence matched by the previous match.
     */
    def group (): String = bMatcher.group ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the input subsequence captured by the given group during the previous match operation.
     *  @param group
     */
    def group (group: Int): String = bMatcher.group (group)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of capturing groups in this match result's pattern.
     */
    def groupCount (): Int = bMatcher.groupCount ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this `PatMatcherB` object to its underlying regex string.
     */
    override def toString: String = regex

} // PatMatcherB class
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PatMatcherTest` object is used to test the `PatMatcher` vs. `PatMatcherB` classes.
 *  It compares the match results and performance of Java and Brics regex pattern matchers.
 *  @see http://lh3lh3.users.sourceforge.net/reb.shtml
 *  > run-main scalation.util.PatMatcherTest
 */
object PatMatcherTest extends App
{
    import scala.collection.mutable.ListBuffer
    import scalation.linalgebra.MatrixD

    val DEBUG = true
    val ITER  = 100
    val sourceFile = SRC_SCALA_DIR + ⁄ + "scalation" + ⁄ + "util" + ⁄ + "PatMatcher.scala"

    val pattern = Array (
        "([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)",                         // URI (protocol://server/path) 
        "([^ @]+)@([^ @]+)",                                                 // Email (name@server)
        "([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)",             // Date (month/day/year)
        "([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?|([^ @]+)@([^ @]+)")      // URI|Email

    val lines = getFromURL_File (sourceFile).toArray                         // lines from file
    val tim   = new MatrixD (pattern.length, 2)                              // matrix to hold timing results
    val res   = ListBuffer [Boolean] ()                                      // match results for java.util.regex
    val res2  = ListBuffer [Boolean] ()                                      // match results for competitor

    for (j <- pattern.indices) {
        banner ("T E S T: " + pattern(j))

        banner ("Test: java.util.regex")
        val pat = new PatMatcher (pattern(j))
        for (line <- lines) { val mat = pat.matcher (line); mat.find () }       // don't gauge first time through
        tim(j, 0) = gauge {
            for (i <- 0 until ITER) {
                res.clear ()
                for (line <- lines) { val mat = pat.matcher (line); res += mat.find () }
            } // for
        } // gauge
        println ("res = " + res)

/*
        banner ("Test: dk.brics.automaton")
        val pat2 = new PatMatcherB (pattern(j))
        for (line <- lines) { val mat = pat2.matcher (line); mat.find () }       // don't gauge first time through
        tim(j, 1) = gauge {
            for (i <- 0 until ITER) {
                res2.clear ()
                for (line <- lines) { val mat = pat2.matcher (line); res2 += mat.find () }
            } // for
        } // gauge
        println ("res2 = " + res2)

        assert (res2 == res)
*/
    } // for

    println ("tim = " + tim)
    println ("avg tim = " + tim.mean)
        
} // PatMatcherTest object

// http://www.cs.uga.edu/~jam/scalation.html
// jam@cs.uga.edu
// 08/28/2016
// http://www.cs.uga.edu/~jam/scalation.html
