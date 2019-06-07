
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy
 *  @version 1.2
 *  @date    Wed Aug 24 20:55:35 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.c-sharpcorner.com/uploadfile/b81385/efficient-string-matching-algorithm-with-use-of-wildcard-characters/
 */

package scalation.util

import scala.Array.ofDim
import scala.collection.mutable.Stack

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Wildcard` defines useful contants for the `Wildcard` class.
 */
object Wildcard
{
    val DEBUG          = true                     // debug flag
    val CHAR_WILD_ONE  = '_'                      // matches any single character
    val CHAR_WILD_MANY = '%'                      // matches zero or more characters
    val CHAR_WILD      = List ("_", "%")          // wildcard characters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the given string 'str' contains any wildcard characters.
     *  @param str  the string to be checked
     */
    def hasWildcards (str: String): Boolean =
    {
        CHAR_WILD.exists (str.asInstanceOf [String].contains)
    } // hasWildcards

} // Wildcard object

import Wildcard._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Wildcard` value class provides an implementation for wildcard string
 *  matching that checks if the 'self' string matches against a given 'pattern'
 *  string based on single (CHAR_WILD_ONE) and/or multiple (CHAR_WILD_MANY) wildcards.
 *  @param self  the underlying object to be accessed via the 'self' accessor
 */
class Wildcard (val self: String)
      extends AnyVal
{    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The equal-match operator determines whether the 'self' string matches the
     *  given 'pattern' string.   Pattern may contain single- and/or multiple-replacing
     *  wildcard characters.
     *  @param pattern  the pattern against which the string to be matched
     */
    def =~ (pattern: String): Boolean = isMatch (pattern)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test whether the 'self' string matches the given 'pattern' string.
     *  Pattern may contain single- and/or multiple-replacing wildcard characters.
     *  @param pattern  the pattern against which the string to be matched
     */
    def isMatch (pattern: String): Boolean = 
    {
        val end = (self.length, pattern.length)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Match the 'self' string until first/next multiple wildcard in 'pattern'.
         *  @param p  the positions in the self/pattern strings
         */
        def consume2WILD_MANY (p: (Int, Int)): (Int, Int) =
        {
            var (i, j) = p
            while (i < end._1 && j < end._2 && pattern(j) != CHAR_WILD_MANY &&
                  (self(i) == pattern(j) || pattern(j) == CHAR_WILD_ONE)) {
                i += 1; j += 1
            } // while
            if (DEBUG) println (s"consume2WILD_MANY: moves from $p to ${(i, j)}")
            (i, j)                                               // return how far you got
        } // consume2WILD_MANY

        val pointTested = ofDim [Boolean] (end._1+1, end._2+1)    // each true value indicates self vs. pattern positions that have been tested
        val posStack    = Stack [(Int, Int)] ()                   // stack containing (self, pattern) positions still requiring testing
        var pos         = consume2WILD_MANY ((0, 0))              // match beginning of self string until first WILD_MANY in pattern

        var matched = false

        // Push position pos onto stack if it points to end of pattern or WILD_MANY

        if (pos._2 == end._2 || pattern(pos._2) == CHAR_WILD_MANY) {
            pointTested(pos._1)(pos._2) = true
            posStack.push (pos)
        } // if

        // Repeat matching until either string is matched against the pattern or no more parts remain on stack to test

        while (posStack.nonEmpty && ! matched) {
            pos = posStack.pop ()                                 // pop self and pattern positions from stack

            // Matching will succeed if rest of the self string matches rest of the pattern

            if (pos._1 == end._1 && pos._2 == end._2) {
                matched = true                                    // reached end of both pattern & self strings, hence matching is successful
            } else {
                // Next character in pattern is guaranteed to be WILD_MANY, so skip it & search for matches until next WILD_MANY is reached

                for (selfStart <- pos._1 to end._1) {
                    var p = (selfStart + 1, pos._2 + 1)           // position to resume matching
                    p = if (p._2 == end._2) end                   // pattern ends with WILD_MANY, so rest of self string is atomatically matched
                        else consume2WILD_MANY (p)                // continue matching until next WILD_MANY

                    // Reaching the next WILD_MANY in pattern without breaking the matching sequence implies there is another candidate to match.
                    // This candidate should be pushed onto stack for further processing, while also marking (self, pattern) positions as tested,
                    // so they it will not be pushed onto stack later again

                    if (( (p._2 == end._2 && p._1 == end._1) || (p._2 < end._2 && pattern(p._2) == CHAR_WILD_MANY)) && ! pointTested(p._1)(p._2)) {
                        pointTested(p._1)(p._2) = true
                        posStack.push (p)
                    } // if
                } // for

            } // if
        } // while
        matched

    } // isMatch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this `Wildcard` object to its underlying string.
     */
    override def toString: String = self

} // Wildcard class
    
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WildcardTest` object is used to test the `Wildcard` class.
 *  > run-main scalation.util.WildcardTest
 */
object WildcardTest extends App
{
    import Wildcard._
        
    val text   = new Wildcard ("University")
    val result = Array.ofDim [Boolean] (3)    
        
    time {
        result(0) = text.isMatch ("Unive_")
        result(1) = text =~ "Universit_"
        result(2) = text.isMatch ("Un%") 
    } // time
    
    println ("self string    = " + text)
    println ("pattern Unive_ = " + result(0))
    println ("Universit_     = " + result(1))
    println ("Unive%         = " + result(2))

} // WildcardTest object

