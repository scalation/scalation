
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
/** The `Wildcard` object defines useful contants for the `Wildcard` class.
 */
object Wildcard
{
    val DEBUG          = false                    // debug flag
    val CHAR_WILD_ONE  = '_'                      // matches any single character
    val CHAR_WILD_MANY = '%'                      // matches zero or more characters
    val CHAR_WILD      = List ("_", "%")          // wildcard characters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the given string 'str' contains any wildcard characters.
     *  @param str  the string to be checked
     */
    def hasWildcards (str: Any): Boolean =
    {
        str match {
        case _: String => CHAR_WILD.exists (str.asInstanceOf [String].contains)
        case _         => false
        } // match
    } // hasWildcards

} // Wildcard object

import Wildcard._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Wildcard` value class provides an implementation for wildcard string
 *  matching that checks if the 'pattern' string matches against a given 'self'
 *  string based on single (CHAR_WILD_ONE) and/or multiple (CHAR_WILD_MANY) wildcards.
 *  @param pattern  the underlying object to be accessed via the 'pattern' accessor
 */
class Wildcard (val self: String)
      extends AnyVal
{    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The equal-match operator determines whether the 'pattern' string matches the
     *  given 'self' string.   Pattern may contain single- and/or multiple-replacing
     *  wildcard characters.
     *  @param self  the self against which the string to be matched
     */
    def =~ (pattern: Any): Boolean =
    {
        pattern match {
        case _: String => isMatch (pattern.asInstanceOf [String])
        case _         => false
        } // match
    } // hasWildcards

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test whether the 'pattern' string matches the given 'self' string.
     *  Pattern may contain single- and/or multiple-replacing wildcard characters.
     *  @param self  the self against which the string to be matched
     */
    def isMatch (pattern: String): Boolean = 
    {
        val end = (pattern.length, self.length)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Match the 'pattern' string until first/next multiple wildcard in 'self'.
         *  @param p  the positions in the pattern/self strings
         */
        def consume2WILD_MANY (p: (Int, Int)): (Int, Int) =
        {
            var (i, j) = p
            while (i < end._1 && j < end._2 && self(j) != CHAR_WILD_MANY &&
                  (pattern(i) == self(j) || self(j) == CHAR_WILD_ONE)) {
                i += 1; j += 1
            } // while
            if (DEBUG) println (s"consume2WILD_MANY: moves from $p to ${(i, j)}")
            (i, j)                                                // return how far you got
        } // consume2WILD_MANY

        val pointTested = ofDim [Boolean] (end._1+1, end._2+1)    // each true value indicates pattern vs. self positions that have been tested
        val posStack    = Stack [(Int, Int)] ()                   // stack containing (pattern, self) positions still requiring testing
        var pos         = consume2WILD_MANY ((0, 0))              // match beginning of pattern string until first WILD_MANY in self

        var matched = false

        // Push position pos onto stack if it points to end of self or WILD_MANY

        if (pos._2 == end._2 || self(pos._2) == CHAR_WILD_MANY) {
            pointTested(pos._1)(pos._2) = true
            posStack.push (pos)
        } // if

        // Repeat matching until either string is matched against the self or no more parts remain on stack to test

        while (posStack.nonEmpty && ! matched) {
            pos = posStack.pop ()                                 // pop pattern and self positions from stack

            // Matching will succeed if rest of the pattern string matches rest of the self

            if (pos._1 == end._1 && pos._2 == end._2) {
                matched = true                                    // reached end of both self & pattern strings, hence matching is successful
            } else {
                // Next character in self is guaranteed to be WILD_MANY, so skip it & search for matches until next WILD_MANY is reached

                for (patternStart <- pos._1 to end._1) {
                    var p = (patternStart + 1, pos._2 + 1)           // position to resume matching
                    p = if (p._2 == end._2) end                   // self ends with WILD_MANY, so rest of pattern string is atomatically matched
                        else consume2WILD_MANY (p)                // continue matching until next WILD_MANY

                    // Reaching the next WILD_MANY in self without breaking the matching sequence implies there is another candidate to match.
                    // This candidate should be pushed onto stack for further processing, while also marking (pattern, self) positions as tested,
                    // so they it will not be pushed onto stack later again

                    if (( (p._2 == end._2 && p._1 == end._1) || (p._2 < end._2 && self(p._2) == CHAR_WILD_MANY)) && ! pointTested(p._1)(p._2)) {
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

    val text   = new Wildcard ("Universit_")
    val text1  = new Wildcard ("Uni%")
    val text2  = new Wildcard ("U_iver%")
    val text3  = new Wildcard ("W_it_enB_")
    val result = Array.ofDim [Boolean] (4)

    time {
        result(0) = text.isMatch ("University")
        result(1) = text1 =~ "University"
        result(2) = text2.isMatch ("University")
        result(3) = text3 =~ "WrittenBy"
    } // time

    println ("University = " + text  + " ::: " + result(0))
    println ("University = " + text1 + " ::: " + result(1))
    println ("University = " + text2 + " ::: " + result(2))
    println ("WrittenBy  = " + text3 + " ::: " + result(3))

} // WildcardTest object

