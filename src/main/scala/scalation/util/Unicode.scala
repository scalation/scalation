
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Sep  9 13:07:48 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Unicode` object provides arrays to facilitate the use of Unicode.
 *  ScalaTion currently uses a few UTF-16 characters, see code below.  Most UTF-16
 *  characters are 2 bytes (16 bits).  Extended characters are encoded in 4 bytes.
 *  ScalaTion limits characters to the range 'U+0000' to 'U+2bff'.
 *  Developers should test Unicode symbols here before trying them in the code.
 *  @see en.wikipedia.org/wiki/UTF-16
 *  @see www.tamasoft.co.jp/en/general-info/unicode.html
 *  @see en.wikipedia.org/wiki/List_of_Unicode_characters
 *  @see arxiv.org/abs/1112.1751
 *------------------------------------------------------------------------------
 *  Unicode is used in the following:
 *  @see `scalation.analytics.Regression`
 *  @see `scalation.linalgebra.bld.BldVector`
 *  @see `scalation.math` (package object)
 *  @see `scalation.math.Complex`
 *  @see `scalation.math.Rational`
 *  @see `scalation.math.Real`
 *  @see `scalation.math.StrNum`
 *  @see `scalation.relalgebra.Relation`
 */
object Unicode
{
    /** Unicode characters for superscripts to 0, 1, ... 9
     */
    val supc = Array ('⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹')

    /** Unicode numbers for superscripts to 0, 1, ... 9
     */
    val supn = Array ('\u2070', '\u00b9', '\u00b2', '\u00b3', '\u2074', '\u2075', '\u2076', '\u2077', '\u2078', '\u2079')

    /** Unicode characters for subscripts to 0, 1, ... 9
     */
    val subc = Array ('₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉')

    /** Unicode numbers for subscripts to 0, 1, ... 9
     */
    val subn = Array ('\u2080', '\u2081', '\u2082', '\u2083', '\u2084', '\u2085', '\u2086', '\u2087', '\u2088', '\u2089')

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unicode characters for superscripts derived for integer 'i'.
     *  @param i  the integer to convert into a subscript
     */
    def sup (i: Int) = for (c <- i.toString) yield supc(c - '0')

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unicode characters for subscripts derived from integer 'i'
     *  @param i  the integer to convert into a subscript
     */
    def sub (i: Int) = for (c <- i.toString) yield subc(c - '0')

} // Unicode object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UnicodeTest` object is used to the `Unicode` object.
 *  > run-main scalation.util.UnicodeTest
 */
object UnicodeTest extends App
{
    import Unicode._

    val uc = Array ('Ɛ', 'Ʋ', 'π', 'σ', '∙', '≠', '≤', '≥', '⋂', '⋃', '⋈')
    val un = Array ('\u0190', '\u01b2', '\u03c0', '\u03c3', '\u2219',
                    '\u2260', '\u2264', '\u2265', '\u22c2', '\u22c3', '\u22c8')

    println ("unicode character:")
    for (c <- uc) println (s"c = $c = U+%04x".format (c.toInt))
    println ("unicode number:")
    for (n <- un) println (s"n = $n = U+%04x".format (n.toInt))

    def ∙ () = "∙ worked"
 
    println (∙)

    println ("supc = " + supc.deep)
    println ("supn = " + supn.deep)
    println ("subc = " + subc.deep)
    println ("subn = " + subn.deep)

    println ("sup (12) = " + sup (12))
    println ("sub (12) = " + sub (12))

} // UnicodeTest object

