
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Wed Nov  1 11:42:27 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Matchable` trait serves as a bases for string pattern matchers.
 */
trait Matchable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' string matches the given 'pattern'.
     *  @param pattern  the pattern to be matched (e.g., wildcard or regex)
     */
    def =~ (pattern: String): Boolean

} // Matchable trait

