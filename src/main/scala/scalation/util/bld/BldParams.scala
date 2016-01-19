
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Sep 26 13:10:03 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util.bld

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldParams` trait defines common values to be used in code generation.
 */
trait BldParams
{
    /** Shorthand for universal file separator ('/' or '\\').
     */
    val _l = java.io.File.separator

    /** Directory in which to place the generated code.
     */
    val DIR = s"src${_l}main${_l}scala${_l}scalation${_l}util${_l}bld"

    /** Specifications for each base type (I, L, D, R, Q, C, S).
     */
//                      MM_ARRAY     BASE    E_SIZE  BASE2     SORTING        ZERO   ONE          REGEX
    val kind = Array (("MM_ArrayI", "Int",       4, "Int",    "MM_SortingI", "0",   "1",   """"[\\-\\+]?\\d+""""),
                      ("MM_ArrayL", "Long",      8, "Long",   "MM_SortingL", "0l",  "1l",  """"[\\-\\+]?\\d+""""),
                      ("MM_ArrayD", "Double",    8, "Double", "MM_SortingD", "0.0", "1.0", """"[\\-\\+]?\\d*(\\.\\d+)?""""),
                      ("MM_ArrayR", "Real",     16, "Double", "MM_SortingR", "_0",  "_1",  """"[\\-\\+]?\\d*(\\.\\d+)?""""),
                      ("MM_ArrayQ", "Rational", 16, "Long",   "MM_SortingQ", "_0",  "_1",  """"[\\-\\+]?\\d+(/[\\-\\+]?\\d+)?""""),
                      ("MM_ArrayC", "Complex",  16, "Double", "MM_SortingC", "_0",  "_1",  """"[\\-\\+]?\\d*(\\.\\d+)?(\\+[\\-\\+]?\\d*(\\.\\d+)?)?i""""),
                      ("MM_ArrayS", "StrNum",   -1, "Int",    "MM_SortingS", "_0",  "_1",  """"\\s*""""))

    /** Set of custom base types provided by Scalation.
     *  @see scalation.math
     */
    val CUSTOM = Set ("Complex", "Rational", "Real", "StrNum")

} // BldParams trait

