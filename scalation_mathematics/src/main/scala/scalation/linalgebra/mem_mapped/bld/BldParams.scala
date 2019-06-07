
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Sep 27 18:29:28 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.mem_mapped.bld

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
    val DIR = s"src${_l}main${_l}scala${_l}scalation${_l}linalgebra${_l}mem_mapped${_l}bld"

    /** Specifications for each base type (I, L, D, R, Q, C, S).
     */
//                      VECTOR     BASE        MM_Array     VECTOR2    BASE2     FORMAT  MATRI     SORTING       ZERO   ONE          REGEX
    val kind = Array (("VectorI", "Int",      "MM_ArrayI", "VectorD", "Double", "%d",   "MatriI", "MM_SortingI", "0",   "1",   """"[\\-\\+]?\\d+""""),
                      ("VectorL", "Long",     "MM_ArrayL", "VectorD", "Double", "%d",   "MatriL", "MM_SortingL", "0l",  "1l",  """"[\\-\\+]?\\d+""""),
                      ("VectorD", "Double",   "MM_ArrayD", "VectorI", "Int",    "%g",   "MatriD", "MM_SortingD", "0.0", "1.0", """"[\\-\\+]?\\d*(\\.\\d+)?""""),
                      ("VectorR", "Real",     "MM_ArrayR", "VectorD", "Double", "%s",   "MatriR", "MM_SortingR", "_0",  "_1",  """"[\\-\\+]?\\d*(\\.\\d+)?""""),
                      ("VectorQ", "Rational", "MM_ArrayQ", "VectorD", "Double", "%s",   "MatriQ", "MM_SortingQ", "_0",  "_1",  """"[\\-\\+]?\\d+(/[\\-\\+]?\\d+)?""""),
                      ("VectorC", "Complex",  "MM_ArrayC", "VectorD", "Double", "%s",   "MatriC", "MM_SortingC", "_0",  "_1",  """"[\\-\\+]?\\d*(\\.\\d+)?(\\+[\\-\\+]?\\d*(\\.\\d+)?)?i""""),
                      ("VectorS", "StrNum",   "MM_ArrayS", "VectorI", "Int",    "%s",   "MatriS", "MM_SortingS", "_0",  "_1",  """"\\s*""""))

    /** Set of custom base types provided by Scalation.
     *  @see scalation.math
     */
    val CUSTOM = Set ("Complex", "Rational", "Real", "StrNum")

} // BldParams trait

