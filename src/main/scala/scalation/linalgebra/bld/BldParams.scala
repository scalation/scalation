
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat May 30 13:51:12 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

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
    val DIR = s"src${_l}main${_l}scala${_l}scalation${_l}linalgebra${_l}bld"

    /** Specifications for each base type (I, L, D, R, Q, C, S).
     */
//                      VECTOR     BASE        VECTOR2    BASE2     FORMAT  MATRI    SORTING     ZERO   ONE          REGEX
    val kind = Array (("VectorI", "Int",      "VectorD", "Double", "%d",   "MatriI", "SortingI", "0",   "1",   """"[\\-\\+]?\\d+""""),
                      ("VectorL", "Long",     "VectorD", "Double", "%d",   "MatriL", "SortingL", "0l",  "1l",  """"[\\-\\+]?\\d+""""),
                      ("VectorD", "Double",   "VectorI", "Int",    "%g",   "MatriD", "SortingD", "0.0", "1.0", """"[\\-\\+]?\\d*(\\.\\d+)?""""),
                      ("VectorR", "Real",     "VectorD", "Double", "%s",   "MatriR", "SortingR", "_0",  "_1",  """"[\\-\\+]?\\d*(\\.\\d+)?""""),
                      ("VectorQ", "Rational", "VectorD", "Double", "%s",   "MatriQ", "SortingQ", "_0",  "_1",  """"[\\-\\+]?\\d+(/[\\-\\+]?\\d+)?""""),
                      ("VectorC", "Complex",  "VectorD", "Double", "%s",   "MatriC", "SortingC", "_0",  "_1",  """"[\\-\\+]?\\d*(\\.\\d+)?(\\+[\\-\\+]?\\d*(\\.\\d+)?)?i""""),
                      ("VectorS", "StrNum",   "VectorI", "Int",    "%s",   "MatriS", "SortingS", "_0",  "_1",  """"\\s*""""))

    /** Set of custom base types provided by Scalation.
     *  @see scalation.math
     */
    val CUSTOM = Set ("Complex", "Rational", "Real", "StrNum")

} // BldParams trait

