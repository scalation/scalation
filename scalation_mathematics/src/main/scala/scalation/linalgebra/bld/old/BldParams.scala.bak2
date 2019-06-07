
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat May 30 13:51:12 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldParams` trait defines common values to be used in code generation.
 *  Base trait for numeric vectors is Vecto_, with classes Vector_, RleVector_ and
 *  SparseVector_.
 *  Base trait for matrices is Matri_, with classes BidMatrix_, Matrix_, RleMatrix_,
 *  SparseMatrix_ and SymTriMatrix_.
 */
trait BldParams
{
    /** Shorthand for universal file separator ('/' or '\\').
     */
    val _l = java.io.File.separator

    /** Directory in which to place the generated code.
     */
    val DIR = s"src${_l}main${_l}scala${_l}scalation${_l}linalgebra${_l}bld"

    /** Specifications for each base type _ = (I, L, D, R, Q, C, S).
     */
//                      VECTO     BASE        VECTOR2    BASE2     FORMAT  MATRI    SORTING     ZERO   ONE          REGEX
    val kind = Array (("VectoI", "Int",      "VectoD", "Double", "%d",   "MatriI", "SortingI", "0",   "1",   """"[\\-\\+]?\\d+""""),
                      ("VectoL", "Long",     "VectoD", "Double", "%d",   "MatriL", "SortingL", "0l",  "1l",  """"[\\-\\+]?\\d+""""),
                      ("VectoD", "Double",   "VectoI", "Int",    "%g",   "MatriD", "SortingD", "0.0", "1.0", """"[\\-\\+]?\\d*(\\.\\d+)?""""),
                      ("VectoR", "Real",     "VectoD", "Double", "%s",   "MatriR", "SortingR", "_0",  "_1",  """"[\\-\\+]?\\d*(\\.\\d+)?""""),
                      ("VectoQ", "Rational", "VectoD", "Double", "%s",   "MatriQ", "SortingQ", "_0",  "_1",  """"[\\-\\+]?\\d+(/[\\-\\+]?\\d+)?""""),
                      ("VectoC", "Complex",  "VectoD", "Double", "%s",   "MatriC", "SortingC", "_0",  "_1",  """"[\\-\\+]?\\d*(\\.\\d+)?(\\+[\\-\\+]?\\d*(\\.\\d+)?)?i""""),
                      ("VectoS", "StrNum",   "VectoI", "Int",    "%s",   "MatriS", "SortingS", "_0",  "_1",  """"\\s*""""))

    /** Set of custom base types provided by Scalation.
     *  @see scalation.math
     */
    val CUSTOM = Set ("Complex", "Rational", "Real", "StrNum")

} // BldParams trait

