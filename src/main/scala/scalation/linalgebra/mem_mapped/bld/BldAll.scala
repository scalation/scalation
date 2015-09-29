
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Sep 29 18:12:27 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.mem_mapped.bld

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldAll` object calls all the builders to generate code for the vector
 *  and matrix classes/traits in the `scalation.linalgbra` package.
 *  Note, generated files have the suffix ".scalaa".
 *------------------------------------------------------------------------------
 *  vector classes        - general mathematical vectors
 *  matrix traits         - base traits for several kinds of matrices
 *  matrix classes        - regular (dense) matrices
 *------------------------------------------------------------------------------
 *  To see the differences between the current code and 'new generated code',
 *  run the 'check.sh' script.
 *------------------------------------------------------------------------------
 *  To install the 'new generated code' in the package replacing the current code,
 *  run the 'install.sh' script.
 *------------------------------------------------------------------------------
 *  > run-main scalation.linalgebra.mem_mapped.bld.BldAll
 */
object BldAll extends App
{
    BldVector.main (null)             // build vector classes
    BldMatri.main (null)              // build matrix traits
    BldMatrix.main (null)             // build matrix classes

} // BldAll object

