
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Sep 30 12:30:42 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util.bld

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldAll` object calls all the builders to generate code for the array
 *  and sorting classes/traits in the `scalation.util` package.
 *  Note, generated files have the suffix ".scalaa".
 *------------------------------------------------------------------------------
 *  MM array classes   - general mathematical vectors
 *  MM sorting classes - base traits for several kinds of matrices
 *  sorting classes    - regular (dense) matrices
 *------------------------------------------------------------------------------
 *  To see the differences between the current code and 'new generated code',
 *  run the 'check.sh' script.
 *------------------------------------------------------------------------------
 *  To install the 'new generated code' in the package replacing the current code,
 *  run the 'install.sh' script.
 *------------------------------------------------------------------------------
 *  > run-main scalation.util.bld.BldAll
 */
object BldAll extends App
{
    BldMM_Array.main (null)           // memory mapped array classes
    BldMM_Sorting.main (null)         // memory mapped sorting classes
    BldSorting.main (null)            // regular array sorting classes

} // BldAll object

