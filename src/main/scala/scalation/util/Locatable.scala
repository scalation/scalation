
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Wed Feb  5 17:41:18 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Locatable` trait provides location information/coordinates for objects
 *  (e.g., `Component`s).
 */
trait Locatable extends Error
{
    /** Where this object is at (its location)
     */
    private var _at: Array [Double] = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return where this object is at (its location).
     */
    def at: Array [Double] = _at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the location of this object.
     *  @param loc  the location of this object
     */
    def setAt (loc: Array [Double])
    {
        if (_at == null && loc != null) {
            _at = loc
        } else {
            flaw ("setAt", "location may only be set once")
        } // if
    } // setAt

} // Locatable trait

