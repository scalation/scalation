
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
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
    /** Get the location where this object is currently at.
     */
    def at: Array [Double] = _at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the location of this object.
     *  @param at  the location of this object
     */
    def at_= (at: Array [Double])
    {
        if (_at == null && at != null) {
            _at = at
        } else {
            flaw ("at_=", "location may only be set once")
        } // if
    } // at_=

} // Locatable trait

