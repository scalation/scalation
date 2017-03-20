
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Tue Sep  15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Identifiable` trait provides unique identification for simulation components,
 *  entities and events.  Includes a mandatory id and an optional name.
 */
trait Identifiable extends Error
{
    import Identifiable.next

    /** The globally unique integer identifier
     */
    val id = next ()

    /** The given name (assigned once)
     */
    private var _name = ""

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the name.
     */
    def name = _name

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the name.
     *  @param name  the name to assign
     */
    def name_= (name: String)
    {
        if (_name == "" && name != null) {
            _name = name
        } else {
            flaw ("name_=", "name may only be set once")
        } // if
    } // name_=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the type of the simulation object.
     */
    def simType = getClass.getSimpleName ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the full identity.
     */
    def me = simType + "." + _name + "." + id

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether Identifiable object 'this' equals Identifiable object 'that'.
     *  Works since 'id' is unique for all Identifiable objects.
     */
    override def equals (that: Any): Boolean = 
    {
        that match {
            case that: Identifiable => this.id == that.id
            case _ => false
        } // match
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hashCode as the unique id.
     */
    override def hashCode: Int = id

} // Identifiable trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Identifiable` object is used to generate unique identifiers.
 */
object Identifiable
{
    /** Used for counter
     */
    private var i = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the next value from the counter.
     */
    def next () = { i += 1; i }

} // Identifiable object

