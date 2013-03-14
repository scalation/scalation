
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.event

import scalation.util.Identity

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class represents a single simulation entity for event-scheduling 
 *  simulation.
 *  @param creationTime  the time at which the entity is created
 *  @param name          an optional name for the entity
 */
class Entity (val creationTime: Double, name: String = "e")
      extends Identity
{
    {
        setName (name)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the entity to a string
     */
    override def toString = "Entity ( " + me + " created at " + creationTime + " )" 

} // Entity class

