
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Fri Sep 25 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Error` trait is used to report errors showing the class and method within
 *  which the error or flaw occurred.
 */
trait Error
{
    /** Name of the class where the error occurred
     */
    private val className = getClass.getSimpleName ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the flaw by printing the error message.
     *  @param method   the method where the error occurred
     *  @param message  the error message
     */
    def flaw (method: String, message: String)
    {
        println ("ERROR @ " + className + "." + method +  ": " + message)
    } // flaw

} // Error trait

