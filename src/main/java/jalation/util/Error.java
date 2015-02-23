
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Tue Sep  9 17:45:50 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.util;

import static java.lang.System.out;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Error` class is used to report errors showing the class and method within
 *  which the error or flaw occurred.
 */
public class Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the flaw by printing the error message.
     *  FIX: need class-name
     *  @param method   the method where the error occurred
     *  @param message  the error message
     */
    public static void flaw (String method, String message)
    {
        out.println ("ERROR @ Class." + method +  ": " + message);
    } // flaw

} // Error class

