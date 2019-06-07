
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Santosh Uttam Bobade, John Miller
 *  @version 1.6
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */
package scalation.columnar_db

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Query` trait specifies operations such as doQuery and doUpdate that can
 *  be performed on Relations.
 */
trait Query extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perfrom the compiled query statement.
     */
    def doQuery ()
    {
        flaw ("Query.doQuery", "must override function in implementing object")
    } // doQuery

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perfrom the compiled update statement.
     */
    def doUpdate ()
    {
        flaw ("Query.doUpdate", "must override function in implementing object")
    } // doUpdate

} // Query trait

