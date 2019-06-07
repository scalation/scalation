
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Santosh Uttam Bobade, John Miller
 *  @version 1.6
 *  @date    Mon Aug  6 15:31:51 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.columnar_db

import RelationSQL.from

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Query1` object is a sample object for a basic query.
 */
object Query1 extends Query
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This method implements the `doQuery` method declared in the trait Query.
     */
    override def doQuery ()
    {
        val (professor, teaching) = from ("professor", "teaching")
        (professor join teaching)
                  .where [Int] (("title", _ == 0))
                  .select ("name", "department", "title", "cname")
                  .show ()
    } // doQuery

} // Query1 object

