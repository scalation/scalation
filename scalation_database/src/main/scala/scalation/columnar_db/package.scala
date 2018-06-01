
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Yang Fan
 *  @version 1.5
 *  @date    Tue Apr 24 16:28:29 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import linalgebra.Vec

/** The `columar_db` package contains classes, traits and objects for
 *  columnar relational database, where columns are vectors from the
 *  `linalgebra` package.  Basic type definitions are given.
 */
package object columnar_db
{
   /** Boolean function that uses the value for the given column name (String)
    *  in the predicate (e.g., used by 'where' and 'filter')
    */
   type Predicate [T] = (String, T => Boolean)

   /** Boolean function that uses the values for two given column names (String, String)
    *  in the predicate (e.g., used by 'thetajoin')
    */
   type Predicate2 [T] = (String, String, (T, T) => Boolean)

   /** Indicates which relation and which column an aggregate is to be applied to
    */
   type AggFunction = (Relation, String) => Vec

} // columnar_db package object 

