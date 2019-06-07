
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Thu Aug  2 21:20:55 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  An implementation supporting columnar relational databases facilitating easy
 *  and rapid analytics.  The columns in a relation are vectors from the
 *  `scalation.linalgebra` package.  Vectors and matrices may be readily extracted
 *  from a relation and feed into any of the numerous analytics techniques provided
 *  in `scalation.analytics`.  The implementation provides most of the columnar
 *  relational algebra operators given in the following paper:
 *  @see db.csail.mit.edu/projects/cstore/vldb.pdf
 *
 *  Some of the operators have Unicode versions: @see `scalation.util.UnicodeTest`
 */

package scalation.columnar_db

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tabular` trait is the base for all the relational structures defined
 *  in this package.
 *  FIX - factor out additional methods the apply universally to Relation, RelationF, RelationSQL
 */
trait Tabular
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show 'this' relation row by row.
     *  @param limit  the limit on the number of tuples to display
     */
    def show (limit: Int = Int.MaxValue)

} // Tabular trait

