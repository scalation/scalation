
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.columnar_db

import collection.mutable.Map

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Catalog` object keeps track of relations in the database.
 */
object Catalog
{
    /** Map from relation-name to meta-data
     */
    private val relMap = Map [String, Tuple3 [Seq [String], Int, String]] ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add meta-data about the new relation to the catalog.
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param col      the Scala Vector of columns making up the columnar relation
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     */
    def add (name: String, colName: Seq [String], key: Int, domain: String)
    {
        relMap += name -> (colName, key, domain)
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the relation named 'name' from the catalog.
     *  @param name     the name of the relation
     */
    def remove (name: String)
    {
        relMap -= name
    } // remove

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove all relations from the catalog.
     */
    def clear ()
    {
        relMap.clear ()
    } // clear

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an iterator over all all relations names in the catalog.
     */
    def names: Iterable [String] =
    {
        relMap.keys
    } // names

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the meta-data about the relation named 'name'.
     *  @param name     the name of the relation
     */
    def get (name: String): Tuple3 [Seq [String], Int, String] =
    {
        relMap (name)
    } // get

} // Catalog object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CatalogTest` object is used to test the `Catalog` object.
 *  > runMain scalation.columnar_db.CatalogTest
 */
object CatalogTest extends App
{
    import Catalog._

    add ("customer", Seq ("cname", "street", "ccity"), 0, "SSS")
    add ("deposit",  Seq ("bname", "accno", "cname", "balance"), 1, "SISD")
    add ("branch",   Seq ("bname", "assets", "bcity"), 0, "SDS")
    add ("borrow",   Seq ("bname", "loanno", "cname", "amount"), 0, "SISD")

    for (nm <- names) println (nm + " -> " + get (nm))

} // CatalogTest object

