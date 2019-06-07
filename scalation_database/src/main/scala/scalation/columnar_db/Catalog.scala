
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Santosh Uttam Bobade
 *  @version 1.6
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.columnar_db

import collection.mutable.Map

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Catalog` object keeps track of relations in the database.
 *  It maintains two Maps: one mapping name to relation schema and the other
 *  mapping name to relation object stored in memory.
 */
object Catalog
       extends Serializable
{
    /** Map from relation-name to relation schema/meta-data
     */
    private val name2Schema = Map [String, (Seq [String], Int, String)] ()

    /** Map from relation-name to relation 
     */
    private val name2Relation = Map [String, Tabular] ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add meta-data about the new relation to the catalog.
     *  @param name     the name of the relation
     *  @param colName  the names of columns
     *  @param key      the column number for the primary key (< 0 => no primary key)
     *  @param domain   an optional string indicating domains for columns (e.g., 'SD' = 'StrNum', 'Double')
     *  @param rel      the relation
     */
    def add (name: String, colName: Seq [String], key: Int, domain: String = null, rel: Tabular = null)
    {
        name2Schema   += name -> (colName, key, domain)
        name2Relation += name -> rel
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the relation named 'name' from the catalog.
     *  @param name  the name of the relation
     */
    def remove (name: String)
    {
        name2Schema   -= name
        name2Relation -= name
    } // remove

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove all relations from the catalog.
     */
    def clear ()
    {
        name2Schema.clear ()
        name2Relation.clear ()
    } // clear

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an iterator over all relations names in the catalog.
     */
    def names: Iterable [String] = name2Schema.keys

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the schema/meta-data about the relation named 'name'.
     *  @param name  the name of the relation
     */
    def getSchema (name: String): (Seq [String], Int, String) = name2Schema getOrElse (name, null)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the relation object for the relation named 'name'.
     *  @param name  the name of the relation
     */
    def getRelation (name: String): Tabular = name2Relation getOrElse (name, null)

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

    for (nm <- names) println (nm + " -> " + getSchema (nm))

} // CatalogTest object

