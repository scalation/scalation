
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Santosh Uttam Bobade, John Miller
 *  @version 1.6
 *  @date    Sat Jun 9 14:09:25 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.columnar_db

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CreateTable1` object is used to create two tables: 'professor' and 'teaching'.
 */
object CreateTable1
{
    RelationSQL ("professor",
        Seq ("pid", "name", "department", "title"),
        Seq (Vector [Any] (1, "jackson", "pharm", 4),
             Vector [Any] (2, "ken",     "cs",    2),
             Vector [Any] (3, "pan",     "pharm", 0),
             Vector [Any] (4, "yang",    "gis",   3),
             Vector [Any] (5, "zhang",   "cs",    0),
             Vector [Any] (6, "Yu",      "cs",    0)),
        -1, "ISSI")

    RelationSQL ("teaching",
        Seq ("cid", "cname", "cdept", "pid"),
        Seq (Vector [Any] ("CSCI6470", "DBMS", "cs", 1),
             Vector [Any] ("CSCI6460", "AI",   "ai", 2),
             Vector [Any] ("CSCI6970", "DS",   "cs", 3),
             Vector [Any] ("CSCI8470", "DCS",  "cs", 4)),
        -1, "SSSI")

} // CreateTable1 object

