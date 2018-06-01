
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sat Aug  8 20:26:34 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PackageInfo` trait provides methods to retrieve meta-data about packages.
 */
trait PackageInfo
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the current (calling context) package name.
     */
    def getPackageName: String = getClass.getPackage.getName

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the default data path for the current package.
     */
    def getDataPath: String =
    {
        getPackageName.replace ("scalation", "data").replace (".", ⁄) + ⁄
    } // gatDataPath

} // PackageInfo trait

