
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun May 17 15:07:22 EDT 2015
 *  @see     LICENSE (MIT style license file)
 */

package scalation.scala3d

import javafx.scene._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PhongMaterial` object provides factory methods for building materials
 *  shaded according the Phong reflection model.
 */
object PhongMaterial
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build Phong material based on the given diffuse and specular colors
     *  @param diffuseColor   the color light reflected at many angles
     *  @param specularColor  the color of the mirror-like reflection
     */
    def apply (diffuseColor: paint.Color, specularColor: paint.Color) =
    {
        val mat = new paint.PhongMaterial ()
        mat.setDiffuseColor (diffuseColor)
        mat.setSpecularColor (specularColor)
        mat
    } // apply

} // PhongMaterial object

