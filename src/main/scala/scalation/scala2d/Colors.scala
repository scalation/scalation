
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep  21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.scala2d

import scalation.random.Randi
 
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A convenience object defining numerous common colors.
 *  Source of colors:  http://www.webmoments.com/colorchart.htm
 */
object Colors
{
    type Color = java.awt.Color

    val antiquewhite     = new Color (0xFAEBD7)
    val aqua             = new Color (0x00FFFF)
    val aquamarine       = new Color (0x7FFFD4)
    val azure            = new Color (0xF0FFFF)
    val beige            = new Color (0xF5F5DC)
    val bisque           = new Color (0xFFE4C4)
    val black            = new Color (0x000000)
    val blanchedalmond   = new Color (0xFFEBCD)
    val blue             = new Color (0x0000FF)
    val blueviolet       = new Color (0x8A2BE2)
    val brightskyblue    = new Color (0xF0F9FF)
    val brown            = new Color (0xA52A2A)
    val burlywood        = new Color (0xDEB887)
    val cadetblue        = new Color (0x5F9EA0)
    val chartreuse       = new Color (0x7FFF00)
    val chocolate        = new Color (0xD2691E)
    val coral            = new Color (0xFF7F50)
    val cornflowerblue   = new Color (0x6495ED)
    val cornsilk         = new Color (0xFFF8DC)
    val crimson          = new Color (0xDC143C)
    val cyan             = new Color (0x00FFFF)
    val darkblue         = new Color (0x00008B)
    val darkcyan         = new Color (0x008B8B)
    val darkgoldenrod    = new Color (0xB8860B)
    val darkgray         = new Color (0xA9A9A9)
    val darkgreen        = new Color (0x006400)
    val darkkhaki        = new Color (0xBDB76B)
    val darkmagenta      = new Color (0x8B008B)
    val darkolivegreen   = new Color (0x556B2F)
    val darkorange       = new Color (0xFF8C00)
    val darkorchid       = new Color (0x9932CC)
    val darkred          = new Color (0x8B0000)
    val darksalmon       = new Color (0xE9967A)
    val darkseagreen     = new Color (0x8FBC8F)
    val darkslateblue    = new Color (0x483D8B)
    val darkslategray    = new Color (0x2F4F4F)
    val darkturquoise    = new Color (0x00CED1)
    val darkviolet       = new Color (0x9400D3)
    val darkyellow       = new Color (0xEEEE00)
    val deeppink         = new Color (0xFF1493)
    val deepskyblue      = new Color (0x00BFFF)
    val dimgray          = new Color (0x696969)
    val dodgerblue       = new Color (0x1E90FF)
    val firebrick        = new Color (0xB22222)
    val floralwhite      = new Color (0xFFFAF0)
    val forestgreen      = new Color (0x228B22)
    val fuchsia          = new Color (0xFF00F0)
    val gainsboro        = new Color (0xDCDCDC)
    val ghostwhite       = new Color (0xF8F8FF)
    val gold             = new Color (0xFFD700)
    val goldenrod        = new Color (0xDAA520)
    val gray             = new Color (0x808080)
    val green            = new Color (0x008000)
    val greenyellow      = new Color (0xADFF2F)
    val honeydew         = new Color (0xF0FFF0)
    val hotpink          = new Color (0xFF69B4)
    val indianred        = new Color (0xCD5C5C)
    val indigo           = new Color (0x4B0082)
    val ivory            = new Color (0xFFFFF0)
    val khaki            = new Color (0xF0E68C)
    val lavender         = new Color (0xE6E6FA)
    val lavenderblush    = new Color (0xFFF0F5)
    val lawngreen        = new Color (0x7CFC00)
    val lemonchiffon     = new Color (0xFFFACD)
    val lemonlime        = new Color (0xBACD22)
    val lightblue        = new Color (0xADD8E6)
    val lightcoral       = new Color (0xF08080)
    val lightcyan        = new Color (0xE0FFFF)
    val tgoldenrodyellow = new Color (0xFAFAD2)
    val lightgreen       = new Color (0x90EE90)
    val lightgrey        = new Color (0xD3D3D3)
    val lightpink        = new Color (0xFFB6C1)
    val lightred         = new Color (0xFF7755)
    val lightsalmon      = new Color (0xFFA07A)
    val lightseagreen    = new Color (0x20B2AA)
    val lightskyblue     = new Color (0x87CEFA)
    val lightslategray   = new Color (0x778899)
    val lightsteelblue   = new Color (0xB0C4DE)
    val lightyellow      = new Color (0xFFFFE0)
    val lime             = new Color (0x00FF00)
    val limegreen        = new Color (0x32CD32)
    val linen            = new Color (0xFAF0E6)
    val magenta          = new Color (0xFF00FF)
    val maroon           = new Color (0x800000)
    val mediumaquamarine = new Color (0x66CDAA)
    val mediumblue       = new Color (0x0000CD)
    val mediumorchid     = new Color (0xBA55D3)
    val mediumpurple     = new Color (0x9370DB)
    val mediumseagreen   = new Color (0x3CB371)
    val mediumslateblue  = new Color (0x7B68EE)
    val ediumspringgreen = new Color (0x00FA9A)
    val mediumturquoise  = new Color (0x48D1CC)
    val mediumvioletred  = new Color (0xC71585)
    val midnightblue     = new Color (0x191970)
    val mintcream        = new Color (0xF5FFFA)
    val mistyrose        = new Color (0xFFE4E1)
    val moccasin         = new Color (0xFFE4B5)
    val navajowhite      = new Color (0xFFDEAD)
    val navy             = new Color (0x000080)
    val oldlace          = new Color (0xFDF5E6)
    val olive            = new Color (0x808000)
    val olivedrab        = new Color (0x6B8E23)
    val orange           = new Color (0xFFA500)
    val orangered        = new Color (0xFF4500)
    val orchid           = new Color (0xDA70D6)
    val palegoldenrod    = new Color (0xEEE8AA)
    val palegreen        = new Color (0x98FB98)
    val paleturquoise    = new Color (0xAFEEEE)
    val palevioletred    = new Color (0xDB7093)
    val papayawhip       = new Color (0xFFEFD5)
    val peachpuff        = new Color (0xFFDAB9)
    val peru             = new Color (0xCD853F)
    val pink             = new Color (0xFFC0CB)
    val plum             = new Color (0xDDA0DD)
    val powderblue       = new Color (0xB0E0E6)
    val purple           = new Color (0x800080)
    val red              = new Color (0xFF0000)
    val rosybrown        = new Color (0xBC8F8F)
    val royalblue        = new Color (0x4169E1)
    val saddlebrown      = new Color (0x8B4513)
    val salmon           = new Color (0xFA8072)
    val sandybrown       = new Color (0xF4A460)
    val seagreen         = new Color (0x2E8B57)
    val seashell         = new Color (0xFFF5EE)
    val sienna           = new Color (0xA0522D)
    val silver           = new Color (0xC0C0C0)
    val skyblue          = new Color (0x87CEEB)
    val slateblue        = new Color (0x6A5ACD)
    val slategray        = new Color (0x708090)
    val snow             = new Color (0xFFFAFA)
    val springgreen      = new Color (0x00FF7F)
    val steelblue        = new Color (0x4682B4)
    val tan              = new Color (0xD2B48C)
    val teal             = new Color (0x008080)
    val thistle          = new Color (0xD8BFD8)
    val tomato           = new Color (0xFF6347)
    val turquoise        = new Color (0x40E0D0)
    val violet           = new Color (0xEE82EE)
    val wheat            = new Color (0xF5DEB3)
    val white            = new Color (0xFFFFFF)
    val whitesmoke       = new Color (0xF5F5F5)
    val yellow           = new Color (0xFFFF00)
    val yellowgreen      = new Color (0x9ACD32)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Based upon an object's id, generate (somewhat randomly) a color.
     *  @param id  the identifier/index for some object
     */
    def randomColor (id: Int) =
    {
        val rc = Randi (0x1F, 0xB7)
        id % 6 match {
            case 0 => new Color (0xF7,    rc.igen, rc.igen)
            case 1 => new Color (0xF7,    0xF7,    rc.igen)
            case 2 => new Color (rc.igen, 0xF7,    rc.igen)
            case 3 => new Color (rc.igen, 0xF7,    0xF7)
            case 4 => new Color (rc.igen, rc.igen, 0xF7)
            case _ => new Color (0xF7,    rc.igen, 0xF7)
        } // match
    } // randomColor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Based upon an object's id, generate (somewhat randomly) a color.
     *  @param id  the identifier/index for some object
     *
    def randomColor (id: Int) =
    {
        rc = Randi (0x1F, 0xF7)
        id % 3 match {
            case 0 => new Color (255, (31 * id) % 224, 0)
            case 1 => new Color (255, 0, (31 * id) % 256)
            case _ => new Color (32 + (31 * id) % 224, 0, 255)
        } // match
    } // randomColor
     */

} // Colors object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Colors object.
 */
object ColorsTest extends App
{
    import Colors.randomColor
    for (i <- 0 until 20) println ("color " + i + " = " + randomColor (i))

} // ColorsTest object

