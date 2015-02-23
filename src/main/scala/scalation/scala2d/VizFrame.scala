
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Thu May 15 15:18:39 EDT 2014
 *  @see     LICENSE (MIT style license file). 
 */

package scalation.scala2d

import scalation.scala2d.Shapes.Dimension

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::s
/** The `VizFrame` class puts the canvas in the visualization/drawing frame.
 *  @param title   the title for the frame
 *  @param canvas  the drawing canvas
 *  @param w       the width of the frame
 *  @param h       the height of the frame
 *  @param o       the offset of the frame
 */
class VizFrame (title: String, canvas: Panel, w: Int = 700, h: Int = 700, o: Int = 100)
      extends Frame (title)
{
    println ("Run + title")
    setLocation (o, o)
    setSize (new Dimension (w, h))
    if (canvas != null) {                     // may need to set these later
        getContentPane ().add (canvas)
        setVisible (true)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::s
    /** Get the width of the frame.
     */
    def getW: Int = w

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::s
    /** Get the height of the frame.
     */
    def getH: Int = h

} // VizFrame class

