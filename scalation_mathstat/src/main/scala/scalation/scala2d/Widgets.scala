
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Tue May 13 16:18:42 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 * Intended to make switching GUI's easier.
 */

package scalation.scala2d

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Frame` is a convenience class for `JFrame`.
 */
class Frame (title: String) extends javax.swing.JFrame (title)
{
    setDefaultCloseOperation (javax.swing.JFrame.EXIT_ON_CLOSE);
} // Frame

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Panel` is a convenience class for `JPanel`.
 */
class Panel extends javax.swing.JPanel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `ScrollPane` is a convenience class for `JScrollPane`.
 */
class ScrollPane (table: Table) extends javax.swing.JScrollPane (table)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Table` is a convenience class for `JTable`.
 */
class Table (rows: Int, cols: Int) extends javax.swing.JTable (rows, cols)

