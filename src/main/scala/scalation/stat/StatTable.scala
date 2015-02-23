
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Wed Feb 26 18:20:39 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import collection.mutable.ListBuffer

import scalation.scala2d.{Frame, ScrollPane, Table}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatTable` class display statistical results in a frame's table.
 *  @param _title  the title of the frame
 *  @param stats   the statistics to be displayed in the table.
 */
class StatTable (_title: String, stats: ListBuffer [Statistic])
      extends Frame (_title)
{
    val rows  = stats.size + 1
    val cols  = Statistic.label.length
    val table = new Table (rows, cols)
    for (j <- 0 until cols) table.setValueAt (Statistic.label(j), 0, j)
    for (i <- 0 until rows - 1) {
        val st = stats(i).statRow
        for (j <- 0 until cols) table.setValueAt (st(j), i+1, j)
    } // for

    getContentPane.add (new ScrollPane (table))
    pack ()
    setVisible (true)

} // StatTable class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatTableTest` is used to test the `StatTable` class.
 */
object StatTableTest extends App
{
     println ("Create a StatTable called Test")
     val stats = ListBuffer [Statistic] ()
     for (i <- 0 until 50) stats += new Statistic ()
     for (j <- 0 until 50) {
         for (i <- 0 until 50) stats(j).tally (i)
     } // for
     new StatTable ("Test", stats)

} // StatTableTest object

