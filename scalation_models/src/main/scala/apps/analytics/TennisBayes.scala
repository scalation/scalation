
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Sep 25 19:24:11 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package apps.analytics

import scalation.analytics.classifier.NaiveBayes
import scalation.linalgebra.{MatrixI, VectorI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TennisBayes` object is a sample application that uses the `NaiveBayes`
 *  class.  Classify (No/Yes) whether a person will play tennis based on the
 *  measured integer-valued features.
 *  @see suanpalm3.kmutnb.ac.th/teacher/FileDL/choochart82255418560.pdf
 *  > runMain apps.analytics.TennisBayes
 */
object TennisBayes extends App
{
    // training-set -----------------------------------------------------------
    // x0: Outlook:     Rain (0),   Overcast (1), Sunny (2)
    // x1: Temperature: Cold (0),   Mild (1),     Hot (2)
    // x2: Humidity:    Normal (0), High (1)
    // x3: Wind:        Weak (0),   Strong (1)
    // features:                   x0     x1     x2     x3
    val x  = new MatrixI ((14, 4),  2,     2,     1,     0,       // day  1 - data matrix
                                    2,     2,     1,     1,       // day  2
                                    1,     2,     1,     0,       // day  3
                                    0,     1,     1,     0,       // day  4
                                    0,     0,     0,     0,       // day  5
                                    0,     0,     0,     1,       // day  6
                                    1,     0,     0,     1,       // day  7
                                    2,     1,     1,     0,       // day  8
                                    2,     0,     0,     0,       // day  9
                                    0,     1,     0,     0,       // day 10
                                    2,     1,     0,     1,       // day 11
                                    1,     1,     1,     1,       // day 12
                                    1,     2,     0,     0,       // day 13
                                    0,     1,     1,     1)       // day 14
    // day:           1  2  3  4  5  6  7  8  9 10 11 12 13 14
    val y  = VectorI (0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0)   // classification vector: 0(No), 1(Yes))
    val cn = Array ("No", "Yes")                                  // class names
    val fn = Array ("Outlook", "Temp", "Humidity", "Wind")        // feature names
    val vc = Array (3, 3, 2, 2)                                   // value count: distinct values for each feature

    val nb = new NaiveBayes (x, y, fn, 2, cn, vc)                // create a classifier
  
    nb.train ()
    val z = VectorI (2, 2, 1, 1)                                  // new data vector to classify
    println ("classify (" + z + ") = " + nb.classify (z))

} // TennisBayes object

