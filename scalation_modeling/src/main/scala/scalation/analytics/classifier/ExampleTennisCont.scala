
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Feb 16 16:14:34 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scalation.linalgebra.{MatrixD, MatrixI, VectoI}
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleTennisCont` object is used to test all integer based classifiers.
 *  This is the well-known classification problem on whether to play tennis
 *  based on given weather conditions.  Applications may need to slice 'xy'.
 *  The 'Cont' version uses continuous values for Temperature and Humidity.
 *  <p>
 *      val x = xy.sliceCol (0, 4)      // columns 0, 1, 2, 3
 *      val y = xy.col (4)              // column 4
 *  <p>
 *  @see euclid.nmu.edu/~mkowalcz/cs495f09/slides/lesson004.pdf
 *  @see sefiks.com/2018/05/13/a-step-by-step-c4-5-decision-tree-example
 */
object ExampleTennisCont
{
    // combined data matrix [ x | y ]
    // dataset ----------------------------------------------------------------
    // x0: Outlook:     Rain (0),   Overcast (1), Sunny (2)
    // x1: Temperature: Continuous
    // x2: Humidity:    Continuous
    // x3: Wind:        Weak (0),   Strong (1)
    // y:  the response/classification decision
    // variables/features:          x0    x1     x2      x3    y
    val xy = new MatrixD ((14, 5),  2,    85,    85,     0,    0,      // day  1
                                    2,    80,    90,     1,    0,      // day  2
                                    1,    83,    78,     0,    1,      // day  3
                                    0,    70,    96,     0,    1,      // day  4
                                    0,    68,    80,     0,    1,      // day  5
                                    0,    65,    70,     1,    0,      // day  6
                                    1,    64,    65,     1,    1,      // day  7
                                    2,    72,    95,     0,    0,      // day  8
                                    2,    69,    70,     0,    1,      // day  9
                                    0,    75,    80,     0,    1,      // day 10
                                    2,    75,    70,     1,    1,      // day 11
                                    1,    72,    90,     1,    1,      // day 12
                                    1,    81,    75,     0,    1,      // day 13
                                    0,    71,    80,     1,    0)      // day 14

    val fn    = Array ("Outlook", "Temp", "Humidity", "Wind")          // feature names
    val isCon = Array (false, true, true, false)                       // continuous feature flag
    val cn    = Array ("No", "Yes")                                    // class names for y
    val k     = cn.size                                                // number of classes

    val x = xy.sliceCol (0, 4)      // columns 0, 1, 2, 3
    val y = xy.col (4).toInt        // column 4

} // ExampleTennisCont object

import ExampleTennisCont._
import Round.roundMat

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleTennisContTest` test several classifiers on the Tennis dataset.
 *  > runMain scalation.analytics.classifier.ExampleTennisContTest
 */
object ExampleTennisContTest extends App
{
    var yp: VectoI = null

    banner ("NullModel")
    val nm = new NullModel (y)
    nm.train ()
    yp = nm.classify (x)
    println ("fitMap = " + nm.fitMap (y, yp))
    println ((new ConfusionMat (y, yp, 2)).confusion)

    banner ("NaiveBayesR")
    val nb = new NaiveBayesR (x, y)
    nb.train ()
    yp = nb.classify (x)
    println ("fitMap = " + nb.fitMap (y, yp))
    println ((new ConfusionMat (y, yp, 2)).confusion)

/*
    banner ("TANBayes")
    val tan = new TANBayes (x, y)
    tan.train ()
    yp = tan.classify (x)
    println ("fitMap = " + tan.fitMap (y, yp))
    println ((new ConfusionMat (y, yp, 2)).confusion)
*/

    banner ("LogisticRegression")
    val lrg = new LogisticRegression (x, y)
    lrg.train ()
    yp = lrg.classify (x)
    println ("fitMap = " + lrg.fitMap (y, yp))
    println ((new ConfusionMat (y, yp, 2)).confusion)

    banner ("LDA")
    val lda = new LDA (x, y)
    lda.train ()
    yp = lda.classify (x)
    println ("fitMap = " + lda.fitMap (y, yp))
    println ((new ConfusionMat (y, yp, 2)).confusion)

    banner ("KNN_Classifier")
    val knn = new KNN_Classifier (x, y)
    knn.train ()
    yp = knn.classify (x)
    println ("fitMap = " + knn.fitMap (y, yp))
    println ((new ConfusionMat (y, yp, 2)).confusion)

    banner ("DecisionTreeC45")
    val dtc = new DecisionTreeC45 (x, y, isCon)
    dtc.train ()
    yp = dtc.classify (x)
    println ("fitMap = " + dtc.fitMap (y, yp))
    println ((new ConfusionMat (y, yp, 2)).confusion)

} // ExampleTennisContTest object

