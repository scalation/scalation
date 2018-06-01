
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Mar 11 15:12:46 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.Double.NaN

import scalation.linalgebra.{MatriI, MatrixI, VectoD, VectorD, VectoI, VectorI}
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConfusionMat` object provides functions for determining the confusion
 *  matrix as well as derived quality metrics such as accuracy, precsion, recall,
 *  and Cohen's kappa coefficient.
 *  @param y   the actual class labels
 *  @param yp  the precicted class labels
 *  @param k   the number class values
 */
class ConfusionMat (y: VectoI, yp: VectoI, k: Int = 2)
{
    private val conf = new MatrixI (k, k)                   // confusion matrix
    for (i <- y.range) conf(y(i), yp(i)) += 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the actual class 'y' vector versus the predicted class 'yp' vector,
     *  returning the confusion matrix, which for 'k = 2' is
     *  <p>
     *       yp  0   1
     *        ----------
     *  y  0  | tn  fp |
     *     1  | fn  tp |
     *        ----------
     *  <p>
     *  @see www.dataschool.io/simple-guide-to-confusion-matrix-terminology
     */
    def confusion: MatriI = conf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the confusion matrix for 'k = 2' as a tuple (tn, fp, fn, tp).
     */
    def pos_neg: (Double, Double, Double, Double) =
    {
        if (k == 2) {
            (conf(0, 0) /* tn */, conf(0, 1) /* fp */,
             conf(1, 0) /* fn */, conf(1, 1) /* tp */)
        } else (NaN, NaN, NaN, NaN)
    } // pos_neg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the accuracy of the classification, i.e., the fraction of correct
     *  classifications.  Note, the correct classifications 'tp_i' are in the main
     *  diagonal of the confusion matrix.
     */
    def accuracy: Double = conf.trace / conf.sum.toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute micro-precision and micro-recall, which gives the precision and
     *  recall for each class i in {0, 1, ... k}.  Also return the macro-precision
     *  and macro-recall that are simply their respective means. 
     *  The precision is the fraction classified as true, which are actually true.
     *  The recall is the fraction of the actually true, which are classified as true.
     */
    def prec_recl: (VectoD, VectoD, Double, Double) =
    {
        val prec = new VectorD (conf.dim1)              // micro-precision vector
        val recl = new VectorD (conf.dim1)              // micro-recall vector
        for (i <- conf.range1) {
            val tp    = conf(i, i)                      // true positives for class i
            val tp_fp = conf.col(i).sum                 // tp + false positives for class i
            val tp_fn = conf(i).sum                     // tp + false negatives for class i
            prec(i)   = tp / tp_fp.toDouble             // precision for class i
            recl(i)   = tp / tp_fn.toDouble             // recall for class i
        } // for
        (prec, recl, prec.mean, recl.mean)
    } // prec_recl

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the F1-measure, i.e., the harmonic mean of the precision and recall.
     *  @param prec  the precision
     *  @param recl  the recall
     */
    def f1_measure (prec: Double, recl: Double): Double = 2.0 * prec * recl / (prec + recl)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Cohen's 'kappa' coefficient that measures agreement between
     *  actual 'y' and predicted 'yp' classifications.
     *  @see en.wikipedia.org/wiki/Cohen%27s_kappa
     */
    def kappa: Double =
    {
        val freq_y  = new VectorI (k)
        val freq_yp = new VectorI (k)
        for (i <- y.range) {
            freq_y(y(i))  += 1
            freq_yp(y(i)) += 1
        } // for
        val pe = (freq_y dot freq_yp) / (y.dim * y.dim).toDouble
        val po = accuracy
        (po - pe) / (1.0 - pe)
     } // kappa

} // ConfusionMat class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConfusionMatTest` object is used to test the ConfusionMat` class.
 *  > runMain scalation.analytics.classifier.ConfusionMatTest
 */
object ConfusionMatTest extends App
{
    val y  = VectorI (0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
    val yp = VectorI (0, 0, 0, 1, 2, 0, 0, 1, 1, 2, 0, 1, 1, 1, 2)
    val k  = 3

    val cm  = new ConfusionMat (y, yp, k)
    val cnf = cm.confusion
    val kpp = cm. kappa
    val acc = cm.accuracy
    val (pv, rv, p, r) = cm.prec_recl
    val f1  = cm.f1_measure (p, r)

    println ("y            = " + y)                     // actual class values
    println ("yp           = " + yp)                    // predicted class values
    banner ("confusion matrix")
    println ("confusion    = " + cnf)                   // confusion matrix
    banner ("derived quality metrics")
    println ("accuracy     = " + acc)                   // accuracy
    println ("error-rate   = " + (1.0 - acc))           // error rate
    println ("mu-precision = " + pv)                    // micro-precision vector
    println ("mu-recall    = " + rv)                    // micro-recall vector
    println ("precision    = " + p)                     // (macro) precision
    println ("recall       = " + r)                     // (macro) recall
    println ("F1-measure   = " + f1)                    // (macro) F1-measure
    println ("kappa        = " + kpp)                   // Cohen's 'kappa' coefficient

} // ConfusionMatTest object

