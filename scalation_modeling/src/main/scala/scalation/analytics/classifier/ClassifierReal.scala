
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.util.control.Breaks.{break, breakable}

import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectoI, VectorI}
import scalation.stat.vectorD2StatVector
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClassifierReal` abstract class provides a common foundation for several
 *  classifiers that operate on real-valued data.
 *  @param x   the real-valued training/test data vectors stored as rows of a matrix
 *  @param y   the training/test classification vector, where y_i = class for row i of the matrix x
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
abstract class ClassifierReal (x: MatriD, y: VectoI, fn: Array [String], k: Int,
                               cn: Array [String])
         extends Classifier with Error
{
    /** the number of data vectors in training-set (# rows)
     */
    protected val m = x.dim1

    /** the number of features/variables (# columns)
     */
    protected val n = x.dim2

    /** the training-set size as a Double
     */
    protected val md = m.toDouble

    /** the feature-set size as a Double
     */
    protected val nd = n.toDouble

    /** the set of features to turn on or off. All features are on by default.
     *  Used for feature selection.
     */
    protected val fset = Array.fill [Boolean](n)(true)

    if (y.dim != m)     flaw ("constructor", "y.dim must equal training-set size (m)")
    if (fn != null && fn.length != n) flaw ("constructor", "fn.length must equal feature-set size (n)")
    if (k >= m)         flaw ("constructor", "k must be less than training-set size (m)")
    if (cn.length != k) flaw ("constructor", "cn.length must equal number of classes (k)")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return default values for binary input data (value count 'vc' set to 2).
     *  Also may be used for binning into two categories.
     */
    def vc_default: Array [Int] = Array.fill (n)(2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of data vectors in training/test-set (# rows).
     */
    def size: Int = m

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete (integer-valued) data vector 'z', determine which
     *  class it belongs to, by first converting it to a vector of doubles.
     *  Return the best class, its name and its relative probability
     *  @param z  the vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) = classify (z.toDouble)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify all of the row vectors in matrix 'xx'.
     *  @param xx  the row vectors to classify
     */
    def classify (xx: MatriD): VectoI =
    {
        VectorI (for (i <- xx.range1) yield classify (xx(i))._1)
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param itest  indices of the instances considered test data
     */
    def test (itest: IndexedSeq [Int]): Double =
    {
        var correct = 0
        for (i <- itest if classify (x(i))._1 == y(i)) correct += 1
        correct / itest.size.toDouble
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param xx  the real-valued test vectors stored as rows of a matrix
     *  @param yy  the test classification vector, where 'yy_i = class for row i of xx'
     */
    def test (xx: MatriD, yy: VectoI): Double =
    {
        val mm = xx.dim1
        if (yy.dim != mm) flaw ("test", "yy.dim must equal test-set size (mm)")
        var correct = 0
        for (i <- 0 until mm if classify (xx(i))._1 == yy(i)) correct += 1
        correct / mm.toDouble
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the correlation matrix for the feature vectors 'fea'.
     *  If the correlations are too high, the independence assumption may be dubious.
     */
    def calcCorrelation: MatriD =
    {
        val fea = for (j <- 0 until n) yield x.col(j).toDense
        val cor = new MatrixD (n, n)
        for (j1 <- 0 until n; j2 <- 0 until j1) cor(j1, j2) = fea(j1) corr fea(j2)
        cor
    } // calcCorrelation

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the correlation matrix for the feature vectors of Z (Level 3)
     *  and those of X (level 2).
     *  If the correlations are too high, the independence assumption may be dubious.
     *  @param zrg  the range of Z-columns
     *  @param xrg  the range of X-columns
     */
    def calcCorrelation2 (zrg: Range, xrg: Range): MatriD =
    {
        val zfea = for (j <- zrg) yield x.col(j).toDense
        val xfea = for (j <- xrg) yield x.col(j).toDense
        val cor = new MatrixD (zfea.size, xfea.size)
        for (j1 <- 0 until cor.dim1; j2 <- 0 until cor.dim2) cor(j1, j2) = zfea(j1) corr xfea(j2)
        cor
    } // calcCorrelation2

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform feature selection on the classifier. Use backward elimination
     *  technique, that is, remove the least significant feature, in terms of cross-
     *  validation accuracy, in each round.
     *  @param TOL  tolerance indicating negligible accuracy loss when removing features
     */
    def featureSelection (TOL: Double = 0.01)
    {
        val DEBUG = false

        var accuracy = crossValidateRand ()
        if (DEBUG) println ("Initial accuracy with no feature removed: " + accuracy)

        // keep removing one feature at a time until no more feature should be removed
        breakable { while (true) {
            var minDiff  = 1.0
            var toRemove = -1
            if (DEBUG) println ("Try to remove each feature and achieve best accuracy...")

            for (j <- 0 until n if fset(j)) {
                if (DEBUG) println ("Test by temporarily removing feature " + j)
                fset(j) = false
                val currentAccu = crossValidateRand ()
                val accuracyDiff = accuracy - currentAccu
                if (accuracyDiff < minDiff) {               // search for the feature with minimal impact on cv accuracy
                    minDiff  = accuracyDiff
                    accuracy = currentAccu
                    toRemove = j
                } // if
                fset(j) = true
            } // for

            //only remove the feature if the minimum accuracy drop is less than a small TOL value (acceptable accuracy reduction)
            if (minDiff < TOL && toRemove > -1) {
                fset(toRemove) = false
                if (DEBUG) {
                    println ("Feature " + toRemove + " has been removed.")
                    println ("The new accuracy is " + accuracy + " after removing feature " + toRemove)
                }
            } else {
                if (DEBUG) println ("No more features can/should be removed.")
                break
            } // if
        }} // breakable while

        val remained = new StringBuilder ()
        val removed  = new StringBuilder ()
        for (j <- 0 until n) if (fset(j)) remained append s"$j " else removed append s"$j "
        println ("The following features have remained: " + remained)
        println ("The following features were removed: " + removed)
        if (DEBUG) println ("NOTE: The classifier must be re-trained before classifying any instances.")
    } // featureSelection

} // ClassifierReal abstract class

