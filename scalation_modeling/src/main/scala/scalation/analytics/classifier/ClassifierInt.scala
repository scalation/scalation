
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.util.control.Breaks.{break, breakable}

import scalation.linalgebra.{MatriD, MatriI, MatrixD, MatrixI, VectoD, VectoI, VectorI}
import scalation.stat.vectorD2StatVector
import scalation.util.{Error, getFromURL_File}

import Round.roundVec

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClassifierInt` abstract class provides a common foundation for several
 *  classifiers that operate on integer-valued data.
 *  @param x   the integer-valued data vectors stored as rows of a matrix
 *  @param y   the integer-valued classification vector, where y_i = class for row i of matrix x
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
abstract class ClassifierInt (x: MatriI, y: VectoI, protected var fn: Strings = null,
                              k: Int, protected var cn: Strings = null)
         extends Classifier with Error
{
    /** the number of data vectors in training/test-set (# rows)
     */
    protected val m = y.dim

    /** the number of features/variables (# columns)
     */
    protected val n = if (x == null) 0 else x.dim2

    /** the training-set size as a Double
     */
    protected val md = m.toDouble

    /** the feature-set size as a Double
     */
    protected val nd = n.toDouble

    /** the set of features to turn on or off. All features are on by default.
     *  Used for feature selection.
     */
    protected val fset = Array.fill (n)(true)

    if (fn == null && x != null) fn = x.range2.map ("x" + _).toArray    // default variable names
    if (cn == null) cn = if (k == 2) Array ("no", "yes")                // default class names
                         else (0 until k).map ("c" + _).toArray

    if (x != null && x.dim1 != m)     flaw ("constructor", "y.dim must equal training-set size (m)")
    if (fn != null && fn.length != n) flaw ("constructor", "fn.length must equal feature-set size (n)")
    if (k >= m)         flaw ("constructor", "k must be less than training-set size (m)")
    if (cn.length != k) flaw ("constructor", "cn.length must equal number of classes (k)")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of data vectors/points in the entire dataset (training + testing),
     */
    def size: Int = m

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return default values for binary input data (value count 'vc' set to 2).
     */
    def vc_default: Array [Int] = Array.fill (n)(2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return value counts calculated from the input data.
     *  May wish to call 'shiftToZero' before calling this method.
     */
    def vc_fromData: Array [Int] = (for (j <- x.range2) yield x.col(j).max() + 1).toArray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return value counts calculated from the input data.
     *  May wish to call 'shiftToZero' before calling this method.
     *  @param rg  the range of columns to be considered
     */
    def vc_fromData2 (rg: Range): Array [Int] = (for (j <- rg) yield x.col(j).max() + 1).toArray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shift the 'x' Matrix so that the minimum value for each column equals zero.
     */
    def shiftToZero () { x -= VectorI (for (j <- x.range2) yield x.col(j).min()) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector 'z', determine which class it fits into,
     *  returning the best class, its name and its relative probability.
     *  Override in classes that require precise real values for classification.
     *  @param z  the real vector to classify
     */
    def classify (z: VectoD): (Int, String, Double) = classify (roundVec (z))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify all of the row vectors in matrix 'xx'.
     *  @param xx  the row vectors to classify
     */
    def classify (xx: MatriI): VectoI =
    {
        VectorI (for (i <- xx.range1) yield classify (xx(i))._1)
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param itest  indices of the instances considered test data
     */
    def test (itest: Ints): Double =
    {
        var correct = 0
        for (i <- itest if classify (x(i))._1 == y(i)) correct += 1
        correct / itest.size.toDouble
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param xx  the integer-valued test vectors stored as rows of a matrix
     *  @param yy  the test classification vector, where 'yy_i = class' for row 'i' of 'xx'
     */
    def test (xx: MatriI, yy: VectoI): Double =
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
        val fea = for (j <- 0 until n) yield x.col(j).toDouble.toDense
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
        val zfea = for (j <- zrg) yield x.col(j).toDouble.toDense
        val xfea = for (j <- xrg) yield x.col(j).toDouble.toDense
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
                } // if
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

} // ClassifierInt abstract class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClassifierInt` companion object provides methods to read in data
 *  matrices in a combined 'xy' format that can be later decomposed into
 *  'x' the feature data matrix and 'y' the classification vector.
 */
object ClassifierInt
{
    private val SP = ','                      // the token separation character

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read the data set (e.g., a CSV file) and return the 'xy' data matrix.
     *  It will make sure the classification column 'cc' is last.
     *  @param fname  the file-name (file should contain lines of data)
     *  @param m      the number of data rows
     *  @param n      the number of data columns/features (including the classification)
     *  @param skip   the number of columns at the beginning the line to skip (e.g., id column)
     *  @param cc     the classification column (the default (-1) => no position checking)
     */
    def apply (fname: String, m: Int, n: Int, skip: Int = 1, cc: Int = -1): MatrixI =
    {
        val lines = getFromURL_File (fname)
        val xy    = new MatrixI (m, n)
        var i     = 0
        for (ln <- lines) { xy(i) = VectorI (ln.split (SP), skip); i += 1; }

        if (cc >= 0 && cc != n-1) {       // want the classification column (cc) to be the last column
            val c1 = xy.col (cc)          // swap column cc with last (n-1), if necessary
            val c2 = xy.col (n-1)
            xy.setCol (cc, c2)
            xy.setCol (n-1, c1)
        } // if
        xy
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pull out the designed response column from the combined matrix.
     *  When 'col' is negative or the last column, slice out the last column.
     *  @param xy   the combined data and response/classification matrix
     *  @param col  the designated response column to be pulled out
     */
    def pullResponse (xy: MatriI, col: Int = -1): (MatriI, VectoI) =
    {
        if (col < 0 || col == xy.dim2-1) (xy.sliceCol (0, xy.dim2-1), xy.col (xy.dim2-1))
        else                             (xy.sliceEx (xy.dim1, col), xy.col (col))
    } // pullResponse

} // ClassifierInt object

