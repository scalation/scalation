
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math.round

import scalation.linalgebra.{MatriD, MatrixD, MatriI, MatrixI, VectoD, VectoI, VectorI}
import scalation.stat.vectorD2StatVector
import scalation.util.{Error, getFromURL_File}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClassifierInt` abstract class provides a common foundation for several
 *  classifiers that operate on integer-valued data.
 *  @param x   the integer-valued training/test data vectors stored as rows of a matrix
 *  @param y   the training/test classification vector, where y_i = class for row i of the matrix x
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
abstract class ClassifierInt (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String])
         extends Classifier with Error
{
    /** the number of data vectors in training/test-set (# rows)
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

    if (y.dim != m)     flaw ("constructor", "y.dim must equal training-set size (m)")
    if (fn.length != n) flaw ("constructor", "fn.length must equal feature-set size (n)")
    if (k >= m)         flaw ("constructor", "k must be less than training-set size (m)")
    if (cn.length != k) flaw ("constructor", "cn.length must equal number of classes (k)")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of data vectors in training/test-set (# rows).
     */
    def size: Int = m

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return default values for binary input data (value count 'vc' set to 2).
     */
    def vc_default: VectorI = { val vc = new VectorI (n); vc.set (2); vc }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return value counts calculated from the input data.
     *  May wish to call 'shiftToZero' before calling this method.
     */
    def vc_fromData: VectorI = VectorI (for (j <- x.range2) yield x.col(j).max() + 1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return value counts calculated from the input data.
     *  May wish to call 'shiftToZero' before calling this method.
     *  @param rg  the range of columns to be considered
     */
    def vc_fromData2 (rg: Range): VectorI = VectorI (for (j <- rg) yield x.col(j).max() + 1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shift the 'x' Matrix so that the minimum value for each column equals zero.
     */
    def shiftToZero () { x -= VectorI (for (j <- x.range2) yield x.col(j).min()) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector 'z', determine which class it belongs
     *  to, by first rounding it to an integer-valued vector.
     *  Return the best class, its name and its relative probability
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): (Int, String, Double) =
    {
        val zi = new VectorI (z.dim)
        for (j <- 0 until z.dim) zi(j) = (round (z(j))).toInt
        classify (zi)
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     */
    def test (testStart: Int, testEnd: Int): Double =
    {
        var correct = 0
        for (i <- testStart until testEnd if classify (x(i))._1 == y(i)) correct += 1
        correct / (testEnd - testStart).toDouble
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param xx  the integer-valued test vectors stored as rows of a matrix
     *  @param yy  the test classification vector, where 'yy_i = class' for row 'i' of 'xx'
     */
    def test (xx: MatrixI, yy: VectorI): Double =
    {
        val mm = xx.dim1
        if (yy.dim != mm) flaw ("test", "yy.dim must equal test-set size (mm)")
        var correct = 0
        for (i <- 0 until mm if classify (xx(i))._1 == yy(i)) correct += 1
        correct / mm.toDouble
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param itest  indices of the instances considered test data
     */
    override def test (itest: VectorI): Double =
    {
        var correct = 0
        for (i <- itest if classify (x(i))._1 == y(i)) correct += 1
        correct / itest.dim.toDouble
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the correlation matrix for the feature vectors 'fea'.
     *  If the correlations are too high, the independence assumption may be dubious.
     */
    def calcCorrelation: MatriD =
    {
        val fea = for (j <- 0 until n) yield  x.col(j).toDouble.toDense
        val cor = new MatrixD (n, n)
        for (j1 <- 0 until n; j2 <- 0 until j1) {
//          println ("fea (j1) = " + fea(j1))
//          println ("fea (j2) = " + fea(j2))
            cor(j1, j2) = fea(j1) corr fea(j2)
        } // for
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
        val zfea = for (j <- zrg) yield  x.col(j).toDouble.toDense
        val xfea = for (j <- xrg) yield  x.col(j).toDouble.toDense
        val cor = new MatrixD (zfea.size, xfea.size)
        for (j1 <- 0 until cor.dim1; j2 <- 0 until cor.dim2) {
            //println ("fea (j1) = " + fea(j1))
            //println ("fea (j2) = " + fea(j2))
            cor(j1, j2) = zfea(j1) corr xfea(j2)
        } // for
        cor
    } // calcCorrelation

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

} // ClassifierInt object

