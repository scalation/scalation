
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

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
abstract class ClassifierReal (x: MatriD, y: VectoI, fn: Array [String], k: Int, cn: Array [String])
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

    if (y.dim != m)     flaw ("constructor", "y.dim must equal training-set size (m)")
    if (fn.length != n) flaw ("constructor", "fn.length must equal feature-set size (n)")
    if (k >= m)         flaw ("constructor", "k must be less than training-set size (m)")
    if (cn.length != k) flaw ("constructor", "cn.length must equal number of classes (k)")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return default values for binary input data (value count 'vc' set to 2).
     */
    def vc_default: VectoI = { val vc = new VectorI (n); vc.set (2); vc }

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
    def classify (z: VectoI): (Int, String, Double) =
    {
        val zd = new VectorD (z.dim)
        for (j <- 0 until z.dim) zd(j) = z(j).toDouble
        classify (zd)
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
     *  @param xx  the real-valued test vectors stored as rows of a matrix
     *  @param yy  the test classification vector, where 'yy_i = class for row i of xx'
     */
    def test (xx: MatrixD, yy: VectoI): Double =
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
        val fea = for (j <- 0 until n) yield x.col(j)
        val cor = new MatrixD (n, n)
        for (j1 <- 0 until n; j2 <- 0 until j1) cor(j1, j2) = fea(j1).asInstanceOf [VectorD] corr fea(j2).asInstanceOf [VectorD]
        cor
    } // calcCorrelation

} // ClassifierReal abstract class

