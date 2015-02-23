
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.round

import scalation.linalgebra.{MatrixI, VectorD, VectorI}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClassifierInt` abstract class provides a common foundation for several
 *  classifiers that operate on integer-valued data.
 *  @param x   the integer-valued training data vectors stored as rows of a matrix
 *  @param y   the training classification vector, where y_i = class for row i of the matrix x
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
abstract class ClassifierInt (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String])
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
    /** Return default values for binary input data (value count (vc) set to 2).
     */
    def vc_default: VectorI = { val vc = new VectorI (n); vc.set (2); vc }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector 'z', determine which class it belongs
     *  to, by first rounding it to an integer-valued vector.
     *  @param z  the vector to classify
     */
    def classify (z: VectorD): Tuple2 [Int, String] =
    {
        val zi = new VectorI (z.dim)
        for (j <- 0 until z.dim) zi(j) = (round (z(j))).toInt
        classify (zi)
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param xx  the integer-valued test vectors stored as rows of a matrix
     *  @param yy  the test classification vector, where yy_i = class for row i of xx
     */
    def test (xx: MatrixI, yy: VectorI): Double =
    {
        val mm = xx.dim1
        if (yy.dim != mm) flaw ("test", "yy.dim must equal test-set size (mm)")
        var correct = 0
        for (i <- 0 until mm if classify (xx(i))._1 == yy(i)) correct += 1
        correct / mm.toDouble
    } // test

} // ClassifierInt abstract class

