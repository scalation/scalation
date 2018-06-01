
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Khalifeh Al-Jadda, John A. Miller
 *  @version 1.5
 *  @date    Mon Aug 15 13:13:15 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math.abs

import scalation.columnar_db.Relation
import scalation.linalgebra.{MatrixD, MatriI, MatrixI, VectorD, VectoI, VectorI}
import scalation.linalgebra.gen._
import scalation.util.{banner, time}

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3fl` class implements a three-level Probabilistic Classifier for discrete
 *  (binary) input data, based on flow from bottom to top levels.
 *  The classifier is trained using the following data matrices:
 *  <p>
 *      'x' - the mid/features 'X' (level 1)
 *      'z' - the low/feature 'Z'  (level 2)
 *      'y' - the top/class 'C'    (level 0)
 *  <p>
 *  Each random variable 'C_j, X_k and Z_l' is binary ('vc = 2', 'k = 2'), where
 *  '1' indicates occurrence, while '0' indicates no evidence of occurreence.
 *  Frequency counts and classification scores are computed from a training-set.
 *  -----------------------------------------------------------------------------
 *  @param x   the integer-valued level-1 data vectors stored as rows of a matrix
 *  @param z   the integer-valued level-2 data vectors stored as rows of a matrix
 *  @param y   the class matrix, where y(i) = classes for row 'i' of the matrix
 *  @param fn  the names for all X/Z-features/variables
 *  @param cn  the names for all C-class variables
 *  @param me  use m-estimates (me == 0 => regular MLE estimates)
 */
class PGMHD3fl (x: MatriI, z: MatriI, y: MatriI, fn: Array [String], cn: Array [String], me: Float = me_default)
      extends BayesClassifier (x, y.col(0), fn, 2, cn)
{
    private val DEBUG  = true                              // debug flag
    private val nx     = x.dim2                            // number of X-features/columns (level 1)
    private val nz     = z.dim2                            // number of Z-features/columns (level 2)
    private val ny     = y.dim2                            // number of C-classes/columns (level 0)

//  private val xrg    = 0 until nx                        // range (column indices) of the X-features
//  private val zrg    = nx until x.dim2                   // range (column indices) of the Z-features
//  private val cor    = calcCorrelation2 (zrg, xrg)       // feature correlation matrix

//  private val pa_X2C = new HMatrix2 [Int] (nx)           // hypermatrix holding the parents for each X-feature
//  private val pa_Z2X = new HMatrix2 [Int] (nz)           // hypermatrix holding the parents for each Z-feature

    private val f_CX_  = new MatrixI (ny, nx)              // co-occurrence frequency counts for C-class j & X-feature k
    private val f_XZ  = new MatrixI (nx, nz)               // co-occurrence frequency counts for X-feature k & Z-feature l

    private val x_in  = new VectorI (nx)                   // f_CX summed over all C - flow into X from above
    private val z_in  = new VectorI (nz)                   // f_XZ summed over all X - flow into Z from above

    private val y_cl  = new MatrixD (ny, nx)               // classification score for C-classes
    private val x_cl  = new MatrixD (nx, nz)               // classification score for X-feature

//  computeParent ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  z features can only select a parent from the x features.
    def computeParent ()
    {
        for (i <- 0 until nz) {
            val correl = cor(i).map ((x: Double) => abs (x))
            parent(i) = correl.argmax ()
        } // for
    } // computeParent
     */

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a model.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def buildModel (testStart: Int, testEnd: Int): (Array [Boolean], DAG) =
    {
        (Array.fill (n)(true), new DAG (Array.ofDim [Int] (n, 0)))
    } // buildModel

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment frequency counters based on the 'i'th row of the data matrix.
     *  @param i  the index for current data row
     */
    private def increment (i: Int)
    {
        val y_i = y(i)                                               // level 0 - top class level
        val x_i = x(i)                                               // level 1 - mid feature level
        val z_i = z(i)                                               // level 2 - low feature level
        for (j <- y_i.range if y_i(j) == 1) {
            for (k <- x_i.range if x_i(k) == 1) f_CX_(j, k) += 1     // both y_i(j) & x_i(k) occur
        } // for
        for (k <- x_i.range if x_i(k) == 1) {
            for (l <- z_i.range if z_i(l) == 1) f_XZ(k, l) += 1      // both x_i(k) & z_i(l) occur
        } // for
    } // increment

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies, compute the inflow and classification scores.
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    private def frequencies (testStart: Int, testEnd: Int)
    {
        if (DEBUG) banner ("frequencies (testStart, testEnd)")
        for (i <- 0 until y.dim1 if i < testStart || i >= testEnd) increment (i)

        for (j <- 0 until ny; k <- 0 until nx) x_in(k) += f_CX_(j, k)
        for (k <- 0 until nx; l <- 0 until nz) z_in(l) += f_XZ(k, l)

        for (j <- 0 until ny; k <- 0 until nx) y_cl(j, k) += f_CX_(j, k) / x_in(k).toDouble
        for (k <- 0 until nx; l <- 0 until nz) x_cl(k, l) += f_XZ(k, l) / z_in(l).toDouble

        if (DEBUG) {
            println ("f_CX  = " + f_CX_)                       // #(C_j = c & X_k = x)
            println ("f_XZ  = " + f_XZ)                        // #(X_k = x & Z_l = z)
            println ("x_in  = " + x_in)                        // sum f_CX over C
            println ("z_in  = " + z_in)                        // sum f_XZ over X
            println ("y_cl  = " + y_cl)                        // classification score for C's
            println ("x_cl  = " + x_cl)                        // classification score for X's
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing frequencies, inflows and scores.
     *  @param itest  the indices of the test data
     */
    def train (itest: IndexedSeq [Int]): PGMHD3fl =
    {
        frequencies (0 until m diff itest)                    // compute frequencies skipping test region

        if (DEBUG) banner ("train (testStart, testEnd)")
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies, compute the inflow and classification scores.
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param itrain indices of the instances considered train data
     */
    private def frequencies (itrain: IndexedSeq [Int])
    {
        if (DEBUG) banner ("frequencies (itrain)")
        for (i <- itrain) increment (i)

        for (j <- 0 until ny; k <- 0 until nx) x_in(k) += f_CX_(j, k)
        for (k <- 0 until nx; l <- 0 until nz) z_in(l) += f_XZ(k, l)

        for (j <- 0 until ny; k <- 0 until nx) y_cl(j, k) += f_CX_(j, k) / x_in(k).toDouble
        for (k <- 0 until nx; l <- 0 until nz) x_cl(k, l) += f_XZ(k, l) / z_in(l).toDouble

        if (DEBUG) {
            println ("f_CX  = " + f_CX_)                       // #(C_j = c & X_k = x)
            println ("f_XZ  = " + f_XZ)                        // #(X_k = x & Z_l = z)
            println ("x_in  = " + x_in)                        // sum f_CX over C
            println ("z_in  = " + z_in)                        // sum f_XZ over X
            println ("y_cl  = " + y_cl)                        // classification score for C's
            println ("x_cl  = " + x_cl)                        // classification score for X's
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'u', classify it returning the class(es)
     *  with the highest classication scores.
     *  Return the best class, its name and its score
     *  @param u  the data vector to classify
     */
    def classify (u: VectoI): (Int, String, Double) =
    {
        if (DEBUG) banner ("classify (u)")
        val xx = u.slice (0, nx)
        val zz = u.slice (nx, u.dim)
        
        val score = new VectorD (ny)
        for (j <- y.range2) {
            score(j) = 0.0
            for (k <- xx.range if xx(k) == 1) {
                var x_weight = 0.0
                for (l <- zz.range if zz(l) == 1) x_weight += x_cl (k, l)
                score(j) += x_weight * y_cl(j, k)
            } // for
        } // for

        if (DEBUG) println ("score = " + score)
        val best = score.argmax ()               // class with the highest relative score
        (best, cn(best), score(best))            // return the best class, its name and its score
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the frequency counters to 0.
     */
    def reset ()
    {
        f_CX_.set (0)
        f_XZ.set (0)
        x_in.set (0)
        z_in.set (0)
        y_cl.set (0.0)
        x_cl.set (0.0)
    } // reset

} // PGMHD3fl class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `PGMHD3fl` is the companion object for the `PGMHD3fl` class.
 */
object PGMHD3fl
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `PGMHD3fl` object, passing 'x', 'z' and 'y' together in one matrix.
     *  @param xzy  the data vectors along with their classifications stored as rows of a matrix
     *  @param nx   the number of X features/columns
     *  @param nxz  the number of X + Z features/columns
     *  @param fn   the names of the X & Z feature variables
     *  @param cn   the names of C-class variable outcomes
     *  @param me   use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xzy: MatriI, nx: Int, nxz: Int, fn: Array [String], cn: Array [String],
               me: Float = me_default): PGMHD3fl =
    {
        val x = xzy.sliceCol (0, nx)
        val z = xzy.sliceCol (nx, nxz)
        val y = xzy.sliceCol (nxz, xzy.dim2)
        new PGMHD3fl (x, z, y, fn, cn, me)
    } // apply

} // PGMHD3fl object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3flTest` object is used to test the `PGMHD3fl` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > runMain scalation.analytics.classifier.PGMHD3flTest
 */
object PGMHD3flTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // x3: Mpg:     High (1), Low (0)
    // classes/features:           x0 x1 z0 z1 y0 y1
    val xzy = new MatrixI ((10, 6), 1, 0, 1, 1, 1, 0,         // data matrix
                                    1, 0, 1, 0, 0, 0,
                                    1, 0, 1, 1, 1, 1,
                                    0, 0, 1, 1, 0, 1,
                                    0, 0, 0, 1, 1, 1,
                                    0, 1, 0, 0, 0, 0,
                                    0, 1, 0, 0, 1, 0,
                                    0, 1, 1, 1, 0, 1,
                                    1, 1, 0, 0, 0, 0,
                                    1, 0, 0, 0, 1, 0)

    val fn = Array ("x0", "x1", "z0", "z1")                  // feature names for X & Z
    val cn = Array ("c0", "c1")                              // class names for C

    println ("xzy = " + xzy)
    println ("---------------------------------------------------------------")

    val x = xzy.sliceCol (0, 2)
    val z = xzy.sliceCol (2, 4)
    val y = xzy.sliceCol (4, 6)
    val pgm = new PGMHD3fl (x, z, y, fn, cn)

    // train the classifier ---------------------------------------------------
    pgm.train ()

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1, 1)                            // existing data vector to classify
    val z2 = VectorI (1, 1, 1, 0)                            // new data vector to classify
    println ("classify (" + z1 + ") = " + pgm.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + pgm.classify (z2) + "\n")

//  pgm.crossValidateRand ()                                 // cross validate the classifier

} // PGMHD3fl object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3flTest2` object is used to test the 'PGMHD3fl' class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > runMain scalation.analytics.classifier.PGMHD3flTest2
object PGMHD3flTest2 extends App
{
    // training-set -----------------------------------------------------------
    // x0: Fast
    // x1: Strong
    // y:  Classification (No/0, Yes/1)
    // features:                  x0 x1  y
    val xy = new MatrixI ((10, 3), 1, 1, 1,
                                   1, 1, 1,
                                   1, 0, 1,
                                   1, 0, 1,
                                   1, 0, 0,
                                   0, 1, 0,
                                   0, 1, 0,
                                   0, 1, 1,
                                   0, 0, 0,
                                   0, 0, 0)

    val fn = Array ("Fast", "Strong")                        // feature names
    val cn = Array ("No", "Yes")                             // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val pgm = PGMHD3fl (xy, 1, fn, 2, cn, null, 0)           // create the classifier

    // train the classifier ---------------------------------------------------
    pgm.train()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                   // new data vector to classify
    println ("classify (" + z + ") = " + pgm.classify (z) + "\n")

    println ("Cross Validation starts:")
    println ("CV average accuracy = " + pgm.crossValidate ())     // cross validate the classifier

} // PGMHD3flTest2 object
 */


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3flTest3` object is used to test the 'PGMHD3fl' class.
 *  > runMain scalation.analytics.classifier.PGMHD3flTest3
object PGMHD3flTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1")                                // class names
    val pgm = PGMHD3fl (xy, 2, fn, 2, cn, null, 0)           // create the classifier
    pgm.train ()
    pgm.crossValidate ()
    println ("Cross Validation starts:")
    println ("CV average accuracy = " + pgm.crossValidate ())     // cross validate the classifier

} // PGMHD3flTest3 object
 */
