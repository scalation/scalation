
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Khalifeh Al-Jadda, John A. Miller
 *  @version 1.6
 *  @date    Mon Aug 15 13:13:15 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.math.abs

import scalation.columnar_db.Relation
import scalation.linalgebra.{MatriI, VectoI, VectorD, VectorI}
import scalation.linalgebra.gen._
import scalation.util.{banner, time}

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3cp` class implements a three level Bayes Classifier for discrete input data.
 *  The classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the frequency/population
 *  of  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier is naive, because it assumes feature independence and therefore
 *  simply multiplies the conditional probabilities.
 *  -----------------------------------------------------------------------------
 *  [ x ] -> [ x z ]  where x features are level 2 and z features are level 3.
 *  -----------------------------------------------------------------------------
 *  @param x    the integer-valued data vectors stored as rows of a matrix
 *  @param nx   the number of x features/columns
 *  @param y    the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn_  the names for all features/variables
 *  @param k    the number of classes
 *  @param cn_  the names for all classes
 *  @param vc   the value count (number of distinct values) for each feature
 *  @param me   use m-estimates (me == 0 => regular MLE estimates)
 */
class PGMHD3cp (x: MatriI, nx: Int, y: VectoI, fn_ : Strings = null, k: Int, cn_ : Strings = null,
                private var vc: Array [Int] = null, me: Float = me_default)
      extends BayesClassifier (x, y, fn_, k, cn_)
{
    private val DEBUG  = true                              // debug flag
    private val nz     = x.dim2 - nx                       // number of z features/columns
    private val xrg    = 0 until nx                        // range (column indices) of the X-features
    private val zrg    = nx until x.dim2                   // range (column indices) of the Z-features
    private val cor    = calcCorrelation2 (zrg, xrg)       // feature correlation matrix
    private val parent = new VectorI (nz)                  // vector holding the parent for each Z-feature
    private val vc_p   = Array.ofDim [Int] (nz)            // value count for the parent


//  private val nu_X    = new HMatrix2 [Int] (nx)           // frequency counts for X-feature jx
                nu_Xy   = new HMatrix3 [Int] (k, nx)        // frequency counts for y-class c & X-feature jx
//  private val nu_XZ   = new HMatrix4 [Int] (nx, nz)       // frequency counts for X-feature jx & Z-feature jz
//  private val nu_XZy  = new HMatrix4 [Int] (k, nz)        // frequency counts for y-class c & X-feature jx & Z-feature jz
    private val nu_XZ_y = new HMatrix4 [Int] (k, nz)        // frequency counts for y-class c & X-feature jx & Z-feature jz

    if (vc == null) {
        shiftToZero; vc = vc_fromData                      // set value counts from the data
    } // if
    val vc_x = vc.slice (0, nx)                            // distinct value counts for X-features
    val vc_z = vc.slice (nx, n)                            // distinct value counts for Z-features

    computeParent ()
    computeVcp ()

//  nu_X.alloc (vc_x)
    nu_Xy.alloc (vc_x)
//  nu_XZ.alloc (vc_x, vc_z)
    nu_XZ_y.alloc (vc_z, vc_p)

    if (DEBUG) {
        println ("distinct value count vc_x = " + vc_x.deep)
        println ("distinct value count vc_z = " + vc_z.deep)
        println ("correlation matrix        = " + cor)
        println ("Z's parent                = " + parent)
        println ("parent value count vc_p   = " + vc_p.deep)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  z features can only select a parent from the x features.
     */
    def computeParent ()
    {
        for (i <- 0 until nz) {
            val correl = cor(i).map ((x: Double) => abs (x))
            parent(i) = correl.argmax ()
        } // for
    } // computeParent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value counts of each parent feature based on the parent vector.
     */
    def computeVcp () { for (j <- 0 until nz) vc_p(j) = vc_x(parent(j)) }

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
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param itest  the indeices of the test data
     */
    def train (itest: Ints): PGMHD3cp =
    {
        frequencies (0 until m diff itest)                    // compute frequencies skipping test region

        if (DEBUG) banner ("train (testStart, testEnd)")
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and value 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param itrain  indices of the instances considered train data
     */
    private def frequencies (itrain: Ints)
    {
        if (DEBUG) banner ("frequencies (itrain)")

        for (l <- itrain) {                                    // l = lth row of data matrix x
            val i = y(l)                                       // get the class for l-th tow

            for (j <- 0 until n) {
                if (j < nx) {
                    val x_j = x(l, j)                          // get value for j-th X-feature
//                  nu_X(j, x_j)     += 1                      // nu_X
                    nu_Xy(i, j, x_j) += 1                      // nu_Xy
                } else {
                    val jj = j - nx                            // index from start of Z region
                    val z_j = x(l, j)                          // get value for j-th Z-feature
                    val jp = parent (jj)                       // X_jp is parent of Z_j
                    val x_jp = x(l, jp)                        // get value for jp-th X-feature
//                  nu_XZ(jp, jj, x_jp, z_j) += 1              // nu_XZ
                    nu_XZ_y(i, jj, z_j, x_jp) += 1             // nu_XZy
                } // if
            } // for

        } // for

        if (DEBUG) {
//          println ("nu_X   = " + nu_X)                         // #(X_j = x)
            println ("nu_Xy  = " + nu_Xy)                        // #(y = c & X_j = x)
//          println ("nu_XZ  = " + nu_XZ)                        // #(X_jp = x & Z_j = z)
            println ("nu_XyZ = " + nu_XyZ)                       // #(X_jp = x & y = c & Z_j = z)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment/Decrement frequency counters based on the 'i'th row of the
     *  data matrix.
     *  @param i  the index for current data row
     */
    protected def updateFreq (i: Int) = ???

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'u', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param u  the data vector to classify
     */
    def classify (u: VectoI): (Int, String, Double) =
    {
        if (DEBUG) banner ("classify (u)")
        val prob = new VectorD (k)
        
        for (i <- 0 until k) {
            prob(i) = 1.0                       // proportional to probabilty
            for (j <- 0 until n) {
                if (j < nx) prob(i) *= nu_Xy(i, j, u(j))
                else        prob(i) *= nu_XZ_y(i, j-nx, u(j), u(parent(j-nx)))
            } // for
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()               // class with the highest relative probability
        (best, cn(best), prob(best))            // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the frequency hypermatrices to 0.
     */
    def reset ()
    {
//      nu_X.set (0)
        nu_Xy.set (0)
//      nu_XZ.set (0)
        nu_XyZ.set (0)
    } // reset

} // PGMHD3cp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `PGMHD3cp` is the companion object for the `PGMHD3cp` class.
 */
object PGMHD3cp
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `PGMHD3cp` object, passing 'x' and 'y' together in one matrix.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param nx  the number of x features/columns
     *  @param fn  the names of the features
     *  @param k   the number of classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, nx: Int, fn: Strings, k: Int, cn: Strings,
               vc: Array [Int] = null, me: Float = me_default) =
    {
        new PGMHD3cp (xy(0 until xy.dim1, 0 until xy.dim2 - 1), nx, xy.col(xy.dim2 - 1), fn, k, cn, vc, me)
    } // apply

} // PGMHD3cp object

import scalation.linalgebra.MatrixI

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3cpTest` object is used to test the `PGMHD3cp` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > runMain scalation.analytics.classifier.PGMHD3cpTest
 */
object PGMHD3cpTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // x3: Mpg:     High (1), Low (0)
    // features:                 x0 x1 x2 x3
    val x = new MatrixI ((10, 4), 1, 0, 1, 1,                // data matrix
                                  1, 0, 1, 0,
                                  1, 0, 1, 1,
                                  0, 0, 1, 1,
                                  0, 0, 0, 1, 
                                  0, 1, 0, 0, 
                                  0, 1, 0, 0,
                                  0, 1, 1, 1,
                                  1, 1, 0, 0,
                                  1, 0, 0, 0)

    val y  = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)          // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin", "Mpg")        // feature/variable names
    val cn = Array ("No", "Yes")                             // class names

    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    val pgm = new PGMHD3cp (x, 2, y, fn, 2, cn)              // create the classifier            

    // train the classifier ---------------------------------------------------
    pgm.train ()

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1, 1)                            // existing data vector to classify
    val z2 = VectorI (1, 1, 1, 0)                            // new data vector to classify
    println ("classify (" + z1 + ") = " + pgm.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + pgm.classify (z2) + "\n")

//  pgm.crossValidateRand ()                                 // cross validate the classifier

} // PGMHD3cpTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3cpTest2` object is used to test the 'PGMHD3cp' class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > runMain scalation.analytics.classifier.PGMHD3cpTest2
 */
object PGMHD3cpTest2 extends App
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

    val pgm = PGMHD3cp (xy, 1, fn, 2, cn, null, 0)           // create the classifier

    // train the classifier ---------------------------------------------------
    pgm.train()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                   // new data vector to classify
    println ("classify (" + z + ") = " + pgm.classify (z) + "\n")

    println ("Cross Validation starts:")
    println ("CV average accuracy = " + pgm.crossValidate ())     // cross validate the classifier

} // PGMHD3cpTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3cpTest3` object is used to test the 'PGMHD3cp' class.
 *  > runMain scalation.analytics.classifier.PGMHD3cpTest3
 */
object PGMHD3cpTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1")                                // class names
    val pgm = PGMHD3cp (xy, 2, fn, 2, cn, null, 0)           // create the classifier
    pgm.train ()
    pgm.crossValidate ()
    println ("Cross Validation starts:")
    println ("CV average accuracy = " + pgm.crossValidate ())     // cross validate the classifier

} // PGMHD3cpTest3 object

