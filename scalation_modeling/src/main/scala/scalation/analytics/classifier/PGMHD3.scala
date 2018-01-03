
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Khalifeh Al-Jadda, John A. Miller, Hao Peng
 *  @version 1.4
 *  @date    Mon Aug 15 13:13:15 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math._

import scalation.columnar_db.Relation
import scalation.linalgebra.{MatriI, VectoI, VectorD, VectorI}
import scalation.linalgebra.gen.{HMatrix4, HMatrix3}
import scalation.util.{banner, time}

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3` class implements a three level Bayes Classifier for discrete input data.
 *  The classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier is naive, because it assumes feature independence and therefore
 *  simply multiplies the conditional probabilities.
 *  -----------------------------------------------------------------------------
 *  [ x ] -> [ x z ]  where x features are level 2 and z features are level 3.
 *  -----------------------------------------------------------------------------
 *  @param x   the integer-valued data vectors stored as rows of a matrix
 *  @param nx  the number of x features/columns
 *  @param y   the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 *  @param vc  the value count (number of distinct values) for each feature
 *  @param me  use m-estimates (me == 0 => regular MLE estimates)
 */
class PGMHD3 (x: MatriI, nx: Int, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                  private var vc: VectoI = null, me: Float = me_default)
      extends BayesClassifier (x, y, fn, k, cn)
{
    private val DEBUG  = true                              // debug flag
    private val nz     = x.dim2 - nx                       // number of z features/columns
    private val xrg    = 0 until nx                        // range (column indices) of the X-features
    private val zrg    = nx until x.dim2                   // range (column indices) of the Z-features
    private val cor    = calcCorrelation2 (zrg, xrg)       // feature correlation matrix
    private val parent = new VectorI (nz)                  // vector holding the parent for each Zfeature/variable
    private val vcp    = Array.ofDim [Int] (nz)            // value count for the parent

    private val popC  = new VectorI (k)                    // frequency counts for classes 0, ..., k-1
    private val probC = new VectorD (k)                    // probabilities for classes 0, ..., k-1
    private val popX  = new HMatrix3 [Int] (k, nx)         // conditional frequency counts for variable/feature j
    private val probX = new HMatrix3 [Double] (k, nx)      // conditional probabilities for variable/feature j
    private val popZ  = new HMatrix4 [Int] (k, nz)         // conditional frequency counts for variable/feature j
    private val probZ = new HMatrix4 [Double] (k, nz)      // conditional probabilities for variable/feature j

    if (vc == null) {
        shiftToZero; vc = vc_fromData                      // determine 'vc' from data
    } // if
    val vc_x = vc.slice (0, nx)().toArray
    val vc_z = vc.slice (nx, n)().toArray

    computeParent ()
    computeVcp ()

    popX.alloc (vc_x)
    probX.alloc (vc_x)
    popZ.alloc (vc_z, vcp)
    probZ.alloc (vc_z, vcp)

    if (DEBUG) {
        println ("value count vc     = " + vc)
        println ("value count vc_x   = " + vc_x.deep)
        println ("value count vc_z   = " + vc_z.deep)
        println ("correlation matrix = " + cor)
        println ("value count vcP    = " + vcp.deep)
        println ("Z's parent         = " + parent)
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
    def computeVcp ()
    {
        for (j <- 0 until nz) vcp(j) = vc_x(parent(j))
    } // computeVcp

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
    /** Count the frequencies for 'y' having class 'i' and value 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    private def frequencies (testStart: Int, testEnd: Int)
    {
        if (DEBUG) banner ("frequencies (testStart, testEnd)")
        for (l <- 0 until m if l < testStart || l >= testEnd) {
            // l = lth row of data matrix x
            val i = y(l)                                       // get the class
            popC(i) += 1                                       // increment ith class
            for (j <- 0 until n) {
                if (j < nx) popX(i, j, x(l, j))    += 1                         // increment ith class, jth feature, x value
                else        popZ(i, j-nx, x(l, j), x(l, parent(j-nx))) += 1     // increment ith class, jth feature, z value
            } // for
        } // for

        if (DEBUG) {
            println ("popC = " + popC)                         // #(C = i)
            println ("popX = " + popX)                         // #(X_j = x & C = i)
            println ("popZ = " + popZ)                         // #(Z_j = z & C = i)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int)
    {
        frequencies (testStart, testEnd)                       // compute frequencies skipping test region

        if (DEBUG) banner ("train (testStart, testEnd)")
        for (i <- 0 until k) {
            // for each class i
            val pci = popC(i).toDouble                         // population of class i
            probC(i) = pci / md                                // probability of class i

            for (j <- 0 until nx) {                            // for each feature j
                val me_vc = me / vc_x(j).toDouble
                for (xj <- 0 until vc_x(j)) {                  // for each value for feature j: xj
                    probX(i, j, xj) = (popX(i, j, xj) + me_vc) / (pci + me)
                } // for
            } // for
            for (j <- 0 until nz) {                            // for each feature j
                val me_vc = me / vc_z(j).toDouble
                for (zj <- 0 until vc_z(j); zp <- 0 until vc_x(parent(j))) {  // for each value of feature j: zj and each value of zj's parent: zp
                    probZ(i, j, zj, zp) = (popZ(i, j, zj, zp) + me_vc) / (pci + me)
                } // for
            } // for
        } // for

        if (DEBUG) {
            println ("probC = " + probC)                       // P(C = i)
            println ("probX = " + probX)                       // P(X_j = x | C = i)
            println ("probZ = " + probZ)                       // P(Z_j = z | C = i)
        } // if
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and value 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param itrain indices of the instances considered train data
     */
    private def frequencies (itrain: IndexedSeq [Int])
    {
        if (DEBUG) banner ("frequencies (itrain)")
        for (l <- itrain) {                                    // l = lth row of data matrix x
            // l = lth row of data matrix x
            val i = y(l)                                       // get the class
            popC(i) += 1                                       // increment ith class
            for (j <- 0 until n) {
                if (j < nx) popX(i, j, x(l, j))    += 1                         // increment ith class, jth feature, x value
                else        popZ(i, j-nx, x(l, j), x(l, parent(j-nx))) += 1     // increment ith class, jth feature, z value
            } // for
        } // for

        if (DEBUG) {
            println ("popC = " + popC)                         // #(C = i)
            println ("popX = " + popX)                         // #(X_j = x & C = i)
            println ("popZ = " + popZ)                         // #(Z_j = z & C = i)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param itrain indices of the instances considered train data
     */
    override def train (itrain: IndexedSeq [Int])
    {
        frequencies (itrain)                                   // compute frequencies skipping test region

        if (DEBUG) banner ("train (itrain)")
        for (i <- 0 until k) {
            // for each class i
            val pci = popC(i).toDouble                         // population of class i
            probC(i) = pci / md                                // probability of class i

            for (j <- 0 until nx) {                            // for each feature j
            val me_vc = me / vc_x(j).toDouble
                for (xj <- 0 until vc_x(j)) {                  // for each value for feature j: xj
                    probX(i, j, xj) = (popX(i, j, xj) + me_vc) / (pci + me)
                } // for
            } // for
            for (j <- 0 until nz) {                            // for each feature j
            val me_vc = me / vc_z(j).toDouble
                for (zj <- 0 until vc_z(j); zp <- 0 until vc_x(parent(j))) {  // for each value of feature j: zj and each value of zj's parent: zp
                    probZ(i, j, zj, zp) = (popZ(i, j, zj, zp) + me_vc) / (pci + me)
                } // for
            } // for
        } // for

        if (DEBUG) {
            println ("probC = " + probC)                       // P(C = i)
            println ("probX = " + probX)                       // P(X_j = x | C = i)
            println ("probZ = " + probZ)                       // P(Z_j = z | C = i)
        } // if
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        if (DEBUG) banner ("classify (z)")
        val prob = new VectorD (k)
        for (i <- 0 until k) {
            prob(i) = probC(i)                                    // P(C = i)
            for (j <- 0 until n) {
                if (j < nx) prob(i) *= probX(i, j, z(j))          // P(X_j = z_j | C = i)
                else        prob(i) *= probZ(i, j-nx, z(j), z(parent(j-nx)))       // P(Z_j = z_j | C = i )
            } // for
            //if (DEBUG) println ("prob = " + prob)
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()               // class with the highest relative posterior probability
        (best, cn(best), prob(best))            // return the best class, its name and its probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the population and probability vectors and
     *  hypermatrices to 0.
     */
    def reset ()
    {
        popC.set (0)
        probC.set (0)
        popX.set (0)
        probX.set (0)
        popZ.clear()
        probZ.clear()
        popZ.alloc (vc_z, vcp)
        probZ.alloc (vc_z, vcp)
    } // reset

} // PGMHD3 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `PGMHD3` is the companion object for the `PGMHD3` class.
 */
object PGMHD3
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `PGMHD3` object, passing 'x' and 'y' together in one matrix.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param nx  the number of x features/columns
     *  @param fn  the names of the features
     *  @param k   the number of classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, nx: Int, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI = null, me: Float = me_default) =
    {
        new PGMHD3 (xy(0 until xy.dim1, 0 until xy.dim2 - 1), nx, xy.col(xy.dim2 - 1), fn, k, cn,
            vc, me)
    } // apply

} // PGMHD3 object

import scalation.linalgebra.MatrixI

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3Test` object is used to test the `PGMHD3` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > runMain scalation.analytics.classifier.PGMHD3Test
 */
object PGMHD3Test extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // x3: Mpg:     High (1), Low (0)
    // features:                 x0 x1 x2 x3
    val x = new MatrixI ((10, 4), 1, 0, 1, 1,               // data matrix
                                  1, 0, 1, 0,
                                  1, 0, 1, 1,
                                  0, 0, 1, 1,
                                  0, 0, 0, 1, 
                                  0, 1, 0, 0, 
                                  0, 1, 0, 0,
                                  0, 1, 1, 1,
                                  1, 1, 0, 0,
                                  1, 0, 0, 0)

    val y  = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)         // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin", "Mpg")       // feature/variable names
    val cn = Array ("No", "Yes")                            // class names

    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    val pgmhd3 = new PGMHD3 (x, 2, y, fn, 2, cn)                // create the classifier

    // train the classifier ---------------------------------------------------
    pgmhd3.train ()

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1, 1)                           // existing data vector to classify
    val z2 = VectorI (1, 1, 1, 0)                           // new data vector to classify
    println ("classify (" + z1 + ") = " + pgmhd3.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + pgmhd3.classify (z2) + "\n")

//  nb.crossValidateRand ()                                 // cross validate the classifier

} // PGMHD3Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3Test2` object is used to test the 'PGMHD3' class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > runMain scalation.analytics.classifier.PGMHD3Test2
 */
object PGMHD3Test2 extends App
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

    val fn = Array ("Fast", "Strong")                     // feature names
    val cn = Array ("No", "Yes")                          // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val pgmhd3 = PGMHD3 (xy, 1, fn, 2, cn, null, 0)           // create the classifier

    // train the classifier ---------------------------------------------------
    pgmhd3.train()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                // new data vector to classify
    println ("classify (" + z + ") = " + pgmhd3.classify (z) + "\n")

    println("Cross Validation starts:")
    println("CV average accuracy = " + pgmhd3.crossValidate ())     // cross validate the classifier

} // PGMHD3Test2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGMHD3Test3` object is used to test the 'PGMHD3' class.
 *  > runMain scalation.analytics.classifier.PGMHD3Test3
 */
object PGMHD3Test3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1")                                 // class names
    val pgmhd3 = PGMHD3 (xy, 2, fn, 2, cn, null, 0)           // create the classifier
    pgmhd3.train ()
    println("Cross Validation starts:")
    println("CV average accuracy = " + pgmhd3.crossValidate ())

} // PGMHD3Test3 object

