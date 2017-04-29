
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhe Jin
 *  @version 1.3
 *  @date    Sat Aug  8 20:26:34 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.collection.mutable.ListBuffer

import scalation.linalgebra.{MatrixD, MatriI, MatrixI, VectorD, VectoI, VectorI}
import scalation.linalgebra.gen.{HMatrix3, HMatrix5}
import scalation.math.log2
import scalation.relalgebra.Relation

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DAG' class provides a data structure for storing directed acyclic graphs.
 *  @param parent  records the parents for each node in the graph
 */
class DAG (val parent: Array [Array [Int]])
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `DAG` to a string.
     */
    override def toString: String =
    {
        val sb = new StringBuilder ()
        for (p <- parent) sb append p.deep
        sb.toString
    } // toString

} // DAG class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifier` object provides factory methods for building Bayesian
 *  classifiers.  The following types of classifiers are currently supported:
 *  `NaiveBayes`       - Naive Bayes classifier
 *  `SelNaiveBayes`    - Selective Naive Bayes classifier
 *  `OneBAN`           - Augmented Naive Bayes (1-BAN) classifier
 *  `SelOneBAN`        - Augmented Selective Naive Bayes (Selective 1-BAN) classifier
 *  `TANBayes`         - Tree Augmented Naive Bayes classifier
 *  `SelTANBayes`      - Selective Tree Augmented Naive Bayes classifier
 *  `TwoBAN_OS`        - Ordering-based Bayesian Network (2-BAN with Order Swapping)
 *-----------------------------------------------------------------------------
 *  @param x   the integer-valued data vectors stored as rows of a matrix
 *  @param y   the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
abstract class BayesClassifier (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String])
         extends ClassifierInt (x, y, fn, k, cn) with BayesMetrics
{
    protected var smooth     = true                    // FIX - what ??

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle the value of the 'smooth' property.
     */
    def toggleSmooth () { smooth = ! smooth}

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the model with feature order and selection.
     *  @param testStart  the start of test region (inclusive)
     *  @param testEnd    the end of test region (exclusive)
     */
    def buildModel (testStart: Int = 0, testEnd: Int = 0): (Array [Boolean], DAG)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute conditional mutual information for XY given Z from frequency counts
     *  @see http://www.cs.technion.ac.il/~dang/journal_papers/friedman1997Bayesian.pdf, p.12
     *  @param pz    the probability of Z
     *  @param ptz   the probability of X given Z, or Y given Z
     *  @param pxyz  the probability of Y and Y given Z
     *
    def condMutualInformation (pz: VectorD, ptz: HMatrix3 [Double], pxyz: HMatrix5 [Double]): MatrixD =
    {
        val sum = new MatrixD(ptz.dim2, ptz.dim2)
        for (i <- 0 until ptz.dim2; j <- 0 until ptz.dim2) {
            for (q <- 0 until ptz.dim_3(i); t <- 0 until ptz.dim_3(j)) {
                for (r <- 0 until pz.dim) {
                    sum(i, j) += pxyz(r, i, j, q, t) * log2 ((pz(r) * pxyz(r, i, j, q, t)) / (ptz(r, i, q) * ptz(r, j, t)))
                } // for
            } // for
        } // for
        sum
    } // conMutualInformation
     */

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute conditional mutual information matrix given the marginal probability
     *  of C and joint probabilities of CXZ and CX, where C is the class (parent), and
     *  X & Z are features.
     *  @see en.wikipedia.org/wiki/Conditional_mutual_information
     *  @param p_C   the marginal probability of C
     *  @param p_CX  the joint probability of C and X
     *  @param p_CXZ the joint probability of C, X, and Z
     */
    def cmiJoint (p_C: VectorD, p_CX: HMatrix3 [Double], p_CXZ: HMatrix5 [Double]): MatrixD =
    {
        val cmiMx = new MatrixD (p_CX.dim2, p_CX.dim2)
        for (c <- 0 until k) {                               // check each class, where k = p_C.size
            val pc = p_C(c)
            for (j <- 0 until p_CX.dim2; xj <- 0 until p_CX.dim_3(j)) {
                // n = p_CX.dim2, vc(j) = p_CX.dim_3(j)
                val pcx = p_CX(c, j, xj)
                for (j2 <- j + 1 until p_CX.dim2; xj2 <- 0 until p_CX.dim_3(j2)) {
                    val pcz = p_CX(c, j2, xj2)
                    val pcxz = p_CXZ(c, j, j2, xj, xj2)
                    cmiMx(j, j2) += pcxz * log2((pc * pcxz) / (pcx * pcz))
                } // for
            } // for
        } // for
        cmiMx
    } // cmiJoint

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parent (override as needed).
     */
    def getParent: Any = null

} // BayesClassifier class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifier` object provides factory methods for building Bayes 
 *  classifiers.
 */
object BayesClassifier
{
    /** Perform XFOLD cross-validation
     */
    val XFOLD = 10

    /** Use randomized cross-validation
     */
    val RANDOMIZED = true

    /** The default value for m-estimates (me == 0 => regular MLE estimates)
     *                                     me == 1 => no divide by 0, close to MLE estimates)
     */
//  val me_default = 1.0f
    val me_default = 1E-9f

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Naive Bayes classification model.
     *  @param x   the integer-valued data vectors stored as rows of a matrix
     *  @param y   the class vector, where y(l) = class for row l of the matrix x, x(l)
     *  @param fn  the names for all features/variables
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI, me: Float): NaiveBayes =
    {
        new NaiveBayes (x, y, fn, k, cn, vc, me)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Naive Bayes classification model,  passing 'x' and 'y' together
     *  in one matrix.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names for all features/variables
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI, me: Float): NaiveBayes =
    {
        NaiveBayes (xy, fn, k, cn, vc, me)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Selective Naive Bayes classification model.
     *  @param x     the integer-valued data vectors stored as rows of a matrix
     *  @param y     the class vector, where y(l) = class for row l of the matrix x, x(l)
     *  @param fn    the names for all features/variables
     *  @param k     the number of classes
     *  @param cn    the names for all classes
     *  @param fset  the list of selected features
     *  @param vc    the value count (number of distinct values) for each feature
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               fset: ListBuffer [Int], vc: VectoI, me: Float): SelNaiveBayes =
    {
        new SelNaiveBayes (x, y, fn, k, cn, fset, vc, me)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Selective Naive Bayes classification model, passing 'x' and 'y'
     *  together in one matrix.
     *  @param xy    the data vectors along with their classifications stored as rows of a matrix
     *  @param fn    the names for all features/variables
     *  @param k     the number of classes
     *  @param cn    the names for all classes
     *  @param fset  the list of selected features
     *  @param vc    the value count (number of distinct values) for each feature
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               fset: ListBuffer [Int], vc: VectoI, me: Float): SelNaiveBayes =
    {
        SelNaiveBayes (xy, fn, k, cn, fset, vc, me)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Augmented Naive Bayes (1-BAN) classification model.
     *  @param x      the integer-valued data vectors stored as rows of a matrix
     *  @param y      the class vector, where y(l) = class for row l of the matrix, x(l)
     *  @param fn     the names for all features/variables
     *  @param k      the number of classes
     *  @param cn     the names for all classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI, me: Float, thres: Double): OneBAN =
    {
        new OneBAN (x, y, fn, k, cn, vc, me, thres)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Augmented Naive Bayes (1-BAN) classification model, passing 'x' and 'y'
     *  together in one matrix.
     *  @param xy      the data vectors along with their classifications stored as rows of a matrix
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param vc      the value count (number of distinct values) for each feature
     *  @param me      use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres   the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI, me: Float, thres: Double): OneBAN =
    {
        OneBAN (xy, fn, k, cn, vc, me, thres)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Augmented Selective Naive Bayes (Selective 1-BAN) classification model.
     *  @param x       the integer-valued data vectors stored as rows of a matrix
     *  @param y       the class vector, where y(l) = class for row l the matrix x, x(l)
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param fset    the list of selected features
     *  @param vc      the value count (number of distinct values) for each feature
     *  @param me      use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres   the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               fset: ListBuffer [Int], vc: VectoI, me: Float, thres: Double, smooth: Boolean): SelOneBAN =
    {
        new SelOneBAN (x, y, fn, k, cn, list2Array (fset, fn.length), vc, me, thres)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Augmented Selective Naive Bayes (Selective 1-BAN) classification model,
     *  passing 'x' and 'y' together in one matrix.
     *  @param xy      the data vectors along with their classifications stored as rows of a matrix
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param fset    the list of selected features
     *  @param vc      the value count (number of distinct values) for each feature
     *  @param me      use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres   the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
              fset: ListBuffer [Int], vc: VectoI, me: Float, thres: Double): SelOneBAN =
    {
        SelOneBAN (xy, fn, k, cn, list2Array (fset, fn.length), vc, me, thres)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Tree Augmented Naive Bayes (TAN) classification model.
     *  @param x       the integer-valued data vectors stored as rows of a matrix
     *  @param y       the class vector, where y(l) = class for row l of the matrix, x(l)
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param me      use m-estimates (me == 0 => regular MLE estimates)
     *  @param vc      the value count (number of distinct values) for each feature
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               me: Float, vc: VectoI): TANBayes =
    {
        new TANBayes (x, y, fn, k, cn, me, vc)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Tree Augmented Naive Bayes (TAN) classification model,
     *  passing 'x' and 'y' together in one matrix.
     *  @param xy      the data vectors along with their classifications stored as rows of a matrix
     *  @param fn      the names of the features
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param me      use m-estimates (me == 0 => regular MLE estimates)
     *  @param vc      the value count (number of distinct values) for each feature
     */

    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String], me: Float, vc: VectoI): TANBayes =
    {
        TANBayes (xy, fn, k, cn, me, vc)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Selective Tree Augmented Naive Bayes (Selective TAN) classification model.
     *  @param x     the integer-valued data vectors stored as rows of a matrix
     *  @param y     the class vector, where y(l) = class for row l the matrix x, x(l)
     *  @param fn    the names for all features/variables
     *  @param k     the number of classes
     *  @param cn    the names for all classes
     *  @param fset  the `Boolean` array indicating the selected features
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     *  @param vc    the value count (number of distinct values) for each feature
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               fset: Array [Boolean], me: Float, vc: VectoI): SelTANBayes =
    {
        new SelTANBayes (x, y, fn, k, cn, fset, me, vc)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Selective Tree Augmented Naive Bayes (Selective TAN) classification model,
     *  passing 'x' and 'y' together in one matrix.
     *  @param xy    the data vectors along with their classifications stored as rows of a matrix
     *  @param fn    the names of the features/variables
     *  @param k     the number of classes
     *  @param cn    the names for all classes
     *  @param fset  the `Boolean` array indicating the selected features
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     *  @param vc    the value count (number of distinct values) for each feature
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               fset: Array [Boolean], me: Float, vc: VectoI): SelTANBayes =
    {
        SelTANBayes (xy, fn, k, cn, fset, me, vc)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Bayesian Network (2-BAN-OS) classification model.
     *  @param x      the integer-valued data vectors stored as rows of a matrix
     *  @param y      the class vector, where y(l) = class for row l of the matrix, x(l)
     *  @param fn     the names for all features/variables
     *  @param k      the number of classes
     *  @param cn     the names for all classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI, thres: Double, me: Float): TwoBAN_OS =
    {
        new TwoBAN_OS (x, y, fn, k, cn, vc, thres, me)
    } // appy

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Bayesian Network (2-BAN-OS) classification model, passing 'x' and
     *  'y' together  in one matrix.
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param cn     the names for all classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
              vc: VectoI, thres: Double, me: Float): TwoBAN_OS =
    {
        TwoBAN_OS (xy, fn, k, cn, vc, thres, me)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a selected feature set from a list to a `Boolean` array representation.
     *  @param list  the list of selected features, e.g., (1, 3, 5)
     *  @param n     the total number (selected or not) of features
     */
    def list2Array (list: ListBuffer [Int], n: Int): Array [Boolean] =
    {
        if (list == null) return null
        val arr = Array.fill(n)(false)
        for (js <- list) arr(js) = true
        arr
    } // list2Array

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the given Bayes classifier and return its average accuracy.
     *  @param bc    the Bayes classifier
     *  @param name  name of the Bayes classifier
     */
    def test (bc: BayesClassifier, name: String): Double =
    {
        println ("-" * 60)
        println ("T E S T  " + name)
        bc.buildModel ()
        val avg_accu = if (RANDOMIZED) bc.crossValidateRand (XFOLD)
                       else            bc.crossValidate (XFOLD)
        println ("Average accuracy = " + avg_accu)
        println ("-" * 60)
        avg_accu
    } // test

} // BayesClassifier

import BayesClassifier.{me_default, test}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest` object is used to test the `BayesClassifier` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > run-main scalation.analytics.classifier.BayesClassifierTest
 */
object BayesClassifierTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // y:  Classification (No/0, Yes/1)
    // features:                  x0 x1 x2  y
    val xy = new MatrixI ((10, 4), 1, 0, 1, 1,                 // data matrix
                                   1, 0, 1, 0,
                                   1, 0, 1, 1,
                                   0, 0, 1, 0,
                                   0, 0, 0, 1,
                                   0, 1, 0, 0,
                                   0, 1, 0, 1,
                                   0, 1, 1, 0,
                                   1, 1, 0, 0,
                                   1, 0, 0, 1)

    val fn = Array ("Color", "Type", "Origin")                 // feature/variable names
    val k  = 2                                                 // number of classes
    val cn = Array ("No", "Yes")                               // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    println ("---------------------------------------------------------------")
    println ("D A T A   M A T R I X")
    println ("xy = " + xy)

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, th),     "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest2` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest2
 */
object BayesClassifierTest2 extends App
{
    val fname = BASE_DIR + "bayes_data.csv"                    // file's relative path name
    val (m, n) = (683, 10)                                     // number of (rows/lines, columns) in file

    val xy = ClassifierInt(fname, m, n) // load 'xy' data matrix from file

    xy.setCol (n - 1, xy.col(n - 1).map((z: Int) => z / 2 - 1))       // transform the last column

    val fn = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                    "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val k  = 2
    val cn = Array ("benign", "malignant")
    val vc = VectorI (11, 11, 11, 11, 11, 11, 11, 11, 11)      // value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest3` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest3
 */
object BayesClassifierTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray
    val k  = 2
    val cn = Array ("0", "1")                                  // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest4` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest4
 */
object BayesClassifierTest4 extends App
{
    val filename = BASE_DIR + "adult.txt"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (Seq (0, 1, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14))

    val fn = Array ("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
    val k  = 2
    val cn = Array ("0", "1")                                  // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest5` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest5
 */
object BayesClassifierTest5 extends App
{
    val filename = BASE_DIR + "letter-recognition.data"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null); xy.swapCol (0, xy.dim2 - 1)

    val fn = data.colName.toArray
    val k  = 26
    val cn = Array ("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                    "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")      // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest5 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest6` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest6
 */
object BayesClassifierTest6 extends App
{
    val filename = BASE_DIR + "german.data"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null); xy.setCol (24, xy.col(24).map((z: Int) => z - 1))

    val fn = data.colName.toArray
    val k  = 2
    val cn = Array ("0", "1")                                  // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest6 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest7` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest7
 */
object BayesClassifierTest7 extends App
{
    val filename = BASE_DIR + "flare.data"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray
    val k  = 2
    val cn = Array ("0", "1")                                  // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest7 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest8` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest8
 */
object BayesClassifierTest8 extends App
{
    val filename = BASE_DIR + "connect-4.dat"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray
    val k  = 3
    val cn = Array ("0", "1", "2")                             // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest8 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest9` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest9
 */
object BayesClassifierTest9 extends App
{
    val filename = BASE_DIR + "connect-4.dat"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray
    val k  = 3
    val cn = Array ("0", "1", "2")                             // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest9 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest10` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest10
 */
object BayesClassifierTest10 extends App
{
    val filename = BASE_DIR + "nursery.dat"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray
    val k  = 5
    val cn = Array ("0", "1", "2", "3", "4")                   // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TANaive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest10 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest11` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.BayesClassifierTest11
 */
object BayesClassifierTest11 extends App
{
    val filename = BASE_DIR + "kr-vs-k.dat"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray
    val k  = 18
    val cn = Array ("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                    "10", "11", "12", "13", "14", "15", "16", "17")     // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.3                                               // threshold

    test (BayesClassifier (xy, fn, k, cn, vc, me),           "Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me),     "Selective Naive Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, me, th),       "1-BAN")
    test (BayesClassifier (xy, fn, k, cn, null, vc, me, th), "Selective 1-BAN")
    test (BayesClassifier (xy, fn, k, cn, me, vc),           "TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, null, me, vc),     "Selective TAN Bayes")
    test (BayesClassifier (xy, fn, k, cn, vc, th, me),       "2-BAN-OS")

} // BayesClassifierTest11 object

