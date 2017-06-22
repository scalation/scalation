
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng, Zhe Jin
 *  @version 1.3
 *  @date    Sat Aug  8 20:26:34 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier.par

import java.util.concurrent.ForkJoinPool

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scalation.analytics.classifier.{BayesMetrics, ClassifierInt}
import scalation.linalgebra.gen.{HMatrix2, HMatrix3, HMatrix5}
import scalation.linalgebra.{MatriI, MatrixD, MatrixI, VectoI, VectorD, VectorI}
import scalation.math.log2
import scalation.relalgebra.Relation

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifier` object provides factory methods for building Bayesian
 *  classifiers.  The following types of classifiers are currently supported:
 *  `NaiveBayes`       - Naive Bayes classifier
 *  `SelNaiveBayes`    - Selective Naive Bayes classifier
 *  `TANBayes`         - Tree Augmented Naive Bayes classifier
 *  `SelTAN`           - Selective Tree Augmented Naive Bayes classifier
 *  `TwoBAN_OS`        - Ordering-based Bayesian Network with k = 2
 */
abstract class BayesClassifier (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String], private val PARALLELISM: Int = Runtime.getRuntime().availableProcessors())
        extends ClassifierInt (x, y, fn, k, cn) with BayesMetrics
{
    protected var smooth   = true                    // flag for using parameter smoothing
    protected val N0       = 5.0                     // parameter needed for smoothing
    protected val tiny     = 1E-9                    // value needed for CMI calculations
    protected var additive = true                    // flag to use additive approach for training/cross-validation

    protected val f_C = new VectorI (k)              // frequency counts for classes 0, ..., k-1
    protected var p_C = new VectorD (k)              // probabilities for classes 0, ..., k-1

    protected var f_X:   HMatrix2 [Int] = null       // Frequency of X
    protected var f_CX:  HMatrix3 [Int] = null       // Joint frequency of C and X
    protected var f_CXZ: HMatrix5 [Int] = null       // Joint frequency of C, X, and Z, where X, Z are features/columns

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle the value of the 'smooth' property.
     */
    def toggleSmooth () { smooth = ! smooth}

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the conditional mutual information matrix
     */
    def calcCMI (idx: IndexedSeq [Int], vca: Array [Int]): MatrixD =
    {
        val p_CXZ = new HMatrix5 [Double] (k, n, n, vca, vca)   // Joint probability of C, X, and Z, where X, Z are features/columns
        val p_CX  = new HMatrix3 [Double] (k, n, vca)           // Joint probability of C and X
        var p_C: VectorD = null

        reset ()
        val idxA = split (idx, PARALLELISM)

        val f_Cw   = Array.ofDim [VectorI] (PARALLELISM)
        val f_Xw   = Array.ofDim [HMatrix2 [Int]] (PARALLELISM)
        val f_CXw  = Array.ofDim [HMatrix3 [Int]] (PARALLELISM)
        val f_CXZw = Array.ofDim [HMatrix5 [Int]] (PARALLELISM)

        for (w <- (0 until PARALLELISM).par) {
            f_Cw (w)   = new VectorI (k)
            f_Xw (w)   = new HMatrix2 [Int] (n, vca)
            f_CXw (w)  = new HMatrix3 [Int] (k, n, vca)
            f_CXZw (w) = new HMatrix5 [Int] (k, n, n, vca, vca)
        } // for

        val paraRange = (0 until PARALLELISM).par
        paraRange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))

        for (w <- paraRange; i <- idxA(w)) updateFreq (i, f_Cw(w), f_Xw(w), f_CXw(w), f_CXZw(w))

        for (w <- 0 until PARALLELISM) {
            f_C  += f_Cw (w)
            f_X  += f_Xw (w)
            f_CX += f_CXw (w)
            f_CXZ += f_CXZw(w)
        } // for

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compute marginal and joint probabilities
         */
        def probabilities ()
        {
            for (j <- (0 until n).par if fset(j)) {
                for (xj <- (0 until vca(j)).par) {
                    //p_X(j, xj) = (f_X(j, xj)) / md
                    for (c <- (0 until k).par) {
                        p_CX(c, j, xj) = (f_CX(c, j, xj) + tiny) / md
                        for (j2 <- (j + 1 until n).par if fset(j2); xj2 <- (0 until vca(j2)).par) {
                            p_CXZ(c, j, j2, xj, xj2) = (f_CXZ(c, j, j2, xj, xj2) + tiny) / md
                        } // for
                    } // for
                } // for
            } // for
        } // probabilities

        p_C = f_C.toDouble / m
        probabilities ()

        cmiJoint (p_C, p_CX, p_CXZ)
    } // calcCMI

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute conditional mutual information matrix given the marginal probability
     *  of C and joint probabilities of CXZ and CX, where C is the class (parent), and
     *  X & Z are features.
     *  @see en.wikipedia.org/wiki/Conditional_mutual_information
     *  @param p_C    the marginal probability of C
     *  @param p_CX   the joint probability of C and X
     *  @param p_CXZ  the joint probability of C, X, and Z
     */
    def cmiJoint (p_C: VectorD, p_CX: HMatrix3 [Double], p_CXZ: HMatrix5 [Double]): MatrixD =
    {
        val cmiMx = new MatrixD (p_CX.dim2, p_CX.dim2)
        for (c <- (0 until k).par) {                                                        // check each class, where k = p_C.size
            val pc = p_C(c)
            for (j <- (0 until p_CX.dim2).par if fset(j); xj <- (0 until p_CX.dim_3(j)).par) {         // n = p_CX.dim2, vc(j) = p_CX.dim_3(j)
            val pcx = p_CX(c, j, xj)
                for (j2 <- (j+1 until p_CX.dim2).par if fset(j2); xj2 <- (0 until p_CX.dim_3(j2)).par) {
                    val pcz  = p_CX(c, j2, xj2)
                    val pcxz = p_CXZ (c, j, j2, xj, xj2)
                    cmiMx (j, j2) += pcxz * log2( (pc * pcxz) / (pcx * pcz) )
                } // for
            } // for
        } // for
        cmiMx
    } // cmiJoint

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param itest  indices of the instances considered test data
     */
    def test (itest: Array[Int]): Double =
    {
        val itestA = new VectorI (itest.size, itest).split (PARALLELISM)
        var correctA = Array.fill[Int] (PARALLELISM)(0)

        val paraRange = (0 until PARALLELISM).par
        paraRange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))

        for (w <- paraRange; i <- itestA(w) if classify (x(i))._1 == y(i)) correctA(w) += 1

        correctA.sum / itest.size.toDouble
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split 'indices' into 'k' arrays of equal sizes (perhaps except for the last one)
     *  @param indices  the ParSeq to be splitted
     *  @param k        the number of pieces the vector is to be splitted
     */
    def split (indices: IndexedSeq [Int], k: Int): Array [IndexedSeq[Int]] =
    {
        if (k <= 0) flaw ("split", "k must be a positive integer")
        val pieces = Array.ofDim [IndexedSeq [Int]] (k)
        val size = indices.size / k
        for (i <- (0 until k-1).par) pieces(i) = indices.slice (i*size, (i+1)*size)
        pieces(k-1) = indices.slice ((k-1)*size, indices.size)
        pieces
    } // split

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment/Decrement frequency counters based on the 'i'th row of the
     *  data matrix. Only to be used for CMI frequency calculations.
     *  @param i  the index for current data row
     */
    protected def updateFreq (i: Int, f_C: VectorI, f_X: HMatrix2[Int], f_CX: HMatrix3[Int], f_CXZ: HMatrix5[Int]) {}

} // BayesClassifier class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifier` object provides factory methods for building Bayes
 *  classifiers.
 */
object BayesClassifier
{
    /** The default value for m-estimates (me == 0 => regular MLE estimates)
     *                                     me == 1 => no divide by 0, close to MLE estimates)
     */
    val me_default = 1E-9

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
               vc: VectoI, me: Double, PARALLELISM: Int): NaiveBayes =
    {
        new NaiveBayes (x, y, fn, k, cn, vc, me, PARALLELISM)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Naive Bayes classification model,
     *  passing 'x' and 'y' together in one matrix.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names for all features/variables
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI, me: Double, PARALLELISM: Int): NaiveBayes =
    {
        NaiveBayes (xy, fn, k, cn, vc, me, PARALLELISM)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Tree Augmented Naive Bayes Classification model
     *  @param x      the integer-valued data vectors stored as rows of a matrix
     *  @param y      the class vector, where y(l) = class for row l of the matrix, x(l)
     *  @param fn     the names for all features/variables
     *  @param k      the number of classes
     *  @param cn     the names for all classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               me: Double, vc: VectoI, PARALLELISM: Int): TANBayes =
    {
        new TANBayes (x, y, fn, k, cn, me, vc, PARALLELISM)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Tree Augmented Naive Bayes Classification model,
     *  passing 'x' and 'y' together in one matrix.
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               me: Double, vc: VectoI, PARALLELISM: Int): TANBayes =
    {
        TANBayes (xy, fn, k, cn, me, vc, PARALLELISM)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Bayesian Network 2 classification model.
     *  @param x     the integer-valued data vectors stored as rows of a matrix
     *  @param y     the class vector, where y(l) = class for row l of the matrix, x(l)
     *  @param fn    the names for all features/variables
     *  @param k     the number of classes
     *  @param cn    the names for all classes
     *  @param vc    the value count (number of distinct values) for each feature
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI, thres: Double, me: Double, PARALLELISM: Int): TwoBAN_OS =
    {
        new TwoBAN_OS (x, y, fn, k, cn, vc, thres, me, PARALLELISM)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Bayesian Network 2 classification model, passing 'x' and 'y' together in one matrix.
     *  @param xy    the data vectors along with their classifications stored as rows of a matrix
     *  @param fn    the names of the features
     *  @param k     the number of classes
     *  @param vc    the value count (number of distinct values) for each feature
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI, thres: Double, me: Double, PARALLELISM: Int): TwoBAN_OS =
    {
        TwoBAN_OS (xy, fn, k, cn, vc, thres, me, PARALLELISM)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a selected feature set from a list to a `Boolean` array representation.
     *  @param list  the list of selected features, e.g., (1, 3, 5)
     *  @param n     the total number (selected or not) of features
     */
    def list2Array (list: ListBuffer [Int], n: Int): Array [Boolean] =
    {
        if (list == null) return null
        val arr = Array.fill (n)(false)
        for (js <- list) arr(js) = true
        arr
    } // list2Array

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create and test a Bayes Classifier
     *  @param bc      the Bayes Classifier
     *  @param name    name of the classifier
     */
    def test (bc: BayesClassifier, name: String): Double =
    {
        println ("-" * 50)
        println ("T E S T  " + name)
        val avg_accu = bc.crossValidateRand (10)
        println ("Average accuracy = " + avg_accu)
        println ("-" * 50)
        avg_accu
    } // test

} // BayesClassifier object

import BayesClassifier.{me_default, test}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest` object is used to test the `BayesClassifier` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest
 */
object BayesClassifierTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // y:  Classification (No/0, Yes/1)
    // features:                 x0 x1 x2  y
    val xy = new MatrixI((10, 4), 1, 0, 1, 1,                  // data matrix
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
    val th = 0.0                                               // threshold

    println ("---------------------------------------------------------------")
    println ("D A T A   M A T R I X")
    println ("xy = " + xy)

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest2` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest2
 */
object BayesClassifierTest2 extends App
{
    val fname = BASE_DIR + "bayes_data.csv"                    // file's relative path name
    val (m, n) = (683, 10)                                     // number of (rows/lines, columns) in file

    val xy = ClassifierInt(fname, m, n)                        // load 'xy' data matrix from file

    xy.setCol (n - 1, xy.col(n - 1).map((z: Int) => z / 2 - 1))// transform the last column

    val fn = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
        "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val k  = 2
    val cn = Array ("benign", "malignant")
    val vc = VectorI (11, 11, 11, 11, 11, 11, 11, 11, 11)      // value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest3` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest3
 */
object BayesClassifierTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val k  = 2
    val cn = Array ("0", "1")                                  // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest4` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest4
 */
object BayesClassifierTest4 extends App {
    val filename = BASE_DIR + "adult.txt"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (Seq (0, 1, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14))

    val fn = Array ("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
    val k  = 2
    val cn = Array ("0", "1")                                  // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest5` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest5
 */
object BayesClassifierTest5 extends App
{
    val filename = BASE_DIR + "letter-recognition.data"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null); xy.swapCol (0, xy.dim2 - 1)

    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val k  = 26
    val cn = Array ("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                    "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")      // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest5 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest6` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest6
 */
object BayesClassifierTest6 extends App
{
    val filename = BASE_DIR + "german.data"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null); xy.setCol (24, xy.col(24).map((z: Int) => z - 1))

    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val k  = 2
    val cn = Array ("0", "1")                                  // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest6 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest7` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest7
 */
object BayesClassifierTest7 extends App
{
    val filename = BASE_DIR + "flare.data"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val k  = 2
    val cn = Array ("0", "1")                                  // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest7 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest8` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest8
 */
object BayesClassifierTest8 extends App
{
    val filename = BASE_DIR + "connect-4.dat"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val k  = 3
    val cn = Array ("0", "1", "2")                             // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest8 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest9` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest9
 */
object BayesClassifierTest9 extends App
{
    val filename = BASE_DIR + "connect-4.dat"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val k  = 3
    val cn = Array ("0", "1", "2")                             // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest9 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest10` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest10
 */
object BayesClassifierTest10 extends App
{
    val filename = BASE_DIR + "nursery.dat"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val k  = 5
    val cn = Array ("0", "1", "2", "3", "4")                   // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest10 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest11` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest11
 */
object BayesClassifierTest11 extends App
{
    val filename = BASE_DIR + "kr-vs-k.dat"
    val data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)

    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val k  = 18
    val cn = Array ("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                    "10", "11", "12", "13", "14", "15", "16", "17")     // class names
    val vc = null.asInstanceOf [VectoI]                        // use default value count
    val me = me_default                                        // me-estimates
    val th = 0.0                                               // threshold

    val PARALLELISM = if (args.nonEmpty) args(0).toInt else Runtime.getRuntime().availableProcessors()

    val nb     = BayesClassifier (xy, fn, k, cn, vc, me, PARALLELISM)
    test (nb,         "Naive Bayes")

    val tan    = BayesClassifier (xy, fn, k, cn, me, vc, PARALLELISM)
    test (tan,        "TAN Bayes")

    val twoban = BayesClassifier (xy, fn, k, cn, vc, th, me, PARALLELISM)
    test (twoban,     "2-BAN-OS")

    nb.featureSelection ()
    test (nb,         "Selective Naive Bayes")

    tan.featureSelection ()
    test (tan,        "Selective TAN Bayes")

    twoban.featureSelection ()
    test (twoban,     "Selective 2-BAN-OS")

} // BayesClassifierTest11 object

