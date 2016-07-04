
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhe Jin
 *  @version 1.2
 *  @date    Sat Aug  8 20:26:34 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier.par

import scala.collection.mutable.ListBuffer

import scalation.analytics.classifier.{BayesMetrics, ClassifierInt}
import scalation.linalgebra.gen.{HMatrix3, HMatrix5}
import scalation.linalgebra._
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
 *  `AugNaiveBayes`    - Augmented Naive Bayes classifier
 *  `AugSelNaiveBayes` - Augmented Selective Naive Bayes classifier
 *  `TANBayes`         - Tree Augmented Naive Bayes classifier
 *  `SelTAN`           - Selective Tree Augmented Naive Bayes classifier
 *  `BayesNetwork2`    - Ordering-based Bayesian Network with k = 2
 */
abstract class BayesClassifier (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String])
    extends ClassifierInt(x, y, fn, k, cn) with BayesMetrics
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the model with feature order and selection
     *  @param testStart  beginning of test region. (inclusive)
     *  @param testEnd    end of test region. (exclusive)
     */
    def buildModel(testStart: Int = 0, testEnd: Int = 0): (Array [Boolean], DAG)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute conditional mutual information for XY given Z from frequency counts
     *  @see http://www.cs.technion.ac.il/~dang/journal_papers/friedman1997Bayesian.pdf, p.12
     *  @param pz    the probability of Z
     *  @param ptz   the probability of X given Z, or Y given Z
     *  @param pxyz  the probability of Y and Y given Z
     */
    def condMutualInformation(pz: VectorD, ptz: HMatrix3[Double], pxyz: HMatrix5[Double]): MatrixD =
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
    } // mutualInformation

} // BayesClassifier class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifier` object provides factory methods for building Bayes
 *  classifiers.
 */
object BayesClassifier
{
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
               vc: VectoI, me: Int): NaiveBayes =
    {
        new NaiveBayes (x, y, fn, k, cn, vc, me)
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
               vc: VectoI, me: Int): NaiveBayes =
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
               me: Int, fset: ListBuffer [Int], vc: VectoI): SelNaiveBayes =
    {
        new SelNaiveBayes (x, y, fn, k, cn, me, fset, vc)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Selective Naive Bayes classification model,
     *  passing 'x' and 'y' together in one matrix.
     *  @param xy    the data vectors along with their classifications stored as rows of a matrix
     *  @param fn    the names for all features/variables
     *  @param k     the number of classes
     *  @param cn    the names for all classes
     *  @param fset  the list of selected features
     *  @param vc    the value count (number of distinct values) for each feature
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               me: Int ,fset: ListBuffer[Int], vc: VectoI): SelNaiveBayes =
    {
        SelNaiveBayes (xy, fn, k, cn, me, fset, vc)
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
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
               thres: Double, me: Int, vc: VectoI): TANBayes =
    {
        new TANBayes (x, y, fn, k, cn, thres, me, vc)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Tree Augmented Naive Bayes Classification model,
     *  passing 'x' and 'y' together in one matrix.
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               thres: Double, me: Int, vc: VectoI): TANBayes =
    {
        TANBayes (xy, fn, k, cn, thres, me, vc)
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
               thres: Double, vc: VectoI, me: Int): BayesNetwork2 =
    {
        new BayesNetwork2 (x, y, fn, vc, k, cn, thres, me)
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
               thres: Double,vc: VectoI, me: Int): BayesNetwork2 =
    {
        BayesNetwork2 (xy, fn, vc, k, cn, thres, me)
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
    def test (bc: BayesClassifier, name: String)
    {
        println ("-" * 50)
        println ("Test " + name)
        bc.buildModel ()
        println ("Average accuracy = " + bc.crossValidate())
        println ("-" * 50)
    } //test

} // BayesClassifier

import BayesClassifier.test

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
    val xy = new MatrixI((10, 4), 1, 0, 1, 1,                 // data matrix
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
    val cn = Array ("No", "Yes")                               // class names
    val k  = 2                                                 // number of classes

    println ("---------------------------------------------------------------")
    println ("D A T A   M A T R I X")
    println ("xy = " + xy)

    test (BayesClassifier (xy, fn, k, cn, null, 1),                "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1,null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),           "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc = null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest2` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest2
 */
object BayesClassifierTest2 extends App
{
    val fname = BASE_DIR + "bayes_data.csv"       // file's relative path name
    val (m, n) = (683, 10)                        // number of (rows/lines, columns) in file

    val xy = ClassifierInt (fname, m, n)          // load 'xy' data matrix from file

    xy.setCol (n - 1, xy.col(n - 1).map((z: Int) => z / 2 - 1))

    val fn = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                    "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val vc = VectorI (11, 11, 11, 11, 11, 11, 11, 11, 11)
    val cn = Array ("benign", "malignant")
    val k  = 2

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest3` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest3
 */
object BayesClassifierTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1")
    val k  = 2

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest4` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest4
 */
object BayesClassifierTest4 extends App {
    val filename = BASE_DIR + "adult.txt"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (Seq (0, 1, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14))
    val fn = Array ("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
    val cn = Array ("0", "1")
    val k = 2

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest5` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest5
 */
object BayesClassifierTest5 extends App
{
    val filename = BASE_DIR + "letter-recognition.data"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    xy.swapCol (0, xy.dim2 - 1)
    val fn = data.colName.toArray
    val cn = Array ("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                    "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    val k = 26

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest5 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest6` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest6
 */
object BayesClassifierTest6 extends App
{
    val filename = BASE_DIR + "german.data"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null); xy.setCol (24, xy.col (24).map ((z: Int) => z - 1))
    val fn = data.colName.toArray
    val cn = Array ("0", "1")                                       // class names
    val k  = 2

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest6 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest7` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest7
 */
object BayesClassifierTest7 extends App
{
    val filename = BASE_DIR + "flare.data"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1")
    val k  = 2

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest7 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest8` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest8
 */
object BayesClassifierTest8 extends App
{
    val filename = BASE_DIR + "connect-4.dat"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1", "2")
    val k  = 3

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest8 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest9` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest9
 */
object BayesClassifierTest9 extends App
{
    val filename = BASE_DIR + "connect-4.dat"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1", "2")
    val k  = 3

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest9 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest10` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest10
 */
object BayesClassifierTest10 extends App
{
    val filename = BASE_DIR + "nursery.dat"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1", "2", "3", "4")
    val k  = 5

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest10 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest11` object is used to test the `BayesClassifier` class.
 *  > run-main scalation.analytics.classifier.par.BayesClassifierTest11
 */
object BayesClassifierTest11 extends App
{
    val filename = BASE_DIR + "kr-vs-k.dat"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1", "2", "3", "4", "5", "6", "7", "8",
                    "9", "10", "11", "12", "13", "14", "15", "16", "17")
    val k  = 18

    test (BayesClassifier (xy, fn, k, cn, null, 1),                 "Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 1, null, null),           "Selective Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, 1, null),            "Tree Augmented Naive Bayes classifier")
    test (BayesClassifier (xy, fn, k, cn, 0.3, vc =  null, me = 1), "Bayesian Network2 classifier")

} // BayesClassifierTest11 object

