
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Aug  8 20:26:34 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import collection.mutable.ListBuffer

import scalation.linalgebra.{MatrixI, VectorI}
import scalation.util.PackageInfo

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifier` object provides factory methods for building Bayesian
 *  classifiers.  The following types of classifiers are currently supported:
 *  `NaiveBayes`       - Naive Bayes classifier
 *  `SelNaiveBayes`    - Selective Naive Bayes classifier
 *  `AugNaiveBayes`    - Augmented Naive Bayes classifier
 *  `AugSelNaiveBayes` - Augmented Selective Naive Bayes classifier
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
    def apply (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectorI, me: Int): NaiveBayes =
    {
        new NaiveBayes (x, y, fn, k, cn, vc, me)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Naive Bayes classification model.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names for all features/variables
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatrixI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectorI, me: Int): NaiveBayes =
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
    def apply (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
               fset: ListBuffer [Int], vc: VectorI, me: Int): SelNaiveBayes =
    {
        new SelNaiveBayes (x, y, fn, k, cn, fset, vc, me)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Selective Naive Bayes classification model.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn    the names for all features/variables
     *  @param k     the number of classes
     *  @param cn    the names for all classes
     *  @param fset  the list of selected features
     *  @param vc    the value count (number of distinct values) for each feature
     *  @param me    use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatrixI, fn: Array [String], k: Int, cn: Array [String],
               fset: ListBuffer [Int], vc: VectorI, me: Int): SelNaiveBayes =
    {
        SelNaiveBayes (xy, fn, k, cn, fset, vc, me)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Augmented Naive Bayes classification model.
     *  @param x      the integer-valued data vectors stored as rows of a matrix
     *  @param y      the class vector, where y(l) = class for row l of the matrix, x(l)
     *  @param fn     the names for all features/variables
     *  @param k      the number of classes
     *  @param cn     the names for all classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectorI, me: Int, thres: Double): AugNaiveBayes =
    {
        new AugNaiveBayes (x, y, fn, k, cn, vc, me, thres)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Augmented Naive Bayes classification model.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names for all features/variables
     *  @param k      the number of classes
     *  @param cn     the names for all classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatrixI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectorI, me: Int, thres: Double): AugNaiveBayes =
    {
        AugNaiveBayes (xy, fn, k, cn, vc, me, thres)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Augmented Selective Naive Bayes classification model.
     *  @param x      the integer-valued data vectors stored as rows of a matrix
     *  @param y      the class vector, where y(l) = class for row l the matrix x, x(l)
     *  @param fn     the names for all features/variables
     *  @param k      the number of classes
     *  @param cn     the names for all classes
     *  @param fset   the list of selected features
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
               fset: ListBuffer [Int], vc: VectorI, me: Int, thres: Double): AugSelNaiveBayes =
    {
        new AugSelNaiveBayes (x, y, fn, k, cn, list2Array (fset, fn.length), vc, me, thres)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Augmented Selective Naive Bayes classification model.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names for all features/variables
     *  @param k      the number of classes
     *  @param cn     the names for all classes
     *  @param fset   the list of selected features
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatrixI, fn: Array [String], k: Int, cn: Array [String],
               fset: ListBuffer [Int], vc: VectorI, me: Int, thres: Double): AugSelNaiveBayes =
    {
        AugSelNaiveBayes (xy, fn, k, cn, list2Array (fset, fn.length), vc, me, thres)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a selected feature set from a list to a Boolean array representation.
     *  @param list  the list of selected features, e.g., (1, 3, 5) 
     *  @param n     the total number (selected or not) of features 
     */
    def list2Array (list: ListBuffer [Int], n: Int): Array [Boolean] =
    {
        if (list == null) return null
        val arr = Array.fill (n) (false)
        for (js <- list) arr(js) = true
        arr
    } // list2Array

} // BayesClassifier


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest` object is used to test the 'BayesClassifier' class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > run-main scalation.analytics.BayesClassifierTest
 */
object BayesClassifierTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // y:  Classification (No/0, Yes/1)
    // features:                  x0  x1  x2   y
    val xy = new MatrixI ((10, 4), 1,  0,  1,  1,                // data matrix
                                   1,  0,  1,  0,
                                   1,  0,  1,  1,
                                   0,  0,  1,  0,
                                   0,  0,  0,  1,
                                   0,  1,  0,  0,
                                   0,  1,  0,  1,
                                   0,  1,  1,  0,
                                   1,  1,  0,  0,
                                   1,  0,  0,  1)

    val fn = Array ("Color", "Type", "Origin")                   // feature/variable names
    val k  = 2                                                   // number of classes
    val cn = Array ("No", "Yes")                                 // class names

    println ("---------------------------------------------------------------")
    println ("D A T A   M A T R I X")
    println ("xy = " + xy)

    println ("---------------------------------------------------------------")
    println ("T E S T   N A I V E   B A Y E S")
    val nvb = BayesClassifier (xy, fn, k, cn, null, 1)              // create a Naive Bayes classifier
    nvb.crossValidate ()

    println ("T E S T   S E L E C T I V E    N A I V E   B A Y E S")
    val snb = BayesClassifier (xy, fn, k, cn, null, null, 1)        // create a Selective Naive Bayes classifier
    snb.crossValidate ()

    println ("---------------------------------------------------------------")
    println ("T E S T   A U G M E N T E D   N A I V E   B A Y E S")
    val anb = BayesClassifier (xy, fn, k, cn, null, 1, 0.3)         // create an Augmented Naive Bayes classifier
    anb.crossValidate ()

    println ("---------------------------------------------------------------")
    println ("T E S T   A U G M E N T E D   S E L E C T I V E    N A I V E   B A Y E S")
    val asb = BayesClassifier (xy, fn, k, cn, null, null, 1, 0.3)   // create an Augmented Selective Naive Bayes classifier
    asb.crossValidate ()

} // BayesClassifierTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifierTest2` object is used to test the 'BayesClassifier' class.
 *  > run-main scalation.analytics.BayesClassifierTest2
 */
object BayesClassifierTest2 extends App with PackageInfo
{
    val fname  = getDataPath + "bayes_data.csv"                   // file's relative path name
    val (m, n) = (683, 10)                                        // number of (rows/lines, columns) in file

    val xy = ClassifierInt (fname, m, n)                          // load 'xy' data matrix from file

    xy.setCol (n-1, xy.col (n-1).map ((z: Int) => z / 2 - 1))     // transform the last column

    val fn = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                    "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val k  = 2
    val cn = Array ("benign", "malignant")
    val vc = VectorI (11, 11, 11, 11, 11, 11, 11, 11, 11)

    println ("---------------------------------------------------------------")
    println ("D A T A   M A T R I X")
    println ("xy = " + xy)

    println ("---------------------------------------------------------------")
    println ("T E S T   N A I V E   B A Y E S")
    val nvb = BayesClassifier (xy, fn, k, cn, vc, 1)              // create a Naive Bayes classifier
    nvb.crossValidate ()

    println ("T E S T   S E L E C T I V E    N A I V E   B A Y E S")
    val snb = BayesClassifier (xy, fn, k, cn, null, vc, 1)        // create a Selective Naive Bayes classifier
    snb.crossValidate ()

    println ("---------------------------------------------------------------")
    println ("T E S T   A U G M E N T E D   N A I V E   B A Y E S")
    val anb = BayesClassifier (xy, fn, k, cn, vc, 1, 0.3)         // create an Augmented Naive Bayes classifier
    anb.crossValidate ()

    println ("---------------------------------------------------------------")
    println ("T E S T   A U G M E N T E D   S E L E C T I V E    N A I V E   B A Y E S")
    val asb = BayesClassifier (xy, fn, k, cn, null, vc, 1, 0.3)   // create an Augmented Selective Naive Bayes classifier
    asb.crossValidate ()

} // BayesClassifierTest2 object

