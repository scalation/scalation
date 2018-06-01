
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Fri Feb 16 16:14:34 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scalation.linalgebra.{MatrixD, VectoI}
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleIris` object is used to test all classifiers.
 *  This is the well-known classification problem on how to classify a flower
 *  <p>
 *      val x = xy.sliceCol (1, 5)      // columns 1, 2, 3, 4
 *      val y = xy.col (5).toInt        // column 5
 *  <p>
 *  @see https://en.wikipedia.org/wiki/Iris_flower_data_set
 */
object ExampleIris
{
    // combined data matrix [ x | y ]
    // dataset ----------------------------------------------------------------
    // Columns: Index, Sepal length, Sepal width, Petal length, Petal width, Species
    // x0: Sepal length
    // x1: Sepal width
    // x2: Petal length
    // x3: Petal width
    // y:  Species - the response/classification decision
    // variables/features:        idx   x0   x1   x2   x3  y
    val xy = new MatrixD ((150, 6), 1, 5.1, 3.5, 1.4, 0.2, 0,    // I. setosa
                                    2, 4.9, 3.0, 1.4, 0.2, 0,    // I. setosa
                                    3, 4.7, 3.2, 1.3, 0.2, 0,    // I. setosa
                                    4, 4.6, 3.1, 1.5, 0.2, 0,    // I. setosa
                                    5, 5.0, 3.6, 1.4, 0.3, 0,    // I. setosa
                                    6, 5.4, 3.9, 1.7, 0.4, 0,    // I. setosa
                                    7, 4.6, 3.4, 1.4, 0.3, 0,    // I. setosa
                                    8, 5.0, 3.4, 1.5, 0.2, 0,    // I. setosa
                                    9, 4.4, 2.9, 1.4, 0.2, 0,    // I. setosa
                                   10, 4.9, 3.1, 1.5, 0.1, 0,    // I. setosa
                                   11, 5.4, 3.7, 1.5, 0.2, 0,    // I. setosa
                                   12, 4.8, 3.4, 1.6, 0.2, 0,    // I. setosa
                                   13, 4.8, 3.0, 1.4, 0.1, 0,    // I. setosa
                                   14, 4.3, 3.0, 1.1, 0.1, 0,    // I. setosa
                                   15, 5.8, 4.0, 1.2, 0.2, 0,    // I. setosa
                                   16, 5.7, 4.4, 1.5, 0.4, 0,    // I. setosa
                                   17, 5.4, 3.9, 1.3, 0.4, 0,    // I. setosa
                                   18, 5.1, 3.5, 1.4, 0.3, 0,    // I. setosa
                                   19, 5.7, 3.8, 1.7, 0.3, 0,    // I. setosa
                                   20, 5.1, 3.8, 1.5, 0.3, 0,    // I. setosa
                                   21, 5.4, 3.4, 1.7, 0.2, 0,    // I. setosa
                                   22, 5.1, 3.7, 1.5, 0.4, 0,    // I. setosa
                                   23, 4.6, 3.6, 1.0, 0.2, 0,    // I. setosa
                                   24, 5.1, 3.3, 1.7, 0.5, 0,    // I. setosa
                                   25, 4.8, 3.4, 1.9, 0.2, 0,    // I. setosa
                                   26, 5.0, 3.0, 1.6, 0.2, 0,    // I. setosa
                                   27, 5.0, 3.4, 1.6, 0.4, 0,    // I. setosa
                                   28, 5.2, 3.5, 1.5, 0.2, 0,    // I. setosa
                                   29, 5.2, 3.4, 1.4, 0.2, 0,    // I. setosa
                                   30, 4.7, 3.2, 1.6, 0.2, 0,    // I. setosa
                                   31, 4.8, 3.1, 1.6, 0.2, 0,    // I. setosa
                                   32, 5.4, 3.4, 1.5, 0.4, 0,    // I. setosa
                                   33, 5.2, 4.1, 1.5, 0.1, 0,    // I. setosa
                                   34, 5.5, 4.2, 1.4, 0.2, 0,    // I. setosa
                                   35, 4.9, 3.1, 1.5, 0.2, 0,    // I. setosa
                                   36, 5.0, 3.2, 1.2, 0.2, 0,    // I. setosa
                                   37, 5.5, 3.5, 1.3, 0.2, 0,    // I. setosa
                                   38, 4.9, 3.6, 1.4, 0.1, 0,    // I. setosa
                                   39, 4.4, 3.0, 1.3, 0.2, 0,    // I. setosa
                                   40, 5.1, 3.4, 1.5, 0.2, 0,    // I. setosa
                                   41, 5.0, 3.5, 1.3, 0.3, 0,    // I. setosa
                                   42, 4.5, 2.3, 1.3, 0.3, 0,    // I. setosa
                                   43, 4.4, 3.2, 1.3, 0.2, 0,    // I. setosa
                                   44, 5.0, 3.5, 1.6, 0.6, 0,    // I. setosa
                                   45, 5.1, 3.8, 1.9, 0.4, 0,    // I. setosa
                                   46, 4.8, 3.0, 1.4, 0.3, 0,    // I. setosa
                                   47, 5.1, 3.8, 1.6, 0.2, 0,    // I. setosa
                                   48, 4.6, 3.2, 1.4, 0.2, 0,    // I. setosa
                                   49, 5.3, 3.7, 1.5, 0.2, 0,    // I. setosa
                                   50, 5.0, 3.3, 1.4, 0.2, 0,    // I. setosa
                                   51, 7.0, 3.2, 4.7, 1.4, 1,    // I. versicolor
                                   52, 6.4, 3.2, 4.5, 1.5, 1,    // I. versicolor
                                   53, 6.9, 3.1, 4.9, 1.5, 1,    // I. versicolor
                                   54, 5.5, 2.3, 4.0, 1.3, 1,    // I. versicolor
                                   55, 6.5, 2.8, 4.6, 1.5, 1,    // I. versicolor
                                   56, 5.7, 2.8, 4.5, 1.3, 1,    // I. versicolor
                                   57, 6.3, 3.3, 4.7, 1.6, 1,    // I. versicolor
                                   58, 4.9, 2.4, 3.3, 1.0, 1,    // I. versicolor
                                   59, 6.6, 2.9, 4.6, 1.3, 1,    // I. versicolor
                                   60, 5.2, 2.7, 3.9, 1.4, 1,    // I. versicolor
                                   61, 5.0, 2.0, 3.5, 1.0, 1,    // I. versicolor
                                   62, 5.9, 3.0, 4.2, 1.5, 1,    // I. versicolor
                                   63, 6.0, 2.2, 4.0, 1.0, 1,    // I. versicolor
                                   64, 6.1, 2.9, 4.7, 1.4, 1,    // I. versicolor
                                   65, 5.6, 2.9, 3.6, 1.3, 1,    // I. versicolor
                                   66, 6.7, 3.1, 4.4, 1.4, 1,    // I. versicolor
                                   67, 5.6, 3.0, 4.5, 1.5, 1,    // I. versicolor
                                   68, 5.8, 2.7, 4.1, 1.0, 1,    // I. versicolor
                                   69, 6.2, 2.2, 4.5, 1.5, 1,    // I. versicolor
                                   70, 5.6, 2.5, 3.9, 1.1, 1,    // I. versicolor
                                   71, 5.9, 3.2, 4.8, 1.8, 1,    // I. versicolor
                                   72, 6.1, 2.8, 4.0, 1.3, 1,    // I. versicolor
                                   73, 6.3, 2.5, 4.9, 1.5, 1,    // I. versicolor
                                   74, 6.1, 2.8, 4.7, 1.2, 1,    // I. versicolor
                                   75, 6.4, 2.9, 4.3, 1.3, 1,    // I. versicolor
                                   76, 6.6, 3.0, 4.4, 1.4, 1,    // I. versicolor
                                   77, 6.8, 2.8, 4.8, 1.4, 1,    // I. versicolor
                                   78, 6.7, 3.0, 5.0, 1.7, 1,    // I. versicolor
                                   79, 6.0, 2.9, 4.5, 1.5, 1,    // I. versicolor
                                   80, 5.7, 2.6, 3.5, 1.0, 1,    // I. versicolor
                                   81, 5.5, 2.4, 3.8, 1.1, 1,    // I. versicolor
                                   82, 5.5, 2.4, 3.7, 1.0, 1,    // I. versicolor
                                   83, 5.8, 2.7, 3.9, 1.2, 1,    // I. versicolor
                                   84, 6.0, 2.7, 5.1, 1.6, 1,    // I. versicolor
                                   85, 5.4, 3.0, 4.5, 1.5, 1,    // I. versicolor
                                   86, 6.0, 3.4, 4.5, 1.6, 1,    // I. versicolor
                                   87, 6.7, 3.1, 4.7, 1.5, 1,    // I. versicolor
                                   88, 6.3, 2.3, 4.4, 1.3, 1,    // I. versicolor
                                   89, 5.6, 3.0, 4.1, 1.3, 1,    // I. versicolor
                                   90, 5.5, 2.5, 4.0, 1.3, 1,    // I. versicolor
                                   91, 5.5, 2.6, 4.4, 1.2, 1,    // I. versicolor
                                   92, 6.1, 3.0, 4.6, 1.4, 1,    // I. versicolor
                                   93, 5.8, 2.6, 4.0, 1.2, 1,    // I. versicolor
                                   94, 5.0, 2.3, 3.3, 1.0, 1,    // I. versicolor
                                   95, 5.6, 2.7, 4.2, 1.3, 1,    // I. versicolor
                                   96, 5.7, 3.0, 4.2, 1.2, 1,    // I. versicolor
                                   97, 5.7, 2.9, 4.2, 1.3, 1,    // I. versicolor
                                   98, 6.2, 2.9, 4.3, 1.3, 1,    // I. versicolor
                                   99, 5.1, 2.5, 3.0, 1.1, 1,    // I. versicolor
                                  100, 5.7, 2.8, 4.1, 1.3, 1,    // I. versicolor
                                  101, 6.3, 3.3, 6.0, 2.5, 2,    // I. virginica
                                  102, 5.8, 2.7, 5.1, 1.9, 2,    // I. virginica
                                  103, 7.1, 3.0, 5.9, 2.1, 2,    // I. virginica
                                  104, 6.3, 2.9, 5.6, 1.8, 2,    // I. virginica
                                  105, 6.5, 3.0, 5.8, 2.2, 2,    // I. virginica
                                  106, 7.6, 3.0, 6.6, 2.1, 2,    // I. virginica
                                  107, 4.9, 2.5, 4.5, 1.7, 2,    // I. virginica
                                  108, 7.3, 2.9, 6.3, 1.8, 2,    // I. virginica
                                  109, 6.7, 2.5, 5.8, 1.8, 2,    // I. virginica
                                  110, 7.2, 3.6, 6.1, 2.5, 2,    // I. virginica
                                  111, 6.5, 3.2, 5.1, 2.0, 2,    // I. virginica
                                  112, 6.4, 2.7, 5.3, 1.9, 2,    // I. virginica
                                  113, 6.8, 3.0, 5.5, 2.1, 2,    // I. virginica
                                  114, 5.7, 2.5, 5.0, 2.0, 2,    // I. virginica
                                  115, 5.8, 2.8, 5.1, 2.4, 2,    // I. virginica
                                  116, 6.4, 3.2, 5.3, 2.3, 2,    // I. virginica
                                  117, 6.5, 3.0, 5.5, 1.8, 2,    // I. virginica
                                  118, 7.7, 3.8, 6.7, 2.2, 2,    // I. virginica
                                  119, 7.7, 2.6, 6.9, 2.3, 2,    // I. virginica
                                  120, 6.0, 2.2, 5.0, 1.5, 2,    // I. virginica
                                  121, 6.9, 3.2, 5.7, 2.3, 2,    // I. virginica
                                  122, 5.6, 2.8, 4.9, 2.0, 2,    // I. virginica
                                  123, 7.7, 2.8, 6.7, 2.0, 2,    // I. virginica
                                  124, 6.3, 2.7, 4.9, 1.8, 2,    // I. virginica
                                  125, 6.7, 3.3, 5.7, 2.1, 2,    // I. virginica
                                  126, 7.2, 3.2, 6.0, 1.8, 2,    // I. virginica
                                  127, 6.2, 2.8, 4.8, 1.8, 2,    // I. virginica
                                  128, 6.1, 3.0, 4.9, 1.8, 2,    // I. virginica
                                  129, 6.4, 2.8, 5.6, 2.1, 2,    // I. virginica
                                  130, 7.2, 3.0, 5.8, 1.6, 2,    // I. virginica
                                  131, 7.4, 2.8, 6.1, 1.9, 2,    // I. virginica
                                  132, 7.9, 3.8, 6.4, 2.0, 2,    // I. virginica
                                  133, 6.4, 2.8, 5.6, 2.2, 2,    // I. virginica
                                  134, 6.3, 2.8, 5.1, 1.5, 2,    // I. virginica
                                  135, 6.1, 2.6, 5.6, 1.4, 2,    // I. virginica
                                  136, 7.7, 3.0, 6.1, 2.3, 2,    // I. virginica
                                  137, 6.3, 3.4, 5.6, 2.4, 2,    // I. virginica
                                  138, 6.4, 3.1, 5.5, 1.8, 2,    // I. virginica
                                  139, 6.0, 3.0, 4.8, 1.8, 2,    // I. virginica
                                  140, 6.9, 3.1, 5.4, 2.1, 2,    // I. virginica
                                  141, 6.7, 3.1, 5.6, 2.4, 2,    // I. virginica
                                  142, 6.9, 3.1, 5.1, 2.3, 2,    // I. virginica
                                  143, 5.8, 2.7, 5.1, 1.9, 2,    // I. virginica
                                  144, 6.8, 3.2, 5.9, 2.3, 2,    // I. virginica
                                  145, 6.7, 3.3, 5.7, 2.5, 2,    // I. virginica
                                  146, 6.7, 3.0, 5.2, 2.3, 2,    // I. virginica
                                  147, 6.3, 2.5, 5.0, 1.9, 2,    // I. virginica
                                  148, 6.5, 3.0, 5.2, 2.0, 2,    // I. virginica
                                  149, 6.2, 3.4, 5.4, 2.3, 2,    // I. virginica
                                  150, 5.9, 3.0, 5.1, 1.8, 2)    // I. virginica

     // data for ternary classifiers
     val k = 3
     val x = xy.sliceCol (1, 5)         // columns 1, 2, 3, 4
     val y = xy.col (5).toInt           // column 5

     // data for binary classifiers
     val kk = 2
     val xx = x.slice (0, 100)       // first 100 rows
     val yy = y.slice (0, 100)          // first 100 elements

} // ExampleIris object

import ExampleIris._
import Round.roundMat

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleIrisTest` test several classifiers on the Iris dataset.
 *  > runMain scalation.analytics.classifier.ExampleIrisTest
 */
object ExampleIrisTest extends App
{
    val xr = roundMat (xx)
    var yp: VectoI = null

    banner ("NullModel")
    val nm  = new NullModel (yy)
    nm.train ()
    yp = nm.classify (xr)
    println (nm.fitLabel)
    println (nm.fit (yy, yp))
    println ((new ConfusionMat (yy, yp, 2)).confusion)

    banner ("NaiveBayes")
    val nb  = new NaiveBayes (xr, yy)
    nb.train ()
    yp = nb.classify (xr)
    println (nb.fitLabel)
    println (nb.fit (yy, yp))

    banner ("TANBayes")
    val tan = new TANBayes (xr, yy)
    tan.train ()
    yp = tan.classify (xr)
    println (tan.fitLabel)
    println (tan.fit (yy, yp))

    banner ("LogisticRegression")
    val lrg = new LogisticRegression (xx, yy)
    lrg.train ()
    yp = lrg.classify (xx)
    println (lrg.fitLabel)
    println (lrg.fit (yy, yp))

    banner ("LDA")
    val lda = new LDA (xx, yy)
    lda.train ()
    yp = lda.classify (xx)
    println (lda.fitLabel)
    println (lda.fit (yy, yp))

    banner ("KNN_Classifier")
    val knn = new KNN_Classifier (xx, yy)
    knn.train ()
    yp = knn.classify (xx)
    println (knn.fitLabel)
    println (knn.fit (yy, yp))

} // ExampleIrisTest

