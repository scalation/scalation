
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Dong Yu Yu, John Miller
 *  @version 1.4
 *  @date    Fri Jan  5 16:54:27 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.util.Random

import scalation.linalgebra.{MatrixD, VectorI, VectorD, VectoD}
import scalation.random.RandomVecI
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomForest` class uses randomness for building descision trees in classification.
 *  It randomly selects sub-samples with 'size = bR * sample-size' from the sample
 *  (with replacement) and uses the 'fS' number of sub-features to build the trees,
 *  and to classify by voting from all of the trees.
 *  @param x   the data matrix (instances by features)
 *  @param y   the response class labels of the instances
 *  @param nF  the number of trees
 *  @param bR  bagging ratio (the portion of samples used in building trees)
 *  @param fS  the number of features used in building trees
 *  @param k   the number of classes
 *  @param s   seed for randomness
 *  @param fn  feature names (array of string)
 *  @param cn  class names (array of string)
 */
class RandomForest (x: MatrixD, y: VectorI, nF: Int, bR: Double, fS: Int, k: Int, s: Int,
                    val fn: Array [String], val cn: Array [String])
      extends ClassifierReal (x, y, fn, k , cn) with Error
{
    private val DEBUG  = false
    private val xy     = x.:^+(y.toDouble)
    private val random = new Random (s)
    private val forest = Array.ofDim [DecisionTreeC45] (nF)

    if (nF <= 0) flaw ("constructor", "RF number of tree restrcited to be positive integer ")
    if (bR < 0 || bR > 1) flaw ("constructor", "RF bagging ratio restricted to 0 thru 1")
    if (fS < 0 || fS > x.dim2) flaw ("constructor", "RF feature size restricted to 0 thru number of features")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a 'subSample' (size = baggingRatio * orginal sample size) from the samples,
     *  returning the 'subSample'.
     */
    def createSubsample (): MatrixD =
    {
        val stream     = random.nextInt ().abs.%(1000)
        val sampleSize = (xy.dim1 * bR).toInt
        val rvv        = RandomVecI (min = 0, max = xy.dim1-1, dim = sampleSize, unique = false, stream = stream)
        val subSample  = new MatrixD (sampleSize, xy.dim2)
        val index      = rvv.igen
        for (i <- 0 until sampleSize) subSample.set (i, xy(index(i)))
        subSample
    } // createSubsample

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select 'subFeatures' for input of building Trees, return the 'subFeatures'
     *  and the selected features 'index'.
     *  @param subSample  the sub-sample to select from
     */
    def selectSubFeatures (subSample: MatrixD): (MatrixD, VectorI) =
    {
        val rvv         = RandomVecI (min = 0, max = subSample.dim2-1, dim = fS, unique = true)
        val index       = rvv.igen
        val subFeatures = subSample.selectCols (index.toArray)
        (subFeatures, index)
    } // selectSubFeatures

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the trees of Forest by first selecting the subSamples, then decide
     *  which features used in spliting, then build trees.
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def train (testStart:Int, testEnd:Int)
    {
        println ("=== Start Training ===")
        val isC = Array.fill (x.dim2)(true)
        val vc  = VectorI.fill (x.dim2)(2)
        for (i <- 0 until nF) {
            val temp    = createSubsample ()
            val feature = temp.selectCols (Range (0, temp.dim2-1).toArray)
            val selectTarget = temp.col (temp.dim2-1).toInt
            forest(i) = new DecisionTreeC45 (feature, selectTarget, fn, isCont = isC, k = k, cn = cn, vc = vc)
            forest(i).train (selectSubFeatures (x)._2)
            if (DEBUG) {
                println (s"===Tree$i===")
                println (forest(i).printTree())
            }//if
        } // for
        println ("=== Training Completed ===")
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the vector 'z' by voting within randomized trees, returning the class index,
     *  class name and the probability (not used in Random Forest, always -1.0)
     *  @param z  the vetcotr to be classified
     */
    def classify (z: VectoD): (Int, String, Double) =
    {
        if (DEBUG) println (s"predict for $z:")
        var result = new VectorI (k)
        for (i <- 0 until nF) {
            result(forest(i).classify (z)._1) += 1
            if (DEBUG) println (s"for tree$i, predict class = ${cn(forest(i).classify (z)._1)}")}
        (result.argmax (), cn(result.argmax ()), -1.0)
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the frequency and probability tables (not used here).
     */
    def reset() {}

} // RandomForest class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomForestTest` object is used to test the `RandomForest` class.
 *  It tests a simple case that does not require a file to be read.
 *  > runMain scalation.analytics.classifier.RandomForestTest
 */
object RandomForestTest extends App
{
    val x = new MatrixD ((11, 11),  8.1, 0.27, 0.41,  1.45, 0.033, 11,  63.0, 0.9908, 2.99, 0.56, 12.0, 
                                    8.6, 0.23, 0.40,  4.20, 0.035, 17, 109.0, 0.9947, 3.14, 0.53,  9.7, 
                                    7.9, 0.18, 0.37,  1.20, 0.040, 16,  75.0, 0.9920, 3.18, 0.63, 10.8, 
                                    6.6, 0.16, 0.40,  1.50, 0.044, 48, 143.0, 0.9912, 3.54, 0.52, 12.4, 
                                    8.3, 0.42, 0.62, 19.25, 0.040, 41, 172.0, 1.0002, 2.98, 0.67,  9.7, 
                                    6.6, 0.17, 0.38,  1.50, 0.032, 28, 112.0, 0.9914, 3.25, 0.55, 11.4, 
                                    6.3, 0.48, 0.04,  1.10, 0.046, 30,  99.0, 0.9928, 3.24, 0.36,  9.6, 
                                    6.2, 0.66, 0.48,  1.20, 0.029, 29,  75.0, 0.9892, 3.33, 0.39, 12.8, 
                                    7.4, 0.34, 0.42,  1.10, 0.033, 17, 171.0, 0.9917, 3.12, 0.53, 11.3, 
                                    6.5, 0.31, 0.14,  7.50, 0.044, 34, 133.0, 0.9955, 3.22, 0.50,  9.5, 
                                    6.2, 0.66, 0.48,  1.20, 0.029, 29,  75.0, 0.9892, 3.33, 0.39, 12.8)

    val y = VectorI (5, 5, 5, 7, 5, 7, 6, 8, 6, 5, 8)
    y -= 3
    val numClasses = 7
    val fn = Array ("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides",
                    "free sulfur dioxide", "total sulfur dioxide", "density", "pH", "sulphates", "alcohol")    // feature names

    val cn = Array("Level3", "Level4", "Level5", "Level6", "Level7", "Level8", "Level9")                       // class names

    val rF = new RandomForest (x, y, nF = 5, bR = 0.7, fS = 7, k = numClasses, s = 223, fn = fn, cn = cn)
    rF.train ()
    println (s"Accuracy = ${rF.test (0, x.dim1)}")

} // RandomForestTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomForestTest2` object is used to test the `RandomForest` class.
 *  It tests the Random Forest classifier using well-known WineQuality Dataset.
 *  > runMain scalation.analytics.classifier.RandomForestTest2
 */
object RandomForestTest2 extends App
{
    val file        = BASE_DIR + "winequality-white.csv"
    val numbClasses = 7

    println ("RandomForestTest2: Loading WineQuality Dataset")
    val data   = MatrixD (file)
    val target = data.col (data.dim2-1).-=(3)            // regularize the class labels

    val fn = (for (i <- 0 until data.dim2-1) yield s"feature$i").toArray
    val cn = (for (i <- 0 until numbClasses) yield s"class$i").toArray
    val rF = new RandomForest (data.selectCols (Range (0,data.dim2-1).toArray), target.toInt,
                               nF = 3, bR = 0.7, fS = 7, k = numbClasses, s = 223, fn = fn, cn = cn)
    rF.train ()
    println (s"Accuracy = ${rF.test (0, data.dim1)}")

} // RandomForestTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomForestTest3` object is used to test the `RandomForest` class.
 *  It tests the Random Forest classifier by specific numbers of trees.
 *  > runMain scalation.analytics.classifier.RandomForestTest3
 */
object RandomForestTest3 extends App
{
    val file        = BASE_DIR + "winequality-white.csv"
    val numbClasses = 7
    val maxTrees    = 3

    println ("RandomForestTest3: Loading WineQuality Dataset")
    val data   = MatrixD (file)
    val target = data.col (data.dim2-1).-=(3)            // regularize the class labels

    val fn = (for (i <- 0 until data.dim2-1) yield s"feature$i").toArray
    val cn = (for (i <- 0 until numbClasses) yield s"class$i").toArray

    for (numTrees <- 1 to maxTrees) {
        val rF = new RandomForest (data.selectCols (Range (0, data.dim2 - 1).toArray), target.toInt,
                                   nF = numTrees, bR = 0.7, fS = 7, k = numbClasses, s = 223, fn = fn, cn = cn)
        rF.train ()
        println (s"Number of Tree = $numTrees, Accuracy = ${rF.test (0,data.dim1)}")
    } // for

} // RandomForestTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomForestTest4` object is used to test the `RandomForest` class.
 *  It tests RF using unseen data.
 *  > runMain scalation.analytics.classifier.RandomForestTest4
 */
object RandomForestTest4 extends App
{
    /*Loading the Dataset*/
    val file        = BASE_DIR + "winequality-white.csv"
    val numbClasses = 7

    println ("RandomForestTest4: Loading WineQuality Dataset")
    val data   = MatrixD (file)
    val target = data.col (data.dim2-1).-=(3)              //regulized the class labels
    data.setCol (data.dim2-1, target)
 

    /*Divide samples into training and testing dataset */
    val trainSize  = (data.dim1 * 0.8).toInt
    val rvv        = RandomVecI ( min=0, max=data.dim1-1, dim =trainSize, unique=true,stream=223 )
    val subSample  = new MatrixD (trainSize, data.dim2)
    val elseSample = new MatrixD (data.dim1-trainSize, data.dim2)
    val index      = rvv.igen
    var trainCount = 0
    var elseCount  = 0

    for ( i <- data.range1) {
        if (index contains i) {
            subSample.set (trainCount, data(i))
            trainCount += 1
        } else {
            elseSample.set (elseCount, data(i))
            elseCount  += 1 
        } // if
    } // for

    val elseFeature  = elseSample.selectCols (Range (0, elseSample.dim2-1).toArray)
    val elseTarget   = elseSample.col (elseSample.dim2-1)

    /* Starting training Forest */
    val ran= 3
    val fn = (for (i <- 0 until data.dim2-1) yield s"feature$i").toArray
    val cn = (for (i <- 0 until numbClasses) yield s"class$i").toArray
    val rF = new RandomForest (subSample.selectCols(Range(0, data.dim2 - 1).toArray), subSample.col(subSample.dim2 - 1).toInt, 
                              nF = 5, bR = 0.64, fS = 7, k = numbClasses, s = ran, fn = fn, cn = cn)
    rF.train ()

    /* Print the accuracy for unseen data */
    var accurateCount = 0.0
    for (i <- 0 until elseFeature.dim1) {
        val d = rF.classify (elseFeature(i))._1
        if (rF.classify(elseFeature(i))._1 == elseTarget(i)) accurateCount += 1
    } 
    val accuracy = accurateCount / elseFeature.dim1
    println (s"Testing Accuracy = $accuracy")

} // RandomForestTest4 object

