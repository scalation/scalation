
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Dong Yu Yu, John Miller
 *  @version 1.4
 *  @date    Fri Jan  5 16:54:27 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.util.Random

import scalation.linalgebra.{MatrixD,VectorI,VectorD,VectoD}
import scalation.random.RandomVecI
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomForest` class introduced randomness for building trees in classification.
 *  FIX - explain what is is in more detail
 *  @param x   the features part of samples
 *  @param y   the class labels of samples
 *  @param nF  the number of trees
 *  @param bR  bagging ratio (the portion of samples used in building trees)
 *  @param fS  the number of features used in building trees
 *  @param k   the number of classes in samples
 *  @param s   seed for randomness
 *  @param fn  feature names (array of string)
 *  @param cn  class names (array of string)
 */
class RandomForest (x: MatrixD, y: VectorI, nF: Int, bR: Double, fS: Int, k: Int, s: Int, var fn: Array [String],
                    var cn: Array [String])
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
        for (i <- 0 until nF ){
            val temp    = createSubsample ()
            val feature = temp.selectCols (Range (0, temp.dim2-1).toArray)
            val selectTarget = temp.col (temp.dim2-1).toInt
            forest(i) = new DecisionTreeC45 (feature, selectTarget, fn, isCont = isC, k = k, cn = cn, vc = vc)
            forest(i).train (selectSubFeatures (x)._2)
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
        var result = new VectorI (k)
        for (i <- 0 until nF) result(forest(i).classify (z)._1) += 1
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
    // FIX - add simple test case

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
 *  It tests RF using unseen data
 *  > runMain scalation.analytics.classifier.RandomForestTest4
 *  FIX - clean up this code
 */
object RandomForestTest4 extends App
{
    /*Loading the Dataset*/
    val a = new Array[String](3)
    println("Loading WineQuality Dataset")
    val file = BASE_DIR+"winequality-white.csv"
    val data=MatrixD(file)
    val target =data.col(data.dim2-1).-=(3) //regulized the class labels
    data.setCol(data.dim2-1, target)
    var i = 0

    /*Divide samples into training and testing dataset */
    val trainSize = (data.dim1 * 0.8).toInt
    val rvv = RandomVecI ( min=0, max=data.dim1-1, dim =trainSize, unique=true,stream=223 )
    val subSample = new MatrixD (trainSize, data.dim2)
    val elseSample = new MatrixD (data.dim1-trainSize, data.dim2)
    val index = rvv.igen
    var trainCount = 0; var elseCount = 0;
    for ( i <- 0 until data.dim1) {
        if (index.contains(i)) {
            subSample.set(trainCount, data.apply(i))
            trainCount += 1
        }//if
        else {
            elseSample.set(elseCount, data.apply(i))
            elseCount += 1
        }//else
    }//for
val elseFeature = elseSample.selectCols(Range(0, elseSample.dim2-1).toArray)
    val elseTarget = elseSample.col(elseSample.dim2-1)

    /* Starting training Forest */
    var ran=3
    var fn = new Array[String](data.dim2-1)
    val numbClasses = 7
    val cn = new Array[String](numbClasses)
    for (i <- 0 until data.dim2-1) fn(i)=s"feature$i"
    for (i <-0 until numbClasses) cn(i)=s"class$i"
    val rF = new RandomForest(subSample.selectCols(Range(0, data.dim2 - 1).toArray), subSample.col(subSample.dim2 - 1).toInt, nF = 5, bR = 0.64, fS = 7, k = numbClasses, s = ran, fn=fn, cn=cn)
    rF.train()
    var accurateCount = 0.0

    for (i <- 0 until elseFeature.dim1) {
        val d =rF.classify(elseFeature(i))._1
        if (rF.classify(elseFeature(i))._1 == elseTarget(i)) accurateCount += 1
    }
    val accuracy = accurateCount / elseFeature.dim1
    println(s"Testing Accuracy =$accuracy")

} // RandomForestTest4 object

