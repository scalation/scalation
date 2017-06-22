
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat Aug 20 15:24:46 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scalation.linalgebra.{MatriI, MatrixI, VectoD, VectorD, VectoI, VectorI}
import scalation.relalgebra.Relation

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClfML` class implements an Integer-Based Naive Bayes Multi-Label Classifier,
 *  which is a commonly used such classifier for discrete input data.  The
 *  classifier is trained using a data matrix 'x' and a classification matrix 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier is naive, because it assumes feature independence and therefore
 *  simply multiplies the conditional probabilities.
 *  @see www.aia-i.com/ijai/sample/vol3/no2/173-188.pdf
 *  -----------------------------------------------------------------------------
 *  @param bayesBuilder  the function mapping an integer to a regular Bayes classifier
 *  @param nLabels       the number of labels/class variables
 *  @param nFeatures     the number of feature variables
 */
class BayesClfML (bayesBuilder: Int => BayesClassifier, nLabels: Int, nFeatures: Int)
      extends Classifier
{
    private val DEBUG = true                                         // debug flag
    private val bayes = Array.ofDim [BayesClassifier] (nLabels)      // array of regular Bayes classifiers
    bayes(0) =  bayesBuilder (0)                                     // create the first Bayes classifiers

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of data vectors in training/test-set (# rows).
     */
    def size = bayes(0).size

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a model.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def buildModel (testStart: Int, testEnd: Int): (Array [Boolean], DAG) =
    {
        (Array.fill (nFeatures)(true), new DAG (Array.ofDim [Int] (nFeatures, 0)))
    } // buildModel

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int)
    {
        for (l <- 0 until nLabels) {
//          bayes(l).buildModel ()
            bayes(l).train (testStart, testEnd)
            if (l < nLabels - 1) bayes(l+1) = bayesBuilder (l+1)
        } // for
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param itrain indices of the instances considered train data
     */
    override def train (itrain: IndexedSeq [Int])
    {
        for (l <- 0 until nLabels) {
            bayes(l) = bayesBuilder (l)
            bayes(l).train (itrain)
            if (l < nLabels - 1) bayes(l+1) = bayesBuilder (l+1)
        } // for
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability, returning
     *  the class, its name and its relative probability. 
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        var resMax = (0, "", 0.0)
        for (l <- 0 until nLabels) {
            val result = bayes(l).classify (z)
            if (result._3 > resMax._3) resMax = result
        } // for
        resMax                 // return the best class, its name and probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability, returning
     *  the class, its name and its relative probability. 
     *  @param z  the data vector to classify
     */
    def classify (z: VectoD): (Int, String, Double) = classify (z.toInt)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the population and probability vectors and
     *  hypermatrices to 0.
     */
    def reset ()
    {
        for (l <- 0 until nLabels) bayes(l).reset ()
    } // reset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param testStart  beginning of test region (inclusive)
     *  @param testEnd    end of test region (exclusive)
     */
    def test (testStart: Int, testEnd: Int): Double =
    {
        var correct = 0
// FIX: for (i <- testStart until testEnd if classify (x(i))._1 == y(i)) correct += 1
        correct / (testEnd - testStart).toDouble
    } // test

} // BayesClfML class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClfMLTest` object is used to test the `BayesClfML` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > run-main scalation.analytics.classifier.BayesClfMLTest
 */
object BayesClfMLTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // x3: Mpg:     High (1), Low (0)
    // features:                  x0 x1 x2 x3 y0 y1
    val xy = new MatrixI ((10, 6), 1, 0, 1, 1, 1, 0,         // data matrix
                                   1, 0, 1, 0, 0, 0,
                                   1, 0, 1, 1, 1, 1,
                                   0, 0, 1, 1, 0, 1,
                                   0, 0, 0, 1, 1, 1,
                                   0, 1, 0, 0, 0, 0,
                                   0, 1, 0, 0, 1, 0,
                                   0, 1, 1, 1, 0, 1,
                                   1, 1, 0, 0, 0, 0,
                                   1, 0, 0, 0, 1, 0)

    val fn = Array ("Color", "Type", "Origin", "Mpg")       // feature/variable names
    val cn = Array ("No", "Yes")                            // class names - for both classes

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")
  
    val x = xy.sliceCol (0, 4)                              // X-features
    val y = xy.sliceCol (4, xy.dim2)                        // C-classes

    def bayesBuilder (l: Int): BayesClassifier = new NaiveBayes (x, y.col(l), fn, 2, cn)

    val bayes = new BayesClfML (bayesBuilder, y.dim2, x.dim2)   // create the classifier            

    // train the classifier ---------------------------------------------------
    bayes.train ()

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1, 1)                           // existing data vector to classify
    val z2 = VectorI (1, 1, 1, 0)                           // new data vector to classify
    println ("classify (" + z1 + ") = " + bayes.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + bayes.classify (z2) + "\n")

//  bayes.crossValidateRand ()                              // cross validate the classifier

} // NaiveBayesMLTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesMLTest2` object is used to test the 'NaiveBayesML' class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > run-main scalation.analytics.classifier.NaiveBayesMLTest2
 */
object NaiveBayesMLTest2 extends App
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

    val fn = Array ("Fast", "Strong")                       // feature names
    val cn = Array ("No", "Yes")                            // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val x = xy.sliceCol (0, 2)                              // X-features
    val y = xy.sliceCol (2, xy.dim2)                        // C-classes

    def bayesBuilder (l: Int): BayesClassifier = new NaiveBayes (x, y.col(l), fn, 2, cn)

    val bayes = new BayesClfML (bayesBuilder, y.dim2, x.dim2)   // create the classifier

    // train the classifier ---------------------------------------------------
    bayes.train ()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                  // new data vector to classify
    println ("classify (" + z + ") = " + bayes.classify (z) + "\n")

    bayes.crossValidate ()                                  // cross validate the classifier

} // NaiveBayesMLTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesMLTest3` object is used to test the 'NaiveBayesML' class.
 *  > run-main scalation.analytics.classifier.NaiveBayesMLTest3
 */
object NaiveBayesMLTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("0", "1")                               // class names

    val x = xy.sliceCol (0, xy.dim2-1)                      // X-features
    val y = xy.sliceCol (xy.dim2-1, xy.dim2)                // C-classes

    def bayesBuilder (l: Int): BayesClassifier = new NaiveBayes (x, y.col(l), fn, 2, cn)

    val bayes = new BayesClfML (bayesBuilder, y.dim2, x.dim2)   // create the classifier

    // train the classifier ---------------------------------------------------
    bayes.train ()
    bayes.crossValidate ()

} // NaiveBayesMLTest3 object

