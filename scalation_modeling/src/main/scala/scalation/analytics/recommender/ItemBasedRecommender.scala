
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Khalid Jahangeer, John Miller
 *  @version 1.5
 *  @date    Fri Aug  10 20:26:34 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.recommender

import scala.math.{abs, round, sqrt}

import scalation.linalgebra.{MatrixD, MatrixI, VectorD, VectorI}
import scalation.math.double_exp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ItemBasedRecommender` class is used to perform predictions based on
 *  Item based Collaborative Filtering techniques (Cosine, Correlation, Adjusted Cosine)
 *  @param input   the original 4-column input data matrix
 *  @param m       the number of rows
 *  @param n       the number of columns
 *  @param stream  the random number stream to use
 */
class ItemBasedRecommender (input: MatrixI, m: Int, n: Int, stream: Int = 0)
      extends Recommender
{
    private val ratings  = makeRatings (input, m, n)                         // original ratings matrix
    private val training = new MatrixD (ratings.dim1, ratings.dim2)          // training dataset
    private val sim      = new MatrixD (n, n)                                // similarity matrix
    private val nsim     = 20                                                // number of nearest neighbors

    private var copy_training: MatrixD = null                                // copy of training dataset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a matrix with replacing all values with 0 beyond the interval specified
     *  corresponds to Phase 3 of the expermiments.
     *  @param limit  the interval start point
     *  @param input  the original data matrix
     */
    def zRatings (limit: Int, input: MatrixI)
    {
        for (i <- input.range1) {
            if(i <= limit) training (input(i, 0), input(i, 1)) = input(i, 2)
            else training(input(i, 0), input(i, 1)) = 0.0
        }// for
        copy_training = training.copy ()
    } // zRatings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the training matrix for the dataset for Phase 2.
     *  @param exl    the vector of index values which will be excluded in the train
     *  @param input  the original data matrix
     */
    def genTrainTest (exl: VectorI, input: MatrixI): MatrixD =
    {
        for (i <- input.range1){
            if (exl.indexOf(i) != -1) training (input(i, 0), input(i, 1)) = input(i, 2)
            else training(input (i, 0), input (i, 1)) = 0.0
        } // for
        copy_training = training.copy ()
        ratings - training
    } // genTrainTest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the training matrix for the dataset for Phase 1.
     *  @param train  the training data matrix
     */
    def genTrain2 (train: MatrixI)
    {
        for (i <- train.range1) training (train(i, 0), train(i, 1)) = train(i, 2)
        copy_training = training.copy ()
    } // genTrain2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute values for null/zero values in the 'training' matrix using
     *  Column Mean Imputation.
     */
    def impute
    {
        val colMean = training.meanNZ                                        // store the column mean (ignoring zeros)
        for (i <- training.range1; j <- training.range2) {
            if (training(i, j) =~ 0.0) training(i, j) = colMean(j)           // column mean imputation
        } // for
    } // impute

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize the 'training' matrix by subtracting the row mean from each
     *  non-zero element.  Row mean calculations exclude the zero elements.
     */
    def normalize
    {
        val rowMean = training.meanRNZ                                       // store the row mean (ignoring zeros)
        for (i <- training.range1; j <- training.range2) {
            if (training(i, j) !=~ 0.0) training(i, j) -= rowMean(i)         // row mean normalization
        } // for
    } // normalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a column denormalized version of 'this' matrix.
     *  return the denormalized value of the
     *  @param i  the user index
     */
    def denormalize (i: Int): Double =
    {
        if (copy_training(i).filter (_ !=~ 0.0).sum !=~ 0.0) copy_training(i).filter (_ !=~ 0.0).mean
        else 0.0
    } // denormalize

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the Cosine Similarity matrix on the basis of column - corated pairs.
     */
    def cosSim
    {
        for (i <- sim.range1; j <- sim.range2) {
            var sum1, sum2, sum3 = 0.0
            for (k <- training.range1){
                val t_ki = training(k, i)
                val t_kj = training(k, j)
                if (t_ki > 0.0 && t_kj > 0.0) {                        // only co-rated items
                    sum1 += t_ki * t_kj
                    sum2 += t_ki * t_ki
                    sum3 += t_kj * t_kj
                } //if
            } // for
            sim(i, j) = sum1 / (sqrt(sum2) * sqrt(sum3))
        } // for
    } // cosSim

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the Correlation Similarity matrix.
     */
    def corrSim
    {
        val col_mean = training.meanNZ                                 // column means for non-zero elements

        for (i <- sim.range1; j <- sim.range2) {
            var sum1, sum2, sum3 = 0.0
            for (k <- training.range1){
                val t_ki = training(k, i)
                val t_kj = training(k, j)
                if (t_ki > 0.0 && t_kj > 0.0) {                        // only co-rated items
                    sum1 += (t_ki - col_mean(i)) * (t_kj - col_mean(j))
                    sum2 += (t_ki - col_mean(i)) * (t_ki - col_mean(i))
                    sum3 += (t_kj - col_mean(j)) * (t_kj - col_mean(j))
                } // if
            } // for
            sim(i, j) = sum1 / (sqrt(sum2) * sqrt(sum3))
        } // for
    } // corrSim

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the Adjusted Cosine Similarity matrix.
     */
    def adjCosine
    {
        val row_mean = training.t.meanNZ                                // row means for non-zero elements

        val mean_centered = new MatrixD (training.dim1, training.dim2)  // reduce the training matrix by subtracting row means
        for (i <- training.range1; j <- training.range2){
            if (training(i, j) > 0.0) mean_centered(i, j) = training(i, j) - row_mean(i)
        } //for

        for (i <- sim.range1; j <- sim.range1) {
            var sum1, sum2, sum3 = 0.0
            for (k <- training.range1) {
                if (training(k, i) > 0.0 && training(k, j) > 0.0) {     // only co-rated items
                    val m_ki = training(k, i)
                    val m_kj = training(k, j)
                    sum1 += m_ki * m_kj
                    sum2 += m_ki * m_ki
                    sum3 += m_kj * m_kj
                } // if
            } // for
            sim(i, j) = sum1 / (sqrt(sum2) * sqrt(sum3))
        } // for
    } // adjusted Cosine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a rating based on the similarity between 'nsim' similar items.
     *  @param i  the user index
     *  @param j  the item index
     */
    def rate (i: Int, j: Int): Double =
    {
        var sum, dotprod = 0.0
        val simitem = new VectorD (sim.dim1)
        for (k <- simitem.range) {
            if (! sim(j, k).isNaN && training(i, k) != 0.0) simitem(k) = sim(j, k)
        } // for
        val topksim = topk (simitem, nsim)
        for (k <- topksim.range) {
            dotprod += training(i, topksim(k)) * simitem(topksim(k))
            sum     += abs (simitem(topksim(k)))
        } // for
        val temp = dotprod / sum
        if (temp.isNaN) training(i).mean else temp + denormalize (i)
    } // rate

} // ItemBasedRecommender class
  

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ItemBasedRecommender` companion object provides methods for reading
 *  input data from files.
 */
object ItemBasedRecommender
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read values from a data file into the data matrix and return it.
     *  The TAB separated file consists of four columns:
     *      row index, column index, rating value, time stamp.
     *  @param fname  the file name
     */
    def read (fname: String): MatrixI =
    {
        MatrixI.setSp ('\t')                                            // must specify the column separator
        val data = MatrixI (fname)                                      // input data matrix from data file
        data.setCol (0, data.col(0) - 1)                                // adjust row index down one
        data.setCol (1, data.col(1) - 1)                                // adjust column index down one
        data
    } // read

} // ItemBasedRecommender object

import scalation.⁄
import scalation.analytics.BASE_DIR
import scalation.plot.Plot
import scalation.util.time

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ItemBasedRecommenderTest` object is used to test the `ItemBasedRecommender`
 *  class using the MovieLens dataset.  Corresponds to Phase 1 of the Experiments in Thesis.
 *  > runMain scalation.analytics.recommender.ItemBasedRecommenderTest
 */
object ItemBasedRecommenderTest extends App
{
    val (m, n)     = (943, 1682)                                        // must specify the number of rows and columns
    val data_file  = BASE_DIR + "recommender" + ⁄ + "sorted_data.txt"   // input data file
    val train_file = BASE_DIR + "recommender" + ⁄ + "u2.base"           // training file: replace u(1-5).base
    val test_file  = BASE_DIR + "recommender" + ⁄ + "u2.test"           // testing file:  replace u(1-5).test

    val input  = ItemBasedRecommender.read (data_file)                  // input data matrix from data file
    val train  = ItemBasedRecommender.read (train_file)                 // training matrix
    val tester = ItemBasedRecommender.read (test_file)                  // testing matrix

    val recIB = new ItemBasedRecommender (input, m, n)                  // Item-Based Recommender
    recIB.genTrain2 (train)                                             // train using train matrix
    recIB.normalize                                                     // normalize the result

    for (l <- 0 until 1) {
        time {
            println ("Training Time")
            time {                                                      // training time
//              recIB.cosSim                                            // cosine similarity - UNCOMMENT one
                recIB.corrSim                                           // correlation similarity
//              recIB.adjCosine                                         // adjusted cosine similarity
            } //time
            println ("Prediction Time")                                 // testing time
            time { recIB.error_metrics (tester) }
        } // time
    } // for

} // ItemBasedRecommenderTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ItemBasedRecommenderTest2` object is used to test the `ItemBasedRecommender`
 *  class using the MovieLens dataset.  Corresponds to Phase 2 of the Experiments in Thesis.
 *  > runMain scalation.analytics.recommender.ItemBasedRecommenderTest2
 */
object ItemBasedRecommenderTest2 extends App
{
    val (m, n)    = (943, 1682)                                         // must specify the number of rows and columns
    val data_file = BASE_DIR + "recommender" + ⁄ + "sorted_data.txt"
    val kfold     = 5                                                   // value for k-fold cross-validation
    val diff      = new VectorD (kfold)                                 // MAE values
    val rdiff     = new VectorD (kfold)                                 // rounded MAE values
    val diff2     = new VectorD (kfold)                                 // RMSE value
    val rdiff2    = new VectorD (kfold)                                 // rounded RMSE values
    val hit       = new VectorD (kfold)                                 // number of successful predictions

    val input = ItemBasedRecommender.read (data_file)                   // input data matrix from data file

    val recIB    = new ItemBasedRecommender (input, m, n)               // Item Based Recommender
    val foldsize = input.dim1 / kfold

    for (l <- 0 until 1) {
        time {
            val indx = List.range (0, input.dim1)
            val rand_index = scala.util.Random.shuffle (indx)
            val index = new VectorI (input.dim1)
            val fold = VectorD.range (0, kfold)
            for (i <- input.range1) index(i) = rand_index(i)            // create a vector of a randomly permuted matrix

            for (i <- 0 until kfold) {
                val excl = new VectorI(foldsize)                        // vector to track the exclusion ratings
                println (s"--------------$i------------------")
                for (j <- 0 until excl.dim) excl(j) = index(i * foldsize + j)
                val tester = recIB.genTrainTest (excl, input)

                println("Training Time")
                time {                                                  // training time
                    recIB.normalize                                     // normalize the matrix
//                  recIB.cosSim                                        // cosine similarity - UNCOMMENT one
                    recIB.corrSim                                       // correlation similarity matrix
//                  recIB.adjCosine                                     // adjusted cosine similarity
                } // time
                println("Prediction time")
                time { recIB.crossValidate(tester) }                    // prediction time

                val stats = recIB.getStats
                diff(i)   = stats(0).ma
                rdiff(i)  = stats(1).ma
                diff2(i)  = stats(0).rms
                rdiff2(i) = stats(1).rms
                hit(i)    = stats(2).mean * 100
                for (j <- 0 until 3) stats(j).reset ()
            } // for

            println ("MAE          = " + diff.mean)
            println ("MAE rounded  = " + rdiff.mean)
            println ("RMSE         = " + diff2.mean)
            println ("RMSE rounded = " + rdiff2.mean)
            println ("HIT          = " + hit.mean)
        } // time
    } // for

} //ItemBasedRecommenderTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ItemBasedRecommenderTest3` object is used to test the `ItemBasedRecommender`
 *  class using the MovieLens dataset.  Corresponds to Phase 3 of the experiments i Thesis.
 *  > runMain scalation.analytics.recommender.ItemBasedRecommenderTest3
 */
object ItemBasedRecommenderTest3 extends App
{
    val (m, n)    = (943, 1682)                                         // must specify the number of rows and columns
    val data_file = BASE_DIR + "recommender" + ⁄ + "sorted_data.txt"
    val kfold     = 5                                                   // value for k-fold cross-validation
    val INTERVALS = 100                                                 // time intervals for observation of statistics
    val INT_SIZE  = 1000                                                // no of ratings in each interval
    val INT_START = 75                                                  // starting point of the interval
    val diff      = new VectorD (INTERVALS - INT_START)                 // MAE
    val rdiff     = new VectorD (INTERVALS - INT_START)                 // MAE rounded
    val diff2     = new VectorD (INTERVALS - INT_START)                 // RMSE
    val rdiff2    = new VectorD (INTERVALS - INT_START)                 // RMSE rounded
    val hit       = new VectorD (INTERVALS - INT_START)                 // number of successful predictions

    val input = ItemBasedRecommender.read (data_file)                   // input data matrix from data file

    val recIB = new ItemBasedRecommender (input, m, n)                  // Item Based Recommender
    val t_idx = VectorD.range (INT_START, INTERVALS)

    for (i <- INT_START until INTERVALS) {
        recIB.zRatings((i-1) * INT_SIZE, input)                         // get Zeroes Rating matrix
        println (i)
        recIB.normalize                                                 // normalize the matrix
//      recUB.cosSim                                                    // cosine similarity - UNCOMMENT one
        recIB.corrSim                                                   // correlation similarity matrix
//      recIB.adjCosine                                                 // adjusted cosine similarity
        recIB.test ((i-1) * INT_SIZE, i * INT_SIZE, input)

        val stats = recIB.getStats
        diff(i-INT_START)   = stats(0).ma
        rdiff(i-INT_START)  = stats(1).ma
        diff2(i-INT_START)  = stats(0).rms
        rdiff2(i-INT_START) = stats(1).rms
        hit(i-INT_START)    = stats(2).mean * 100
        for (j <- 0 until 3) stats(j).reset ()
    } // for

    println (diff.mean)
    println (rdiff.mean)
    println (diff2.mean)
    println (rdiff2.mean)
    println (hit.mean)
    println (diff)
    println (rdiff)
    println (hit)

    new Plot (t_idx, diff, rdiff, "DIFF")
    new Plot (t_idx, hit, null, "HIT")

} // ItemBasedRecommenderTest3 object

