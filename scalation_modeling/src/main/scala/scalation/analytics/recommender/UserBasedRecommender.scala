
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  khalid Jahangeer
  *  @version 1.6
  *  @date    Fri Aug  10 20:26:34 EDT 2017
  *  @see     LICENSE (MIT style license file).
  */

// U N D E R   D E V E L O P M E N T

package scalation.analytics.recommender

import scalation.analytics.BASE_DIR
import scalation.linalgebra.{VectorD, MatrixD, VectorI, MatrixI}
import scala.math.{abs, round, sqrt}
import scalation.random.{PermutedVecI, Randi}
import scalation.stat.{Statistic, vectorD2StatVector}
import scalation.math._
import scalation.⁄
import scalation.plot.Plot
import scalation.stat.StatVector._
import scalation.util._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**  The UserBasedRecommender class is used to perform predictions based on
  *  User based Collaborative Filtering techniques (Cosine, Correlation, Adjusted Cosine)
  *  @param input  original matrix
  *  @param m      number of rows
  *  @param n      number of columns
  */

class UserBasedRecommender (input: MatrixI, m: Int, n: Int)
    extends Recommender
{
    private val ratings         = makeRatings(input, m, n)                   // original ratings matrix
    private var training        = new MatrixD(ratings.dim1, ratings.dim2)    // training dataset
    private var copy_training   = new MatrixD(ratings.dim1, ratings.dim2)    // copy of training dataset
    private var sim             = new MatrixD(m, m)                          // user cosine similarity matrix
    private val nsim            = 20                                        // nearest neighbors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Returns a matrix with replacing all values with 0 beyond the interval specified
      *  @param limit : interval start point
      *  @param input : original data matrix
      */
    def zRatings (limit : Int, input: MatrixI)
    {
        for (i <- input.range1){
            if(i <= limit) training(input (i, 0), input (i, 1)) = input(i, 2)
            else training(input (i, 0), input (i, 1)) = 0.0
        }// for
        copy_training = training.copy()
    } //zRatings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Generates the training matrix for the dataset
      *  @param exl : vector of index values which will be excluded in the train
      *  @param input : original data matrix
      */
    def genTrainTest (exl: VectorI, input: MatrixI): MatrixD =
    {
        for (i <- input.range1){
            if(exl.indexOf(i) != -1) training(input (i, 0), input (i, 1)) = input(i, 2)
            else training(input (i, 0), input (i, 1)) = 0.0
        }// for
        copy_training = training.copy()
        ratings - training
    } //zRatings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Generates the training MatrixD for the input dataset in the form MatrixI
      *  @param train : training data matrix
      */
    def genTrain2 (train: MatrixI) = for (i <- train.range1) training(train(i, 0), train(i, 1)) = train(i, 2)


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a column normalized version of 'this' matrix.
      * for all values that are not 0 replace with self - row mean (mean calculation doesnot include 0s)
      */
    def normalize
    {
        var norm_a = new MatrixD (training.dim1, training.dim2)
        for (i <- training.range1; j <- training.range2) {
            var temp = 0.0
            if (training(i).filter(_ !=~ 0.0).sum !=~ 0.0) temp = training(i).filter(_ !=~ 0.0).mean
            if (training(i, j) !=~ 0.0) norm_a(i, j) = training(i, j) - temp
        } // for
        training = norm_a.copy()
    } // normalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a column denormalized version of 'this' matrix.
      * return the denormalized value for the row index i
      * @param i: Row index
      */
    def denormalize (i: Int): Double =
    {
        if(copy_training(i).filter(_ !=~ 0.0).sum !=~ 0.0) copy_training(i).filter(_ !=~ 0.0).mean
        else 0.0
    } //denormalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Column mean Imputation of null values in the input matrix
      */
    def impute (training: MatrixD): MatrixD =
    {
        val colmeans = colMeans                                                  // store the colmeans/min to imputed for all 0 value entries
        for (i <- training.range1; j <- training.range2) {
            if (training(i, j) =~ 0.0) training(i, j) = colmeans(j)              // column mean imputation
        } // for
        training
    }  //colImputation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of mean values of each column without including 0's
      */
    def colMeans: VectorD =
    {
        val minimum  = minNZ                                           // minimum value of input matrix
        var colmeans = new VectorD (training.dim2)                     // impute column mean for all 0 value entries in norm
        for (j <- training.range2) {
            if(training.col(j).filter(_ != 0.0).size == 0)
                colmeans(j) = minimum                                   // if a column has all 0's then assign minimum value
            else colmeans(j) = training.col(j).filter(_ !=~ 0.0).mean
        } // for
        colmeans
    }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum non-zero element of the entire matrix
      */
    def minNZ: Double =
    {
        var mn = training(0, 0)
        for (i <- training.range1; j <- training.range2 if training(i, j) > 0.0) {
            if (training(i, j) < mn) mn = training(i, j)
        } // for
        mn
    } // minNZ

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the Cosine Similarity matrix
      */
    def cosSim
    {
        for (i <- sim.range1; j <- sim.range2){
            var sum1 = 0.0
            var sum2 = 0.0
            var sum3 = 0.0
            for (x <- training.range2){
                if (training(i, x) > 0.0 && training(j, x) > 0.0) {      // only co-rated items
                    sum1 += training(i, x) * training(i, x)
                    sum2 += training(i, x) * training(i, x)
                    sum3 += training(j, x) * training(j, x)
                } //if
            } //for
            sim(i, j) = sum1 / (sqrt(sum2) * sqrt(sum3))
        } //for
    } //cosSim

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate the Correlation Similarity matrix
      */
    def corrSim
    {
        var row_mean = new VectorD(m)
        for (i <- row_mean.range) {
            val row = training(i).filter(_ !=~ 0.0)
            if (row.dim > 0) row_mean(i) = row.mean
            else row_mean(i) = 0.0
        } // for

        for (i <- sim.range1; j <- sim.range2){
            var sum1 = 0.0
            var sum2 = 0.0
            var sum3 = 0.0
            for (x <- training.range2){
                if (training(i, x) > 0.0 && training(j, x) > 0.0) {                          // only co-rated items
                    sum1 += (training(i, x) - row_mean(i)) * (training(j, x) - row_mean(j))
                    sum2 += (training(i, x) - row_mean(i)) * (training(i, x) - row_mean(i))
                    sum3 += (training(j, x) - row_mean(j)) * (training(j, x) - row_mean(j))
                } //if
            } //for
            sim(i, j) = sum1 / (sqrt(sum2) * sqrt(sum3))
        } //for
    } //cosSim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**  Generate a rating based on coorelation similarity between n similar users
      *  @param i  user
      *  @param j item
      */
    def rate (i: Int, j: Int) : Double =
    {
        var sum, dotprod = 0.0
        var simitem = new VectorD(sim.dim1)
        for (x <- simitem.range) {
            if(sim(i, x).isNaN || training(x, j) == 0.0) simitem(x) = 0.0
            else simitem(x) = sim(i, x)
        } //for
        val topksim = topk(simitem, nsim)                                   // top similar items
        for (x <- topksim.range){
            dotprod   += training(topksim(x), j) * simitem(topksim(x))
            sum       += abs(simitem(topksim(x)))
        } // for
        val temp = dotprod/sum
        if (temp.isNaN) training(i).mean else temp + denormalize(i)
    } // rate

}// UserBasedRecommender

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UserBasedRecommenderTest` object is used to test the `Recommender` Trait using the MovieLens dataset.
  * Corresponds to Phase 1 of the Experiments
  *  > run-main scalation.analytics.recommender.UserBasedRecommenderTest
  */

object UserBasedRecommenderTest extends App
{
    val data_file =  BASE_DIR + "recommender" + ⁄ +"sorted_data.txt"

    val (m, n)   = (943, 1682)
    MatrixI.setSp('\t')
    var input   =  MatrixI(data_file)
    input.setCol(0, input.col(0) -1)
    input.setCol(1, input.col(1) -1)
    val recUB    = new UserBasedRecommender(input, m, n)

    val train_file =  BASE_DIR + "recommender" + ⁄ +"u2.base"               // replace u(1-5).base
    val test_file  =  BASE_DIR + "recommender" + ⁄ +"u2.test"               // replace u(1-5).test

    var train   =  MatrixI(train_file)
    train.setCol(0, train.col(0) -1)
    train.setCol(1, train.col(1) -1)

    var tester   =  MatrixI(test_file)
    tester.setCol(0, tester.col(0) -1)
    tester.setCol(1, tester.col(1) -1)

    recUB.genTrain2(train)
    recUB.normalize                                                 // normalize the matrix
    for(i <- 0 until 1) {
        val t = time{
            println("Training Time")
            val t1 = time {
                //recUB.cosSim                                      // cosine similarity
                recUB.corrSim                                       // correlation similarity matrix
            } // training time
            val t2 = time {
                recUB.error_metrics(tester)
            } // testing time
        } //time
    } // for
} //UserBasedRecommenderTest

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UserBasedRecommenderTest2` object is used to test the `Recommender` Trait using the MovieLens dataset.
  * Corresponds to Phase 2 of the Experiments
  *  > run-main scalation.analytics.recommender.UserBasedRecommenderTest2
  */

object UserBasedRecommenderTest2 extends App
{
    val data_file   =  BASE_DIR + "recommender" + ⁄ +"sorted_data.txt"
    val kfold       = 5                                                              // value for k-fold cross-validation
    val diff        = new VectorD(kfold)                                             // mae values
    val rdiff       = new VectorD(kfold)                                             // rounded mae values
    val diff2       = new VectorD(kfold)                                             // rmse value
    val rdiff2      = new VectorD(kfold)                                             // rounded rmse values
    val hit         = new VectorD(kfold)                                             // number of successful predictions

    MatrixI.setSp('\t')
    var input   =  MatrixI(data_file)
    input.setCol(0, input.col(0) -1)
    input.setCol(1, input.col(1) -1)

    val foldsize    = input.dim1/kfold
    val (m, n)   = (943, 1682)
    val recUB    = new UserBasedRecommender(input, m, n)
    for(x <- 0 until 1) {
        val t = time {
            val indx = List.range(0, input.dim1)
            val rand_index = scala.util.Random.shuffle(indx)
            val index = new VectorI(input.dim1)
            val fold = VectorD.range(0, kfold)
            for (i <- 0 until input.dim1) index(i) = rand_index(i)              // create a vector of a randomly permuted matrix

            for (i <- 0 until kfold) {
                val excl = new VectorI(foldsize)                                // Vector to track the exclusion ratings
                println(s"--------------$i------------------")
                for (j <- 0 until excl.dim)
                    excl(j) = index(i * foldsize + j)
                val tester = recUB.genTrainTest(excl, input)
                println("Training Time")
                val t1 = time {                                                 // training time
                    recUB.normalize                                             // normalize the matrix
                    //recUB.cosSim                                              // cosine similarity
                    recUB.corrSim                                               // correlation similarity matrix
                }
                println("Prediction time")
                val t2 = time {                                                 // prediction time
                    recUB.crossValidate(tester)
                }
                val stats = recUB.getStats
                diff(i) = stats(0).ma
                rdiff(i) = stats(1).ma
                diff2(i) = stats(0).rms
                rdiff2(i) = stats(1).rms
                hit(i) = stats(2).mean * 100
                for (j <- 0 until 3) stats(j).reset()
            } // for

            println("MAE            = "+diff.mean)
            println("MAE rounded    = "+rdiff.mean)
            println("RMSE           = "+diff2.mean)
            println("RMSE rounded   = "+rdiff2.mean)
            println("HIT            = "+hit.mean)
        } // time
    } // for
} //UserBasedRecommenderTest2

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**  The `UserBasedRecommenderTest3` object is used to test the `Recommender` Trait using the MovieLens dataset.
  *  Corresponds to Phase 3 of the experiments
  *  > run-main scalation.analytics.recommender.UserBasedRecommenderTest3
  */

object UserBasedRecommenderTest3 extends App
{
    val data_file =  BASE_DIR +"recommender" + ⁄ +"sorted_data.txt"
    val kfold  = 5                                                              // value for k-fold cross-validation
    val INTERVALS = 100                                                         // time intervals for observation of statistics
    val INT_SIZE  = 1000                                                        // no of ratings in each interval
    val INT_START = 75                                                          // starting point of the interval
    val diff  = new VectorD(INTERVALS - INT_START)                              // MAE
    val rdiff = new VectorD(INTERVALS - INT_START)                              // MAE rounded
    val diff2  = new VectorD(INTERVALS - INT_START)                             // RMSE
    val rdiff2 = new VectorD(INTERVALS - INT_START)                             // RMSE rounded
    val hit   = new VectorD(INTERVALS - INT_START)                              // number of successful predictions

    MatrixI.setSp('\t')
    var input   =  MatrixI(data_file)
    input.setCol(0, input.col(0) -1)
    input.setCol(1, input.col(1) -1)

    val (m, n)   = (943, 1682)
    val recUB    = new UserBasedRecommender(input, m, n)
    val t_idx    = VectorD.range(INT_START, INTERVALS)

    for (i <- INT_START until INTERVALS) {
        recUB.zRatings((i-1) * INT_SIZE, input)                           // get Zeroes Rating matrix
        println(i)
        recUB.normalize                                                   // normalize the matrix
        //recUB.cosSim                                                    // cosine similarity
        recUB.corrSim                                                     // correlation similarity matrix
        recUB.test((i-1) * INT_SIZE, i * INT_SIZE, input)
        val stats = recUB.getStats
        diff(i-INT_START)   = stats(0).ma
        rdiff(i-INT_START)  = stats(1).ma
        diff2(i-INT_START)  = stats(0).rms
        rdiff2(i-INT_START) = stats(1).rms
        hit(i-INT_START)    = stats(2).mean * 100
        for (j <- 0 until 3) stats(j).reset()
    } // for

      println(diff.mean)
      println(rdiff.mean)
      println(diff2.mean)
      println(rdiff2.mean)
      println(hit.mean)
      println(diff)
      println(rdiff)
      println(hit)
      new Plot (t_idx, diff, rdiff, "DIFF")
      new Plot (t_idx, hit, null, "HIT")

} //UserBasedRecommenderTest3
