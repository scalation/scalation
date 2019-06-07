
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Vishnu Gowda Harish, John Miller
 *  @version 1.6
 *  @date    Tue Jun 21 14:37:45 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

//  U N D E R   D E V E L O P M E N T
//  generalize or move to app package

package scalation

import java.util.concurrent.RecursiveTask

//import scalation.linalgebra.par.RleVectorD
import scalation.linalgebra.TripletD
import scalation.util.ReArray

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SumTask` computes the sum in parallel via Recursive Tasks (i.e., for `VectorD`).
 *  @param arr  the input array whose sum is to be calculated
 *  @param i    the lower bound of the array for the recursive task
 *  @param j    the upper bound of the array for the recursive task
 */
class SumTask (arr: Array [Double], i: Int, j: Int)
      extends RecursiveTask [Double] 
{   
    val threshhold = 100000                  // threshold value to decide whether tasks should be split or not
  
    def compute: Double =                    // compute function of Recursive tasks
    {                
        var sum = 0.0
        val d = (j - i) + 1    
        if (d > threshhold) {                // split if sub task size is greater than threshold
            val m =  (i + j) / 2 
            val left  = new SumTask (arr, i, m) 
            val right = new SumTask (arr, m, j)
            left.fork ()
            val rSum = right.compute ()
            val lSum = left.join ()
            rSum + lSum
        } else {                              // no split
            sum = 0.0
            for (k <- i until j) sum += arr(k)
            sum
        } // if    
    } // compute 
    
} // SumTask class
  

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SumRleTask` computes the sum in parallel via Recursive Tasks (i.e., for `RleVectorD`)
 *  @param arr  the input array whose sum is to be calculated
 *  @param i    the lower bound of the array for the recursive task
 *  @param j    the upper bound of the array for the recursive task
 */
class SumRleTask (arr: ReArray [TripletD], i: Int, j: Int)
      extends RecursiveTask [Double] 
{
    val threshhold = 100000                  // threshold value to decide whether tasks should be split or not
    
    def compute: Double =                    // compute function of Recursive tasks
    {   
        var sum = 0.0
        val d = (j - i) + 1    
        if (d > threshhold) {                // split if sub task size is greater than threshold
            val m =  (i + j) / 2 
            val left = new SumRleTask (arr, i, m)   
            val right = new SumRleTask (arr, m, j)
            left.fork ()                      
            val rSum = right.compute ()       
            val lSum = left.join ()
            rSum + lSum
        } else {                            // no split
            sum = 0.0
            for (k <- i until j) sum += arr(k).value * arr(k).count
            sum
        } // if     
    } // compute
    
} // SumRleTask class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NormSqRleTask` computes the Euclidean norm (2-norm) in parallel via Recursive
 *  Tasks (i.e., for `RleVectorD`)
 *  @param arr  the input array whose normSq is to be calculated
 *  @param i    the lower bound of the array for the recursive task
 *  @param j    the upper bound of the array for the recursive task
 */
class NormSqRleTask (arr: ReArray [TripletD], i: Int, j: Int)
      extends RecursiveTask [Double] 
{
    val threshhold = 100000                // threshold value to decide whether tasks should be split or not
    
    def compute: Double =                  // compute function of Recursive tasks
    {   
        var sum = 0.0
        val d = (j - i) + 1    
        if (d > threshhold) {             // split if sub task size is greater than threshold
            val m =  (i + j) / 2 
            val left  = new NormSqRleTask (arr, i, m) 
            val right = new NormSqRleTask (arr, m, j)
            left.fork()
            val rSum = right.compute ()
            val lSum = left.join ()
            rSum + lSum
        } else {                          // no split
            sum = 0.0
            for (k <- i until j) sum += (arr(k).value * arr(k).value) * arr(k).count
            sum
        } // if     
    } // compute
    
} // NormSqRleTask class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RecTaskTest` object tests the operations provided in `RecTask`.
 *  > run-main scalation.RecTaskTest
 */
object RecTaskTest extends App
{
    import java.util.concurrent.ForkJoinPool
    import scalation.util.time
  
    val forkJoinPool = new java.util.concurrent.ForkJoinPool (4)  
    val input = (for (i <- 0 until 100000) yield i.toDouble).toArray
    var sum   = 0.0
  
    time { for ( i <- 0 until input.length ) sum += input(i) }                        // calculate sum sequentially

    var sumRec = time { forkJoinPool.invoke (new SumTask (input, 0, input.length)) }  // calculate sum via Recursive Task

    println ("Sum calculated sequentially = " + sum)
    println ("Sum calculated via Recursive Task = " + sumRec)
  
} // RecTaskTest object

