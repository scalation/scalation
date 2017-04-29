
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Vishnu Gowda Harish, John Miller, 
 *  @version 1.3
 *  @date    Thu Dec 22 17:00:46 EST 2016
 *  @see     LICENSE (MIT style license file).
 */
package apps.analytics

import scalation.analytics.BASE_DIR
import scalation.linalgebra.{MatrixD, MatrixKind, RleMatrixD, RleVectorD, VectorD}
//import scalation.linalgebra.MatrixKind
//import scalation.relalgebra.Relation
import scalation.util.{timed, time}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Census` object is a sample application that performs basic analytics on 
 *  census data.
 *  FIX: must download the following file.
 *  @see archive.ics.uci.edu/ml/machine-learning-databases/census1990-mld/
 *  > run-main apps.analytics.Census
 */
object Census extends App
{
    val fName = BASE_DIR + "USCensus1990_data.csv"
//  var censusRel     = Relation (fName, "census", "I" * 69, -1, ",")   	  // FIX : to check why it is slow
//  val censusMat     = censusRel.toMatriI (1 to 68)                                          
//  val censusRleMat  = censusRel.toMatriI (1 to 68, MatrixKind.COMPRESSED).asInstanceOf [RleMatrixI]  
    
    print ("Build dense matrix"); val censusMat = time {MatrixD (fName, 1)}
    print ("Build rle  matrix"); val censusRleMat = time {RleMatrixD (censusMat)}
    
    val jobDense = censusMat.col (4). filterPos (x => x == 1.0)
    val jobRle   = censusRleMat.col (4). filterPos (x => x == 1.0)
    val usBornDense = censusMat.col (5). filterPos (x => x == 0.0) 
    val usBornRle   = censusRleMat.col (5).filterPos (x => x == 0.0)   
    
    // Print the statistics
    println (s" Dense Matrix: Number of US Born = ${usBornDense.size}")
    println (s" Rle Matrix: Number of US Born   = ${usBornRle.size}")
    println (s" Dense Matrix: Number of people who have a job = ${jobDense.size}")
    println (s" Rle Matrix: Number of people who have a job = ${jobRle.size}")
    println (s" Dense Matrix: Born in the US and have a job = ${usBornDense.intersect(jobDense).size}")
    println (s" Rle Matrix: Born in the US and have a job = ${usBornRle.intersect(jobRle).size}")
   
    // Space information before and after RLE compression
    val compRatios =   (censusRleMat.csize.toDense.toDouble/censusMat.dim1.toDouble).recip
    val avgcompRatios = compRatios.mean
	  
    println (s" Size of columns in dense matrix:      ${censusMat.dim1}")
    println (s" Size of columns in Rle:               ${censusRleMat.csize}")
    println (s" Compression ratio column wise:        ${compRatios}")
    println (s" Average compression ratio of columns: ${avgcompRatios}")
	  
    //Compute the time
    val itr = 6                                                                                    
    val denseTimeVec  = new VectorD (itr)
    val rleTimeVec    = new RleVectorD (itr)
	
    for (i <- 0 until itr) {
        denseTimeVec (i) = timed { val usBornDense = censusMat.col (5). filterPos (x => x == 0.0)
                                   val jobDense = censusMat.col (4). filterPos (x => x == 1.0)
                                 }._2
        rleTimeVec (i)   = timed { val usBornRle = censusRleMat.col (5). filterPos (x => x == 0.0)
                                   val jobRle = censusRleMat.col (4). filterPos (x => x == 1.0)
                                 }._2
    } // for
    
    println (s"Dense: $denseTimeVec")
    println (s"Rle:   $rleTimeVec")
    println ("Average time taken by Dense: "+denseTimeVec.slice (1).mean+" ms")
    println ("Average time taken by Rle:   "+rleTimeVec.slice (1).mean+" ms")
    
} // Census object

