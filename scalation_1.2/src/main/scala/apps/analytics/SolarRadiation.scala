
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Vishnu Gowda Harish, John Miller, 
 *  @version 1.2
 *  @date    Thu Dec 22 17:00:46 EST 2016
 *  @see     LICENSE (MIT style license file).
 */
package apps.analytics

import scalation.analytics.BASE_DIR
import scalation.linalgebra.{MatrixD, MatrixKind, RleMatrixD, RleVectorD, VectorD}
import scalation.relalgebra.Relation
import scalation.util.{timed}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SolarRadiation` object is a sample application that performs basic
 *  analytics on solar radiation and meteorological data. The data is 
 *  provided by NSRDB. Hourly Global Horizontal Radiation data of 40 sites 
 *  from the years 1961 to 1990 is considered. Operations are performed directly
 *  on compressed (RLE) vector and matrix.
 *  FIX: must download the following file.
 *  @see rredc.nrel.gov/solar/old_data/nsrdb/
 *  @see rredc.nrel.gov/solar/old_data/nsrdb/1961-1990/hourly/compressed/
 *  > run-main apps.analytics.SolarRadiation
 */
object SolarRadiation extends App
{
    val fName = BASE_DIR + "solar-radiation-40.csv"                                             
    
    val solarRel    = Relation (fName, "solarRel", -1, null, ",")                                  
    val solarMat    = solarRel.toMatriD (0 to 39)                                                  
    val solarRleMat = solarRel.toMatriD (0 to 39, MatrixKind.COMPRESSED).asInstanceOf [RleMatrixD] 
    val siteInfo    = solarRel.colName
    
    val mu  = VectorD (for (j <- solarMat.range2) yield solarMat.col (j).mean) 
    val cmu = RleVectorD (for (j <- solarRleMat.range2) yield solarRleMat.col (j).mean) 
    
    println ("Highest average computations")                                     
    println (s" Dense matrix: Site = ${siteInfo (mu.argmax ())},  average = ${mu.max ()}")
    println (s" Rle matrix:   Site = ${siteInfo (cmu.argmax ())}, average = ${cmu.max ()}")
    println ("Lowest average computations")
    println (s" Dense matrix: Site = ${siteInfo (mu.argmin ())},  average = ${mu.min ()}")
    println (s" Rle matrix:   Site = ${siteInfo (cmu.argmin ())}, average = ${cmu.min ()}")
    println (s" Size of columns in dense matrix:  ${solarMat.dim1}")
    println (s" Size of columns in Rle:           ${solarRleMat.csize}")
    
    val itr = 6                                                                                    
    val denseTimeVec  = new VectorD (itr)
    val rleTimeVec    = new RleVectorD (itr)
    
    for (i <- 0 until itr) {
          denseTimeVec (i) = timed { VectorD (for (j <- solarMat.range2) yield solarMat.col (j).mean) }._2
          rleTimeVec (i)   = timed { RleVectorD (for (j <- solarRleMat.range2) yield solarRleMat.col (j).mean) }._2
    } // for
    println (s"Average time taken by Dense: ${denseTimeVec.slice (1).mean} ms")
    println (s"Average time taken by Rle:   ${rleTimeVec.slice (1).mean} ms")
    
} // SolarRadiation object

