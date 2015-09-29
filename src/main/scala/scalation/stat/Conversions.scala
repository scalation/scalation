
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Sep 28 11:18:16 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import language.implicitConversions

import scalation.linalgebra.mem_mapped.{VectorC, VectorD, VectorI, VectorL, VectorQ, VectorR, VectorS}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Conversions` object provide implicit conversion from memory mapped
 *  vectors to `StatVectors`.
 */
object Conversions
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from `VectorD` to `StatVector`, which supports more
     *  advanced statistical operations on vectors (e.g., covariance).
     *  Other vector types require to conversion to VectorD via 'toDouble'.
     *  Caveat: won't work for vectors of string numbers (`VectorS`).
     *  @param x  the vector to be enriched
     */
    implicit def vectorC2StatVector (x: VectorC) = new MM_StatVector (x.toDouble)
    implicit def vectorD2StatVector (x: VectorD) = new MM_StatVector (x)
    implicit def vectorI2StatVector (x: VectorI) = new MM_StatVector (x.toDouble)
    implicit def vectorL2StatVector (x: VectorL) = new MM_StatVector (x.toDouble)
    implicit def vectorQ2StatVector (x: VectorQ) = new MM_StatVector (x.toDouble)
    implicit def vectorR2StatVector (x: VectorR) = new MM_StatVector (x.toDouble)

} // Conversions object

