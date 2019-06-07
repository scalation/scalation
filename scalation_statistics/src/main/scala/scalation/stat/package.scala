
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Sep  6 21:07:43 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import scala.language.implicitConversions

import scalation.linalgebra.{VectorC, VectorD, VectorI, VectorL, VectorQ, VectorR, VectorT}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `stat` package contains classes, traits and objects for  basic statistical
 *  functions and analyses.  The package object itself defines an implicit
 *  conversion from `scalation.linalgebra.VectorD` to `StatVector`.
 */
package object stat
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from `VectorD` to `StatVector`, which supports more
     *  advanced statistical operations on vectors (e.g., covariance).
     *  Other vector types require to conversion to `VectorD` via 'toDouble'.
     *  Caveat: won't work for vectors of string numbers (`VectorS`) since
     *  there not a meaningful conversion for general strings.
     *  @param x  the vector to be enriched
     */
    implicit def vectorC2StatVector (x: VectorC) = new StatVector (x.toDouble)
    implicit def vectorD2StatVector (x: VectorD) = new StatVector (x)
    implicit def vectorI2StatVector (x: VectorI) = new StatVectorI (x)
    implicit def vectorL2StatVector (x: VectorL) = new StatVector (x.toDouble)
    implicit def vectorQ2StatVector (x: VectorQ) = new StatVector (x.toDouble)
    implicit def vectorR2StatVector (x: VectorR) = new StatVector (x.toDouble)
//  implicit def vectorS2StatVector (x: VectorS) = new StatVector (x.toDouble)   // correlation unclear
    implicit def vectorT2StatVector (x: VectorT) = new StatVector (x.toDouble)

} // stat package object

