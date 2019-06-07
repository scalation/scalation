
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Jan  8 13:02:32 EST 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.language.implicitConversions

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `mem_mapped` package contains classes, traits and objects for out-of-core
 *  linear algebra, including vectors and matrices for real and complex numbers.
 */
package object mem_mapped
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from `VectorD` to `StatVec`, which supports more
     *  advanced statistical operations on vectors (e.g., covariance).
     *  Other vector types require to conversion to `VectorD` via 'toDouble'.
     *  Caveat: won't work for vectors of string numbers (`VectorS`) since
     *  there not a meaningful conversion for general strings.
     *  @param x  the vector to be enriched
     */
    implicit def vectorC2StatVec (x: VectorC) = new StatVec (x.toDouble)
    implicit def vectorD2StatVec (x: VectorD) = new StatVec (x)
//  implicit def vectorI2StatVec (x: VectorI) = new StatVecI (x)
    implicit def vectorI2StatVec (x: VectorI) = new StatVec (x.toDouble)
    implicit def vectorL2StatVec (x: VectorL) = new StatVec (x.toDouble)
    implicit def vectorQ2StatVec (x: VectorQ) = new StatVec (x.toDouble)
    implicit def vectorR2StatVec (x: VectorR) = new StatVec (x.toDouble)
//  implicit def vectorS2StatVec (x: VectorS) = new StatVec (x.toDouble)   // correlation unclear
//  implicit def vectorT2StatVec (x: VectorT) = new StatVec (x.toDouble)

} // mem_mapped object

