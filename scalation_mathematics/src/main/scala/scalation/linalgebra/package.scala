
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Jul 15 16:50:28 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see http://www.scala-lang.org/node/724
 *  @see http://www.scala-lang.org/old/node/12014.html
 */

package scalation

import scala.language.implicitConversions

import scalation.math.FunctionS2S

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `linalgebra` package contains classes, traits and objects for
 *  linear algebra, including vectors and matrices for real and complex numbers.
 */
package object linalgebra
{ 
    /** The type definitions for functions involving base types from `linalgebra` (f: T => T), 
     *  where T is a vector or matrix.
     *  @see also `scalation.math`
     */
//  type FunctionS2S  = Double  => Double      // function of a scalar - Double - in `math`
    type FunctionV2S  = VectorD => Double      // function of a vector - VectorD
    type FunctionV_2S = VectoD  => Double      // function of a vector - VectoD  - base trait
    type FunctionV2V  = VectorD => VectorD     // vector-valued function of a vector - VectorD
    type FunctionV_2V = VectoD  => VectoD      // vector-valued function of a vector - VectoD  - base trait
    type FunctionM2M  = MatrixD => MatrixD     // matrix-valued function of a matrix - MatrixD
    type FunctionM_2M = MatriD  => MatriD      // matrix-valued function of a matrix - MatriD  - base trait

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Vectorize a scalar function (S2S) to create a vector function (V_2V).
     *  @param f  the scalar function to vectorize
     */
    def vectorize (f: FunctionS2S): FunctionV_2V = (x: VectoD) => x.map (f(_))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Matrixize a scalar function (S2S) to create a matrix function (M_2M).
     *  @param f  the scalar function to matrixize
     */
    def matrixize (f: FunctionV_2V): FunctionM_2M = (x: MatriD) => x.map (f(_))

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
    implicit def vectorT2StatVec (x: VectorT) = new StatVec (x.toDouble)

} // linalgebra package object

