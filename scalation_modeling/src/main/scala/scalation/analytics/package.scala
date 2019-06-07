
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat Apr 30 13:32:23 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import scalation.linalgebra.{VectoD, MatriD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `analytics` package contains classes, traits and objects for analytics.
 */
package object analytics
{
    /** The relative path for base directory
     */
    val BASE_DIR = DATA_DIR + "analytics" + ⁄

    /** Shorthand for array of integers
     */
    type Ints = IndexedSeq [Int]

    /** Shorthand for array of strings
     */
    type Strings = Array [String]

    /** Pair of doubles
     */
    type PairD = (Double, Double)

    /** Pair of double vectors
     */
    type PairV = (VectoD, VectoD)

    /** Collection of vectors
     */
    type Vectors  = IndexedSeq [VectoD]

    /** Collection of matrices
     */
    type Matrices = IndexedSeq [MatriD]

    /** Collection of `NetParam`s
     */
    type NetParams = IndexedSeq [NetParam]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find values in a vector of infinite magnitude, returning all the index poistions.
     *  @param x  the vector in question
     */
    def findInfinity (x: VectoD): IndexedSeq [Int] =
    {
        for (i <- x.range if x(i).isInfinite) yield i
    } // findInfinity

} // analytics package object 

