
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Tue Sep 17 17:12:07 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://www.math.harvard.edu/archive/21b_fall_04/exhibits/2dmatrices/index.html
 */

package scalation.linalgebra

import scala.math.sqrt

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Eigen_2by2` object provides simple methods for computing eigenvalues and
 *  eigenvectors for 2-by-2 matrices.  
 */
object Eigen_2by2 extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the eigenvalues for the 2-by-2 matrix 'a' by finding the roots of
     *  the characteristic polynomial:
     *  <p>
     *      (a00 - l) * (a11 - l) - a01 * a10 = 0
     *  <p>
     *  where l is an eigenvalue (2 possible solutions).
     *  @param a  the 2-by-2 matrix whose eigenvalues are sought
     */
    def eigenvalues (a: MatrixD): VectorD =
    {
        if (a.dim1 != 2 && a.dim2 != 2) flaw ("eigenvalues", "a must be a 2-by-2 matrix")
        val t = a(0, 0) + a(1, 1)                          // trace
        val d = a(0, 0)*a(1, 1) - a(0, 1)*a(1, 0)          // difference
        val z = sqrt (t*t/4.0 - d)
        VectorD (t/2.0 + z, t/2.0 - z)
    } // eigenvalues

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the eigenvectors for the the 2-by-2 matrix 'a'.
     *  @param a  the 2-by-2 matrix whose eigenvectors are sought
     *  @param l  the eigenvalues
     */
    def eigenvectors (a: MatrixD, l: VectorD): MatrixD =
    {
        null    // FIX - to be implemented
    } // eigenvectors
    
} // Eigen_2by2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Eigen_2by2Test` is used to test the `Eigen_2by2` object.
 *  > runMain scalation.linalgebra.Eigen_2by2Test
 */
object Eigen_2by2Test extends App
{
    import Eigen_2by2.eigenvalues

    val a = new MatrixD ((2, 2), 2.0, -4.0,
                                -1.0, -1.0)
    println ("a = " + a)
    println ("eigenvalues (a) = " + eigenvalues (a))
    
    val b = new MatrixD ((2, 2), 26.0,  4.0,
                                  4.0, 20.0)
    println ("b = " + b)
    println ("eigenvalues (b) = " + eigenvalues (b))

    val c = new MatrixD ((2, 2), 1.0, 1.0,
                                 0.0, 2.0)
    println ("c = " + c)
    println ("eigenvalues (c) = " + eigenvalues (c))
    
} // Eigen_2by2Test object

