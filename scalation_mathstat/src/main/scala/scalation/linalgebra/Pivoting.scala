
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Fri Aug  5 12:57:28 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Pivoting` trait is used when row or column pivoting of a matrix is needed.
 */
trait Pivoting
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reorder the rows of matrix 'a' according to the pivot vector 'piv'.
     *  @param a        the matrix to reorder
     */
    def reorderRows (a: MatriD, piv: VectorI): MatriD =
    {
        val c = new MatrixD (a.dim1, a.dim2)
        for (i <- a.range1) c(i) = a(piv(i))
        c
    } // reorder

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reorder the columns of matrix 'a' according to the pivot vector 'piv'.
     *  @param a        the matrix to reorder
     */
    def reorderCols (a: MatriD, piv: VectorI): MatriD =
    {
        val c = new MatrixD (a.dim1, a.dim2)
        for (j <- a.range2) c.setCol(j, a.col(piv(j)))
        c
    } // reorder

} // Pivoting trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PivotingTest` is used to test the `Pivoting` trait.
 *  > runMain scalation.linalgebra.PivotingTest
 */
object PivotingTest extends App with Pivoting
{
    val a = new MatrixD ((3, 3), 1, 2, 3,
                                 4, 5, 6,
                                 7, 8, 9)

    val piv = VectorI (2, 1, 0)

    println ("a  = " + a)
    println ("a1 = " + reorderRows (a, piv))
    println ("a2 = " + reorderCols (a, piv))

} // PivotingTest object

