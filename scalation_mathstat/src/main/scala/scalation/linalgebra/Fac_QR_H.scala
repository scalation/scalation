
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhaochong Liu
 *  @version 1.4
 *  @date    Sat Jul 30 22:53:47 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scalation.math.double_exp
import scalation.math.ExtremeD.TOL

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR_H` class provides methods to factor an 'm-by-n' matrix 'a' into the
 *  product of two matrices:
 *  <p>
 *      'q' - an 'm-by-n' orthogonal matrix and
 *      'r' - an 'n-by-n' right upper triangular matrix
 *  <p>
 *  such that 'a = q * r'.   It uses uses Householder orthogonalization.
 *  @see 5.1 and 5.2 in Matrix Computations
 *  @see QRDecomposition.java in Jama
 *  @see www.stat.wisc.edu/~larget/math496/qr.html
 *-------------------------------------------------------------------------------
 *  This implementation improves upon `Fac_QR_H2` by working with the transpose the
 *  original matrix and reorders operations to facilitate parallelism (see par directory).
 *  Caveat: for m < n use `Fac_LQ`.
 *-------------------------------------------------------------------------------
 *  @param aa     the matrix to be factor into q and r
 *  @param needQ  flag indicating whether a full 'q' matrix is needed
 */
class Fac_QR_H [MatT <: MatriD] (aa: MatT, needQ: Boolean = true)
      extends Fac_QR (aa, needQ)
{
    protected val at = aa.t.asInstanceOf [MatrixD]             // transpose (for efficiency) of matrix aa

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform QR Factorization and store the result in a single matrix which contains
     *  Householder vectors of each column in the lower triangle of the 'aa' matrix. 
     *  This algorithm uses Householder orthogonalization.
     *  @see 5.1 and 5.2 in Matrix Computations
     *  @see QRDecomposition.java in Jama
     */
    def factor (): Fac_QR_H [MatT] =
    {
        if (factored) return this

        for (k <- at.range1) colHouse (k)                      // perform kth factoring step
        for (j <- 1 until p) {                                 // fill in rest of r matrix
            val at_j = at.v(j)
            for (i <- 0 until j) r(i, j) = at_j(i)
        } // for
        if (needQ) computeQ ()
        r.clean (TOL)                                          // comment out to avoid cleaning r matrix

        factored = true
        this
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the 'k'th Householder vector and perform the 'k'th Householder
     *  reflection.
     *  @param k  the index of the column performing the Householder reflection
     */
    protected def colHouse (k: Int)
    {
        val at_k  = at.v(k)                                    // kth row of transpose is kth column
        var _norm = at(k).slice (k).norm                       // norm of A(k:m, k) column vector
        if (_norm != 0.0) {
            if (at_k(k) < 0.0) _norm = -_norm                  // make kth Householder vector
            for (i <- k until m) at_k(i) /= _norm
            at_k(k) += 1.0
        } // if
        r(k, k) = -_norm                                       // set the diagonal of 'r' matrix

        for (j <- k + 1 until p) {                             // transform all the rest of 'aa' matrix
            val at_k = at.v(k)                                 // kth column of 'aa' matrix
            val at_j = at.v(j)                                 // jth column of 'aa' matrix
            var sum = 0.0
            for (i <- k until m) sum += at_k(i) * at_j(i)
            if (at_k(k) != 0.0) sum /= - at_k(k)
            for (i <- k until m) at_j(i) += sum * at_k(i)
        } // for
    } // colHouse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the full 'q' orthogonal matrix based on updated values in 'at'.
     */
    def computeQ ()
    {
        for (k <- p-1 to 0 by -1) {
            if (at(k, k) !=~ 0.0) {
                for (j <- k until n) {
                    val at_k = at.v(k)                            // kth column of 'a' is kth row of 'at'
                    var sum  = 0.0                                // update the elements in j column of 'q' matrix
                    for (i <- k until m) sum += q(i, j) * at_k(i)
                    sum /= - at_k(k) 
                    for (i <- k until m) q(i, j) += sum * at_k(i)
                } // for
            } // if
        } // for
    } // computeQ
 
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in 'aa*x = b' using the QR Factorization 'aa = q*r' via
     *  'r*x = q.t * b' without actually calculating the 'q' matrix.
     *  The overriding method is more efficient.
     *  @param b  the constant vector
     */
    override def solve (b: VectoD): VectoD = r.bsolve (VectorD (transformB (b().toArray)))
    def solve (b: VectorD): VectoD = r.bsolve (VectorD (transformB (b().array)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform 'b' into 'q.t * b', i.e., perform the right side of the equation
     *  'r*x = q.t * b' not using a full 'q' matrix.
     *  @param b  the constant vector
     */
    def transformB (b: Array [Double]): Array [Double] =  
    {  
        val qt_b = Array.ofDim [Double] (b.length)
        Array.copy (b, 0, qt_b, 0, b.length)                   // the result vector of 'q.t * b'

        for (j <- 0 until n) {                                 // calculate the result of 'q.t * b'
            val at_j = at.v(j)                                 // get the jth Householder vector
            var sum  = 0.0                                 
            for (i <- j until m) sum += qt_b(i) * at_j(i)   
            sum /= - at_j(j)
            for (i <- j until m) qt_b(i) += sum * at_j(i)
        } // for
        qt_b
    } // transformB
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using QR Factorization
     *  'q*r*x = 0'.  Gives a basis of dimension 'n - rank' for the nullspace
     *  @param rank  the rank of the matrix (number of linearly independent column vectors)
     */
    def nullspace (rank: Int): MatriD = 
    {
        val aat = aa.t                                         // transpose of aa
        val ns = if (aat.dim1 < aat.dim2) {
            val qq = (new Fac_LQ (aat)).factor2 ()             // using LQ
            qq.slice (rank, qq.dim1).t                         // last n - rank rows, transpose
        } else {
            val qq = (new Fac_QR_H (aat)).factor1 ()           // using QR
            qq.slice (0, aat.dim1, rank, aat.dim2)             // last n - rank columns
        } // if
        if (ns.dim2 > 0) ns else new MatrixD (aat.dim1, 0)
    } // nullspace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using QR Factorization
     *  'q*r*x = 0'.  Gives only one vector in the nullspace.
     *  @param x  a vector with the correct dimension
     */
    def nullspaceV (x: VectoD): VectoD =
    {
        x(n-1) = 1.0                                           // vector to solve for
        val b  = x.zero (n)                                    // new rhs as -r_i, n-1          
        for (i <- 0 until n) b(i) = -r(i, n-1)
        val rr = r.slice (0, n, 0, n-1)                        // drop last column
        for (k <- n-2 to 0 by -1) {                            // solve for x in rr*x = b
            x(k) = (b(k) - (rr(k) dot x)) / rr(k, k)
        } // for
        x
    } // nullspaceV

} // Fac_QR_H class

