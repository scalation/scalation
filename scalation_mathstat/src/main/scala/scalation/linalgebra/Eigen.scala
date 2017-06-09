
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Robert Davis
 *  @version 1.3
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  This file contains classes for Hessenburg reductions, finding Eigenvalues
 *  and computing Eigenvectors.
 *  Need to add ability to work with `SparseMatrixD`
 */

package scalation.linalgebra

import scala.math.{abs, signum, sqrt}
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.Householder.house
import scalation.linalgebra.MatrixD.{eye, outer}
import scalation.math.double_exp
import scalation.math.ExtremeD.TOL
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Eigen` trait defines constants used by classes and objects in the group.
 */
trait Eigen
{
    /** Debug flag
     */
    protected val DEBUG = true

} // Eigen trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Hessenburg` class is used to reduce, via similarity transformations, an
 *  'n' by 'n' matrix 'a' to Hessenburg form 'h', where all elements two below the
 *  main diagonal are zero (or close to zero).  Note, similarity transformations
 *  do not changes the eigenvalues.
 *  @param a  the matrix to reduce to Hessenburg form
 */
class Hessenburg (a: MatrixD)
      extends Eigen with Error
{
    private val (m, n) = (a.dim1, a.dim2)               // size of matrix
    private var h = new MatrixD (a)                     // Hessenburg h matrix

    if (m != n) flaw ("constructor", "must have m == n")

    for (j <- 0 until n) {                              // for each column j
        val x  = h.col(j, j)                            // jth column from jth position
        val u  = x + x.oneAt (0) * x.norm * (if (x(0) < 0.0) -1.0 else 1.0)
        val pp = eye (n-j) - outer (u, u) * (2.0 / u.normSq)
        val p  = eye (j) diag pp
        h = p.t * h * p
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the Hessenburg h matrix.
     */
    def getH: MatrixD = h

} // Hessenburg class

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Eigenvalue` class is used to find the eigenvalues of an 'n' by 'n' matrix
 *  'a' using an iterative technique that applies similarity transformations to
 *  convert 'a' into an upper triangular matrix, so that the eigenvalues appear
 *  along the diagonal.  To improve performance, the 'a' matrix is first reduced
 *  to Hessenburg form.  During the iterative steps, a shifted 'QR' decomposition
 *  is performed.
 *  Caveats: (1) it will not handle eigenvalues that are complex numbers,
 *           (2) it uses a simple shifting strategy that may slow convergence.
 *  @param a  the matrix whose eigenvalues are sought 
 */
class Eigenvalue (a: MatrixD)
      extends Eigen with Error
{
    private val ITERATIONS = 12                         // max iterations: increase --> more precision, but slower
    private val (m, n) = (a.dim1, a.dim2)               // size of matrix
    private val e      = new VectorD (m)                // vector of eigenvalues

    if (m != n) flaw ("constructor", "must have m == n")

    var g = (new Hessenburg (a)).getH                   // convert g matrix to Hessenburg form
    var converging = true                               // still converging, has not converged yet
    var lastE      = Double.PositiveInfinity            // save an eigenvalue from last iteration

    for (k <- 0 until ITERATIONS if converging) {       // major iterations
        converging = true
        for (l <- 0 until ITERATIONS) {                 // minor iterations
            val s     = g(n - 1, n - 1)                 // the shift parameter
            val eye_g = eye (g.dim1)
            val (qq, rr) = (new Fac_QR_H (g - eye_g * s)).factor12 ()
            g = rr.asInstanceOf [MatrixD] * qq.asInstanceOf [MatrixD] + eye_g * s      // FIX
        } // for

        for (i <- 0 until n) e(i) = g(i, i)             // extract eigenvalues from diagonal
        val e0 = e(0)                                   // consider one eigenvalue
        if (abs ((lastE - e0) / e0) < TOL) {            // relative error
            converging = false                          // end major iterations
        } else {
            lastE = e0                                  // save this eigenvalue
        } // if

        if (DEBUG) {
            println ("-------------------------------------------")
            println ("Eigenvalue: on iteration " + k + ": g = " + g)
            println ("Eigenvalue: on iteration " + k + ": e = " + e)
            if (! converging) println ("Eigenvalue: converged!")
        } // if
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reorder the eigenvalue vector 'e' in non-increasing order.
      */
    def reorder () { e.sort2 () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the eigenvalue 'e' vector.
     *  @param order  whether to order the eigenvalues in non-increasing order
     */
    def getE (order: Boolean = true): VectorD = { if (order) reorder() ; e }

} // Eigenvalue class

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HouseholderT` class performs a Householder Tridiagonalization on a
 *  symmetric matrix.
 *  @see Algorithm 8.3.1 in Matrix Computations.
 *  @param a  the symmetric matrix to tridiagonalize
 */
class HouseholderT (a: MatrixD)
      extends Eigen with Error
{
    /** The Householder tridiagonal matrix
     */
    private val t = new SymTriMatrixD (a.dim1)

    if (a.dim1 != a.dim2) flaw ("constructor", "must have m == n")
    if (! a.isSymmetric)  flaw ("constructor", "matrix a must be symmetric")

    val n = a.dim1 - 1                                  // the last index
    for (k <- 0 to n - 2) {
        val ts    = a.col(k).slice (k+1, n+1)
        val v_b   = house (ts)
        val v     = v_b._1; val b = v_b._2
        val p     = a.slice (k+1, n+1, k+1, n+1) * v * b 
        val w     = p - v * ((b / 2) * (p dot v))
        t(k, k)   = a(k, k)
        t(k+1, k) = ts.norm
        for (i <- k + 1 to n; j <- k + 1 to n) {
            a(i, j) = a(i, j) - (v(i - (k+1)) * w(j - (k+1)) +
                                 w(i - (k+1)) * v(j - (k+1)))
        } // for
    } // for
    t(n-1, n)   = a(n-1, n)
    t(n-1, n-1) = a(n-1, n-1)
    t(n, n)     = a(n, n)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the Householder Tridiagonal matrix 't'.
     */
    def getT: SymTriMatrixD = t

} // HouseholderT class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SymmetricQRstep` object performs a symmetric 'QR' step with a Wilkinson shift.
 *  @see Algorithm 8.3.2 in Matrix Computations.
 *  @see http://people.inf.ethz.ch/arbenz/ewp/Lnotes/chapter3.pdf (Algorithm 3.6)
 */
object SymmetricQRstep
       extends Eigen with Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a 'QR' reduction step to matrix 't'.
     *  @param t  the unreduced symmetric tridiagonal matrix
     *  @param p  the row index
     *  @param q  the column index
     */
    def qRStep (t: SymTriMatrixD, p: Int, q: Int) = 
    {
        val n   = t.dg.dim - q - 1                      // the last index
        val d   = (t.dg(n-1) - t.dg(n)) / 2.0           // Wilkinson shift
        val t2  = t.sd(n-1) * t.sd(n-1)
        val d2  = t.dg(n) - t2 / (d + signum (d) * sqrt (d * d + t2))
        var g   = t.dg(0) - d2 
        var s   = 1.0
        var c   = 1.0
        var phi = 0.0

        for (k <- p until n) {
            var f   = s * (t.sd(k))
            var b   = c * (t.sd(k))
            var r   = sqrt (g * g + f * f)
            c       = g / r
            s       = f / r
            if (k != 0) t.sd(k-1) = r
            g       = t.dg(k) - phi
            r       = (t.dg(k+1) - g) * s + 2.0 * c * b
            phi     = s * r
            t.dg(k) = g + phi
            g       = c * r - b
        } // for

        t.dg(n)   = t.dg(n) - phi
        t.sd(n-1) = g
    } // qRStep

} // SymmetricQRstep object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EigenvalueSym` class is used to find the eigenvalues of an 'n' by 'n'
 *  symmetric matrix 'a' using an iterative technique, the Symmetric 'QR' Algorithm.
 *  @see Algorithm 8.3.3 in Matrix Computations.
 *  Caveats: (1) it will not handle eigenvalues that are complex numbers,
 *           (2) it uses a simple shifting strategy that may slow convergence.
 *  @param a  the symmetric matrix whose eigenvalues are sought
 */
class EigenvalueSym (a: MatrixD)
      extends Eigen with Error
{
    /** The matrix containing a vector of eigenvalues
     */
    private var d: SymTriMatrixD = null

    val m = a.dim1                                      // number of rows

    if (m != a.dim2)     flaw ("constructor", "must have m == n")
    if (! a.isSymmetric) flaw ("constructor", "matrix a must be symmetric")

    var p = 0                                           // the row index
    var q = 0                                           // the column index
    d = (new HouseholderT (a)).getT                     // make symmetric tridiagonal matrix

    while (q < m) {
        for (i <- 0 to m-2 if abs (d(i, i+1)) <= TOL) d(i, i+1) = 0.0   // clean d
        q = 0; p = m-1
        while (p > 0 && d(p, p-1) =~ 0.0 && q < m) { q += 1; p -= 1 }
        while (p > 0 && ! (d(p, p-1) =~ 0.0)) p -= 1
        if (q < m) SymmetricQRstep.qRStep (d, p, q)
    } // while

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the eigenvalue 'e' vector.
     */
    def getE: VectorD = d.dg           // the diagonal of the tridiagonal matrix

} // EigenvalueSym


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Eigenvector` class is used to find the eigenvectors of an 'n' by 'n' matrix
 *  'a' by solving equations of the form
 *  <p>
 *      (a - eI)v = 0
 *  <p>
 *  where 'e' is the eigenvalue and 'v' is the eigenvector.  Place the eigenvectors
 *  in a matrix column-wise.
 *  @param a   the matrix whose eigenvectors are sought 
 *  @param _e  the vector of eigenvalues of matrix a
 */
class Eigenvector (a: MatrixD, _e: VectorD = null)
      extends Eigen with Error
{
    private val ITERATIONS = 12                            // max iterations
    private val m = a.dim1                                 // number of rows
    if (a.dim2 != m) flaw ("constructor", "must have m == n")
    private val v = new MatrixD (m, m)                     // eigenvectors matrix (each row)
    private val ident = eye (m)                            // identity matrix
    private val e = if (_e == null) (new Eigenvalue (a)).getE () else _e

    // find eigenvectors using nullspace calculation
    for (i <- 0 until m) {                                 // compute eigenvector for i-th eigenvalue
        val a_Ie   = (a - ident * e(i))                    // a - Ie
        val c_a_Ie = a_Ie.clean (TOL)

        if (DEBUG) println (s"a_Ie = $a_Ie \nc_a_Ie = $c_a_Ie")

        val qr = new Fac_QR_H (c_a_Ie)
        qr.factor ()
        val eVec = qr.nullspaceV (e.zero (m))
        println ("+++ eigenvector for eigenvalue " + e(i) + " = " + eVec)
        val mat = a_Ie.slice (1, m)
        if (DEBUG) println ("mat = " + mat)
        val eVec2 = mat.nullspace
        println ("--- eigenvector for eigenvalue " + e(i) + " = " + eVec2)
//      v.setCol (i, eVec)
        v.setCol (i, eVec2)
    } // for

        // find eigenvectors using inverse iteration (also improves eigenvalues)
        // @see http://home.iitk.ac.in/~dasgupta/MathBook/lmastertrans.pdf (p. 130)
//      var y_k = new VectorD (m); y_k.set (1./m.toDouble)      // old estimate of eigenvector
//      var y_l: VectorD = null                                 // new estimate of eigenvector
//
//      for (i <- 0 until m) {                     // compute eigenvector for i-th eigenvalue
//          breakable { for (k <- 0 until ITERATIONS) {
//              val a_Ie = a - ident * e(i)        // form matrix: [a - Ie]
//              f (DEBUG) println ("a_Ie = " + a_Ie)
//              val qr = new Fac_QR_H (a_Ie)
//              qr.factor ()
//              val y = qr.solve (y_k)             // solve [a - Ie]y = y_k
//              y_l   = y / y.norm                 // normalize
//              e(i) += 1.0 / (y_k dot y)          // improve the eigenvalue
//              if ((y_l - y_k).norm < TOL) break
//              y_k = y_l                          // update the eigenvector
//          }} // for
//          println ("eigenvector for eigenvalue " + e(i) + " = " + y_l)
//          v.setCol (i, y_l)
//      } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the eigenvector 'v' matrix.
     */
    def getV: MatrixD = v 

} // Eigenvector class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EigenTest` object is used to test the all the classes used in computing
 *  Eigenvalues and Eigenvectors for the non-symmetric/general case.
 *  > run-main scalation.linalgebra.EigenTest
 */
object EigenTest extends App
{
    import scalation.util.banner

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For matrix a, find Hessenburg matrix, eigenvalues and eigenvectors.
     */
    def test (a: MatrixD, name: String)
    {
        banner (name)
        val e = (new Eigenvalue (a)).getE ()
        val v = (new Eigenvector (a, e)).getV

        println ("----------------------------------------------------------")
        println ("a = " + a)
        println ("e = " + e)
        println ("v = " + v)

        for (i <- 0 until v.dim1) {             // check that a * v_i = e_i * v_i
            println ("a * v_i - v_i * e_i = " + (a * v.col(i) - v.col(i) * e(i)))
        } // for
    } // test

    // @see http://www.mathworks.com/help/symbolic/eigenvalue-trajectories.html
    // should give e = (3., 2., 1.)
    val b = new MatrixD ((3, 3), -149.0, -50.0, -154.0,            // 3-by-3 matrix
                                  537.0, 180.0,  546.0,
                                  -27.0,  -9.0,  -25.0)
    test (b, "matrix b")

    // @see http://www.math.hmc.edu/calculus/tutorials/eigenstuff/eigenstuff.pdf
    // should give e = (1., -3., -3.)
    val c = new MatrixD ((3, 3), 5.0,  8.0,  16.0,                  // 3-by-3 matrix
                                 4.0,  1.0,   8.0,
                                -4.0, -4.0, -11.0)
    test (c, "matrix c")

} // EigenTest object

