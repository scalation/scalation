
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Robert Davis
 *  @version 1.0
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  This file contains classes for Hessenburg reductions, finding Eigenvalues
 *  and computing Eigenvectors.
 *  Need to add ability to work with SparseMatrixD
 */

package scalation.linalgebra

import math.{abs, signum, sqrt}
import util.control.Breaks.{breakable, break}

import scalation.linalgebra.Householder.house
import scalation.linalgebra.MatrixD.outer
import scalation.math.DoubleWithExp._
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This trait defines constants used by classes and objects the Eigen group.
 */
trait Eigen
{
    /** Debug flag
     */
    protected val DEBUG = true

    /** Error tolerance value
     */
    protected val EPSILON = 1E-9

} // Eigen trait

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to reduce, via similarity transformations, an n by n matrix
 *  'a' to Hessenburg form 'h', where all elements two below the main diagonal are
 *  zero (or close to zero).  Note, similarity transformations do not changes the
 *  eigenvalues.
 *  @param a  the matrix to reduce to Hessenburg form
 */
class Hessenburg (a: MatrixD)
      extends Eigen with Error
{
    /** The Hessenburg h matrix
     */
    private var h = new MatrixD (a)

    {
        val m = a.dim1
        val n = a.dim2
        if (m != n) flaw ("constructor", "must have m == n")

        for (j <- 0 until n) {                // for each column j
            val x  = h.col(j, j)              // jth column from jth position
            val u  = x + x.oneAt (0) * x.norm * (if (x(0) < 0.0) -1.0 else 1.0)
            val ident1 = new MatrixD (n - j, 1.0, 0.0)
            val ident2 = new MatrixD (j, 1.0, 0.0)
            val pp = ident1 - outer (u, u) * (2.0 / u.normSq)
            val p  = ident2 diag pp
            h = p.t * h * p
        } // for
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the Hessenburg h matrix.
     */
    def getH: MatrixD = h

} // Hessenburg class

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to find the eigenvalues of an n by n matrix 'a' using an
 *  iterative technique that applies similarity transformations to convert 'a' into
 *  an upper triangular matrix, so that the eigenvalues appear along the diagonal.
 *  To improve performance, the 'a' matrix is first reduced to Hessenburg form.
 *  During the iterative steps, a shifted QR decomposition is performed.
 *  Caveats: (i) it will not handle eigenvalues that are complex numbers,
 *           (ii) it uses a simple shifting strategy that may slow convergence.
 *  @param a  the matrix whose eigenvalues are sought 
 */
class Eigenvalue (a: MatrixD)
      extends Eigen with Error
{
    /** The vector of eigenvalues
     */
    private val e = new VectorD (a.dim1)

    /** Maximum number of iterations
     */
    private val ITERATIONS = 12        // increase --> more precision, but slower

    {
        val m = a.dim1
        val n = a.dim2
        if (m != n) flaw ("constructor", "must have m == n")
        var g = (new Hessenburg (a)).getH         // convert g matrix to Hessenburg form
        var converging = true                     // still converging, has not converged yet
        var lastE      = Double.PositiveInfinity  // save an eigenvalue from last iteration

        for (k <- 0 until ITERATIONS if converging) {  // major iterations
            converging = true
            for (l <- 0 until ITERATIONS) {            // minor iterations
                val s = g(n - 1, n - 1)                // the shift parameter
                val ident = new MatrixD (g.dim1, 1.0, 0.0)
                val qr = new QRDecomp (g - ident * s)
                g = qr.getR * qr.getQ + ident * s
            } // for

            for (i <- 0 until n) e(i) = g(i, i)       // extract eigenvalues from diagonal
            val e0 = e(0)                             // consider one eigenvalue
            if (abs ((lastE - e0) / e0) < EPSILON) {  // relative error
                converging = false                    // end major iterations
            } else {
                lastE = e0                            // save this eigenvalue
            } // if

            if (DEBUG) {
                println ("-------------------------------------------")
                println ("Eigenvalue: on iteration " + k + ": g = " + g)
                println ("Eigenvalue: on iteration " + k + ": e = " + e)
                if ( ! converging) println ("Eigenvalue: converged!")
            } // if
        } // for
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the eigenvalue e vector.
     */
    def getE: VectorD = e

} // Eigenvalue class

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class performs a Householder Tridiagonalization on a symmetric matrix.
 *  @see Algorithm 8.3.1 in Matrix Computations.
 *  @param a  the symmetric matrix to tridiagonalize
 *  @author Robert Davis
 */
class HouseholderT (a: MatrixD)
      extends Eigen with Error
{
    /** The Householder tridiagonal matrix
     */
    private val t = new SymTriMatrixD (a.dim1)

    {
        if (a.dim1 != a.dim2) flaw ("constructor", "must have m == n")
        if (! a.isSymmetric)  flaw ("constructor", "matrix a must be symmetric")
        val n = a.dim1-1                         // the last index
        for (k <- 0 to n - 2) {
            val ts  = a.col (k).slice (k+1, n+1)
            val v_b = house (ts)
            val v   = v_b._1; val b = v_b._2
            val p   = a.slice (k+1, n+1, k+1, n+1) * v * b 
            val w   = p - v * ((b / 2) * (p dot v))
            t(k, k) = a(k, k)
            t(k+1, k) = ts.norm;
            for (i <- k + 1 to n; j <- k + 1 to n) {
                a(i, j) = a(i, j) - (v(i - (k+1)) * w(j - (k+1)) +
                                     w(i - (k+1)) * v(j - (k+1)))
            } // for
        } // for
        t(n-1, n)   = a(n-1, n)
        t(n-1, n-1) = a(n-1, n-1)
        t(n, n)     = a(n, n)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the Householder Tridiagonal matrix.
     */
    def getT: SymTriMatrixD = t

} // HouseholderT class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class performs a symmetric QR step with a Wilkinson shift.
 *  @see Algorithm 8.3.2 in Matrix Computations.
 *  @see http://people.inf.ethz.ch/arbenz/ewp/Lnotes/chapter3.pdf (Algorithm 3.6)
 */
object SymmetricQRstep
       extends Eigen with Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a QR reduction step to matrix t.
     *  @param t  the unreduced symmetric tridiagonal matrix
     *  @param p  the row index
     *  @param q  the column index
     */
    def qRStep (t: SymTriMatrixD, p: Int, q: Int) = 
    {
        val n   = t.dg.dim - q - 1               // the last index
        val d   = (t.dg(n-1) - t.dg(n)) / 2.0     // Wilkinson shift
        val t2  = t.sd(n-1) ~^ 2.0
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create the values for a Givens 2-by-2 rotation matrix.  Given scalars
     *  a and b, efficiently compute c = cos(theta) and s = sin(theta) that can
     *  be used to form the rotation matrix.
     *  @see Algorithm 5.1.3 in Matrix Computation.
     *  @param a  the first scalar
     *  @param b  the second scalar
     */
    def givens (a: Double, b: Double): Tuple2 [Double, Double] =
    {
        val aa = abs (a)
        val ba = abs (b)
        var c  = 1.0          // cos (theta)
        var s  = 0.0          // sin (theta)
        var r  = 0.0
        if (ba > aa) {
            r = -a/b; s = 1.0 / sqrt (1.0 + r*r); c = s*r
        } else if (ba > EPSILON) {
            r = -b/a; c = 1.0 / sqrt (1.0 + r*r); s = c*r
        } // if
        (c, s)
    } // givens

} // SymmetricQRstep object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to find the eigenvalues of an n by n symmetric matrix 'a'
 *  using an iterative technique, the Symmetric QR Algorithm.
 *  @see Algorithm 8.3.3 in Matrix Computations.
 *  Caveats: (i) it will not handle eigenvalues that are complex numbers,
 *           (ii) it uses a simple shifting strategy that may slow convergence.
 *  @param a  the symmetric matrix whose eigenvalues are sought
 */
class EigenvalueSym (a: MatrixD)
      extends Eigen with Error
{
    /** The matrix containing a vector of eigenvalues
     */
    private var d: SymTriMatrixD = null

    {
        val m = a.dim1                          // number of rows
        if (m != a.dim2)     flaw ("constructor", "must have m == n")
        if (! a.isSymmetric) flaw ("constructor", "matrix a must be symmetric")
        var p = 0                               // the row index
        var q = 0                               // the column index
        d = (new HouseholderT (a)).getT          // make symmetric tridiagonal matrix

        while (q < m) {
            for (i <- 0 to m-2 if abs (d(i, i+1)) <=  EPSILON) d(i, i+1) = 0.0   // clean d
            q = 0; p = m-1
            while (p > 0 && d(p, p-1) == 0.0 && q < m) { q += 1; p -= 1 }
            while (p > 0 && d(p, p-1) != 0.0) p -= 1
            if (q < m) SymmetricQRstep.qRStep (d, p, q)
        } // while
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the eigenvalue e vector.
     */
    def getE: VectorD = d.dg     // the diagonal of the tridiagonal matrix

} // EigenvalueSym


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to find the eigenvectors of an n by n matrix 'a' by solving
 *  equations of the form (a - eI)v = 0 where e is the eigenvalue and v is the
 *  eigenvector.  Place the eigenvectors in a matrix column-wise.
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
    private val ident = new MatrixD (m, 1.0, 0.0)            // identity matrix
    private val e = if (_e == null) (new Eigenvalue (a)).getE else _e

    {
        // find eigenvectors using nullspace calculation
        for (i <- 0 until m) {               // compute eigenvector for i-th eigenvalue
            val a_Ie = (a - ident * e(i))    // .clean (EPSILON)
            println ("a_Ie = " + a_Ie)
            val c_a_Ie = a_Ie.clean (EPSILON)
            println ("c_a_Ie = " + c_a_Ie)
            val eVec = (new QRDecomp (c_a_Ie)).nullspaceV
            println ("+++ eigenvector for eigenvalue " + e(i) + " = " + eVec)
            val mat = a_Ie.slice (1, m)
            println ("mat = " + mat)
            val eVec2 = mat.nullspace
            println ("--- eigenvector for eigenvalue " + e(i) + " = " + eVec2)
            v.setCol (i, eVec)
        } // for
    } // primary constructor
/****
    {
        // find eigenvectors using inverse iteration (also improves eigenvalues)
        // @see http://home.iitk.ac.in/~dasgupta/MathBook/lmastertrans.pdf (p. 130)
        var y_k = new VectorD (m); y_k.set (1.0/m.toDouble)   // old estimate of eigenvector
        var y_l: VectorD = null                                 // new estimate of eigenvector

        for (i <- 0 until m) {               // compute eigenvector for i-th eigenvalue
            breakable { for (k <- 0 until ITERATIONS) {
                val a_Ie = a - ident * e(i)      // form matrix: [a - Ie]
                println ("a_Ie = " + a_Ie)
                val y = (new QRDecomp (a_Ie)).solve (y_k)         // solve [a - Ie]y = y_k
                y_l   = y / y.norm               // normalize
                e(i) += 1.0 / (y_k dot y)         // improve the eigenvalue
                if ((y_l - y_k).norm < EPSILON) break
                y_k = y_l                        // update the eigenvector
            }} // for
            println ("eigenvector for eigenvalue " + e(i) + " = " + y_l)
            v.setCol (i, y_l)
        } // for
    } // primary constructor
****/

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the eigenvector v matrix.
     */
    def getV: MatrixD = v 

} // Eigenvector class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the all the classes used in computing Eigenvalues
 *  and Eigenvectors for the non-symmetric/general case.
 */
object EigenTest extends App
{

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For matrix a, find Hessenburg matrix, eigenvalues and eigenvectors.
     */
    def test (a: MatrixD)
    {
        val e = (new Eigenvalue (a)).getE
        val v = (new Eigenvector (a, e)).getV

        println ("----------------------------------------------------------")
        println ("a = " + a)
        println ("e = " + e)
        println ("v = " + v)

        for (i <- 0 until v.dim1) {     // check that a * v_i = e_i * v_i
            println ("a * v_i - v_i * e_i = " + (a * v.col(i) - v.col(i) * e(i)))
        } // for
    } // test

    // @see http://www.mathworks.com/help/symbolic/eigenvalue-trajectories.html
    // should give e = (3.0, 2.0, 1.0)
    val b = new MatrixD ((3, 3), -149.0, -50.0, -154.0,            // 3-by-3 matrix
                                  537.0, 180.0,  546.0,
                                  -27.0,  -9.0,  -25.0)
    test (b)

    // @see http://www.math.hmc.edu/calculus/tutorials/eigenstuff/eigenstuff.pdf
    // should give e = (1.0, -3.0, -3.0)
    val c = new MatrixD ((3, 3), 5.0,  8.0,  16.0,                  // 3-by-3 matrix
                                 4.0,  1.0,   8.0,
                                -4.0, -4.0, -11.0)
    test (c)

} // EigenTest object

