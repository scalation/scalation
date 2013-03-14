
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Robert Davis
 *  @version 1.0
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  This file contains classes for Hessenburg reductions, QR decompositions,
 *  finding Eigenvalues and computing Eigenvectors.  The first two are used
 *  in finding Eigenvalues, but are also useful in their own right. 
 */

package scalation.linalgebra

import math.{abs, pow, signum, sqrt}

import scalation.linalgebra.MatrixD.outer
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to reduce, via similarity transformations, an n by n matrix
 *  'a' to Hessenburg form 'h', where all elements two below the main diagonal are
 *  zero (or close to zero).  Note, similarity transformations do not changes the
 *  eigenvalues.
 *  @param a  the matrix to reduce to Hessenburg form
 */
class Hessenburg (a: MatrixD)
      extends Error
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
            val u  = x + x.oneAt (0) * x.norm * (if (x(0) < 0.) -1. else 1.)
            val ident1 = new MatrixD (n - j, 1., 0.)
            val ident2 = new MatrixD (j, 1., 0.)
            val pp = ident1 - outer (u, u) * (2. / u.normSq)
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
/** This class is used to decompose an m by n matrix 'a' into an orthogonal m by n
 *  matrix 'q' and an n by n right upper triangular matrix 'r' such that a = q * r.
 *  It uses Gram-Schmidt orthogonalization.
 *  Note, orthogonal means that * q.t * q = I.
 *  @see http://www.stat.wisc.edu/~larget/math496/qr.html
 *  @see http://en.wikipedia.org/wiki/Gram–Schmidt_process
 *       (stabilized Gram–Schmidt orthonormalization)
 *  @param a  the matrix to decompose into q and r
 */
class QRdecomposition (a: MatrixD)
      extends Error
{
    /** The orthogonal q matrix
     */
    private val q = new MatrixD (a)

    /** The right upper triangular r matrix
     */
    private val r = new MatrixD (a.dim2, a.dim2)

    {
        val m = a.dim1
        val n = a.dim2
        if (n > m) flaw ("constructor", "must have m >= n")

        for (j <- 0 until n) {                // for each column j
            val _norm = q.col(j).norm         // norm of the jth column
            r(j, j) = _norm

            if (_norm != 0.) {
                for (i <- 0 until m) q(i, j) /= _norm
                for (k <- j + 1 until n) {
                    r(j, k) = q.col(j) dot q.col(k)
                    for (i <- 0 until m) q(i, k) -=  q(i, j) * r(j, k)
                } // for
             } // if

         } // for
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the orthogonal q matrix.
     */
    def getQ: MatrixD = q

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the right upper triangular r matrix.
     */
    def getR: MatrixD = r

} // QRdecomposition class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to find the eigenvalues of an n by n matrix 'a' using an
 *  iterative technique that applies similarity transformations to convert 'a' into
 *  an upper triangular matrix, so that the eigenvalues appear along the diagonal.
 *  To improve performance, the 'a' matrix is first reduced to Hessenburg form.
 *  During the iterative steps, a shifted QR decomposition is performed.
 *  Caveats: (i) it will not handle eigenvalues that are complex numbers,
 *           (ii) it uses a simple shifting strategy that may slow convergence (FIX).
 *  @param a  the matrix whose eigenvalues are sought 
 */
class Eigenvalue (a: MatrixD)
      extends Error
{
    /** Flag indicating whether tracing is on to monitor convergence
     */
    private val trace = true

    /** The vector of eigenvalues
     */
    private val e = new VectorD (a.dim1)

    /** Error tolerance value
     */
    private val EPSILON = 1e-6

    {
        val m = a.dim1
        val n = a.dim2
        if (m != n) flaw ("constructor", "must have m == n")
        var g = (new Hessenburg (a)).getH         // convert g matrix to upper triangular
        var converging = true                     // still converging, has not converged yet
        val iterations = 6                        // increase --> more precision, but slower
        var lastE      = Double.PositiveInfinity  // save an eigenvalue from last iteration

        for (k <- 0 until iterations if converging) {  // major iterations
            converging = true
            for (l <- 0 until iterations) {            // minor iterations
                val s = g(n - 1, n - 1)                // the shift parameter
                val ident = new MatrixD (g.dim1, 1., 0.)
                val qr = new QRdecomposition (g - ident * s)
                g = qr.getR * qr.getQ + ident * s
            } // for

            for (i <- 0 until n) e(i) = g(i, i)       // extract eigenvalues from diagonal
            val e0 = e(0)                             // consider one eigenvalue
            if (abs ((lastE - e0) / e0) < EPSILON) {  // relative error
                converging = false                    // end major iterations
            } else {
                lastE = e0                            // save this eigenvalue
            } // if

            if (trace) {
                println ("-------------------------------------------")
                println ("Eigenvalue: on iteration " + k + " g = " + g)
                println ("Eigenvalue: on iteration " + k + " e = " + e)
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
//class Householder (a: SparseMatrixD)
class Householder (a: MatrixD)
      extends Error
{
    /** The Householder tridiagonal matrix
     */
    private val t = new SymTriMatrixD (a.dim1)

    {
        if (a.dim1 != a.dim2) flaw ("constructor", "must have m == n")
        if (! a.isSymmetric)  flaw ("constructor", "matrix a must be symmetric")
        val n = a.dim1 - 1         // the last index
        for (k <- 0 to n - 2) {
            val ts = a.col (k).slice (k + 1, n + 1)
            val v_b = house (ts)
            val v   = v_b._1; val b = v_b._2
            val p   = a.slice (k + 1, n + 1, k + 1, n + 1) * v * b 
            val w   = p - v * ((b / 2) * (p.dot(v)))
            t(k, k) = a(k, k)
            t(k + 1, k) = ts.norm;
            for (i <- k + 1 to n; j <- k + 1 to n) {
                a(i, j) = a(i, j) - (v(i - (k + 1)) * w(j - (k + 1)) +
                                     w(i - (k + 1)) * v(j - (k + 1)))
            } // for
        } // for
        t(n - 1, n)     = a(n-1,n)
        t(n - 1, n - 1) = a(n-1,n-1)
        t(n, n)         = a(n, n)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Householder vector v and its corresponding scalar b,
     *  where I - b * v * v.t is an orthogonal matrix.
     *  @see Algorithm 5.1.1 in Matrix Computations.
     *  @param x  the vector to create the Householder vector from
     */
    def house (x: VectorD): Tuple2 [VectorD, Double] =
    {
        var b  = 0.
        var v = new VectorD (x)
        v(0) = 1.
        val s= v.normSq - 1
        if (s != 0.) {
            val y = x(0)
            val m = sqrt (y * y + s)
            val z = if (y <= 0) y - m else -s / (y + m)
            v(0) = z
            b = 2. * z * z / (z * z + s)
            v /= z
        } // if
        Tuple2 (v, b)
    } // house

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the Householder Tridiagonal matrix.
     */
    def getT: SymTriMatrixD = t

} // Householder class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class performs a symmetric QR step with a Wilkinson shift.
 *  @see Algorithm 8.3.2 in Matrix Computations.
 *  @param t  the unreduced symmetric tridiagonal matrix
 */
class SymmetricQRstep (a: MatrixD)
      extends Error
{
    /** The symmetric tridiagonal matrix after one reduction step
     */
    private var t = new MatrixD (a)

    /** Error tolerance value
     */
    private val EPSILON = 1e-9

    {
        if (a.dim1 != a.dim2) flaw ("constructor", "must have m == n")
        if (! a.isSymmetric)  flaw ("constructor", "matrix a must be symmetric")
        val n  = t.dim1 - 1        // the last index
        val d  = (t(n-1, n-1) - t(n, n)) / 2.
        val t2 = pow (t(n, n-1), 2)
        val m  = t(n, n) - t2 / (d + signum (d) * sqrt (d * d + t2))
        var dv = new VectorD (t.dim1)
        var ev = new VectorD (t.dim1 - 1)
        for(i <- 0 until t.dim1)     dv(i) = t(i, i)
        for(i <- 0 until t.dim1 - 1) ev(i) = t(i, i + 1)
        var g = t(0, 0) - m
        var s = 1.0
        var c = 1.0
        var p = 0.0
        for (k <- 0 until n) {
            var f = s * ev(k)
            var b = c * ev(k)
            var r = sqrt(g * g + f * f)
            c = g / r
            s = f / r
            if (k != 0) ev(k - 1) = r
            g = dv(k) - p
            r = (dv(k + 1) - g) * s + 2 * c * b
            p = s * r
            dv(k) = g + p
            g = c * r - b
        } // for
        dv(n) = dv(n) - p
        ev(n - 1) = g
    } // primary constructor

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
        var c  = 1.       // cos(theta)
        var s  = 0.       // sin(theta)
        var r  = 0.
        if (ba > aa) {
            r = -a/b; s = 1. / sqrt (1. + r*r); c = s*r
        } else if (ba > EPSILON) {
            r = -b/a; c = 1. / sqrt (1. + r*r); s = c*r
        } // if
        Tuple2 (c, s)
    } // givens

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the tridiagonal matrix after reduction by Givens rotations.
     */
    def getT: MatrixD = t

} // SymmetricQRstep class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object form of the above class.
 */
object SymmetricQRstep
       extends Error
{
    private val EPSILON = 1e-9

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** 
     */
    def qRStep (t: SymTriMatrixD, p: Int, q: Int) = 
    {
        val n   = t.dg.dim - q - 1            // the last index
        val d   = (t(n - 1, n - 1) - t(n, n)) / 2.
        val t2  = pow (t(n, n - 1), 2)
        val m   = t(n, n) - t2 / (d + signum (d) * sqrt (d * d + t2))
        var g   = t(0, 0) - m
        var s   = 1.0
        var c   = 1.0
        var phi = 0.0
        for (k <- p until n) {
            var f = s * (t.sd(k))
            var b = c * (t.sd(k))
            var r = sqrt(g * g + f * f)
            c   = g / r
            s   = f / r
            if( k != 0 ) t.sd(k - 1) = r
            g   = t.dg(k) - phi
            r   = (t.dg(k + 1) - g) * s + 2 * c * b
            phi = s * r
            t.dg(k) = g + phi
            g = c * r - b
        } // for
        t.dg(n) = t.dg(n) - phi
        t.sd(n - 1) = g
    } // qRStep

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** 
     */
    def givens (a: Double, b: Double): Tuple2 [Double, Double] =
    {
        val aa = abs (a)
        val ba = abs (b)
        var c  = 1.       // cos(theta)
        var s  = 0.       // sin(theta)
        var r  = 0.
        if (ba > aa) {
            r = -a/b; s = 1. / sqrt (1. + r*r); c = s*r
        } else if (ba > EPSILON) {
            r = -b/a; c = 1. / sqrt (1. + r*r); s = c*r
        } // if
        Tuple2 (c, s)
    } // givens

} // SymmetricQRstep object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to find the eigenvalues of an n by n symmetric matrix 'a'
 *  using an iterative technique, the Symmetric QR Algorithm.
 *  @see Algorithm 8.3.3 in Matrix Computations.
 *  Caveats: (i) it will not handle eigenvalues that are complex numbers,
 *           (ii) it uses a simple shifting strategy that may slow convergence (FIX).
 *  @param a  the symmetric matrix whose eigenvalues are sought
 */
//class EigenvalueSym (a: SparseMatrixD)
class EigenvalueSym (a: MatrixD)
      extends Error
{
    /** Flag indicating whether tracing is on to monitor convergence
     */
    private val trace = true

    /** The vector of eigenvalues
     */
    private var d: SymTriMatrixD = null

    /** Error tolerance value
     */
    private val EPSILON = 1e-9

    {
        val m = a.dim1
        val n = a.dim2
        if (m != n)          flaw ("constructor", "must have m == n")
        if (! a.isSymmetric) flaw ("constructor", "matrix a must be symmetric")
        var q = 0
        var p = n
        d = (new Householder (a)).getT
        while (q != n - 1) {
            for (i <- 0 to n - 2 if abs (d(i, i + 1)) <=  EPSILON) {
                d(i, i + 1) = 0.
            } // for
            q = 0; p = m - 1
            while (p > 0 && d(p, p - 1) == 0. && q < n) {
                q = q + 1;
                p = p - 1;
            } // while
            while (p > 0 && d(p, p - 1) != 0.) {
                p = p - 1;
            } // while
            if (q != n - 1) SymmetricQRstep.qRStep(d,p,q)
        } // while
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a EigenvalueSym finder (non-sparse matrix case).
     *  @param m  the non-sparse matrix
     *
    def this (m: MatrixD)
    {
        this (new SparseMatrixD (m, 0.))
    } // constructor
     */

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
      extends Error
{
    /** The matrix of eigenvectors (each row holds an eigenvector)
     */
    private val v = new MatrixD (a.dim1, a.dim1)

    {
        val m = a.dim1
        val n = a.dim2
        if (n != m) flaw ("constructor", "must have m == n")

        val e = if (_e == null) (new Eigenvalue (a)).getE else _e
        for (i <- 0 until n) {               // compute eigenvector for i-th eigenvalue
            val ident = new MatrixD (a.dim1, 1., 0.)
            v.setCol (i, (a - ident * e(i)).slice(0,n-1).nullspace)
        } // for
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the eigenvector v matrix.
     */
    def getV: MatrixD = v 

} // Eigenvector class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the all the classes used in computing Eigenvalues
 *  and Eigenvectors for the non-symmetric/general case.
 *  @see http://en.wikipedia.org/wiki/QR_decomposition
 */
object EigenTest extends App
{
    val b = new MatrixD ((3, 3), -149., -50., -154.,            // 3-by-3 matrix
                                  537., 180.,  546.,
                                  -27.,  -9.,  -25.)
    val e = (new Eigenvalue (b)).getE
    println ("e = " + e)                  // should give 3, 2, 1
    val v = (new Eigenvector (b, e)).getV

    println ("v = " + v)
    for (i <- 0 until v.dim1) {   // check that b * v_i = e_i * v_i
        println ("b    * v(i) = " + (b    * v.row (i)))
        println ("v(i) * e(i) = " + (v.row (i) * e(i)))
    } // for

/***
    val b = new MatrixD ((3, 3), 2.,  1.,  3.,                  // 3-by-3 matrix
                                -1.,  0.,  7.,
                                 0., -1., -1.)
    val h = (new Hessenburg (b)).getH
    println ("h = " + h)

    val b = new MatrixD ((3, 3),  12.,  -51.,   4.,             // 3-by-3 matrix
                                   6.,  167., -68.,
                                  -4.,   24., -41.)
    val qr = new QRdecomposition (b)
    val q = qr.getQ
    val r = qr.getR
    println ("b = " + b)
    println ("q = " + q)
    println ("r = " + r)
    println ("q*r = " + q * r)
    println ("q.t*q = " + q.t * q)
***/

} // EigenTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the all the classes used in computing Eigenvalues
 *  and Eigenvectors for the symmetric/special case.
 */
object EigenTest2 extends App
{
    val t = new MatrixD ((4, 4), 1., 1., 0.,   0.,              // 4-by-4 matrix
                                 1., 2., 1.,   0.,
                                 0., 1., 3.,   0.01,
                                 0., 0., 0.01, 4.)

    val v = (new SymmetricQRstep (t)).getT
    println ("t = " + t)
    println ("v = " + v)

} // EigenTest2 object

