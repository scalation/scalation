
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed May 28 16:06:12 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.math.pitt.edu/~sussmanm//2071Spring08/lab09/index.html
 *  @see www.netlib.org/lapack/lawnspdf/lawn03.pdf
 *  @see www.netlib.org/lapack/lawns/lawn11.ps
 *  @see fortranwiki.org/fortran/show/svd
 *
 *  Code translated from LAPACK Fortran code
 */

// U N D E R   D E V E L O P M E N T
// FIX Q and P incorrected permuted

package scalation.linalgebra

import math.{abs, max, min, sqrt}
import util.control.Breaks.{break, breakable}

import scalation.linalgebra.Rotation._
import scalation.math.{double_exp, sign}
import scalation.math.ExtremeD.{EPSILON, MIN_NORMAL, TOL}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD3` class is used to solve Singular Value Decomposition for bidiagonal matrices.
 *
 *  It computes the singular values and, optionally, the right and/or left singular vectors
 *  from the singular value decomposition 'SVD' of a real n-by-n (upper) bidiagonal matrix B
 *  using the implicit zero-shift 'QR' algorithm.  The 'SVD' of B has the form
 *
 *     B = Q * S * P.t
 *
 *  where S is the diagonal matrix of singular values, Q is an orthogonal matrix of
 *  left singular vectors, and P is an orthogonal matrix of right singular vectors.
 *  If left singular vectors are requested, this subroutine actually returns U*Q
 *  instead of Q, and, if right singular vectors are requested, this subroutine
 *  returns P.t * VT instead of P.T, for given real input matrices U and VT.  When
 *  U and VT are the orthogonal matrices that reduce a general matrix A to bidiagonal
 *  form:  A = U*B*VT, as computed by DGEBRD, then
 *
 *     A = (U*Q) * S * (P.t*VT)
 *
 *  is the 'SVD' of the general matrix A.  A positive tolerance 'TOL' gives relative accuracy;
 *  for absolute accuracy negate it.
 *
 *  @see "Computing Small Singular Values of Bidiagonal Matrices With Guaranteed High Relative Accuracy,"
 *  @see J. Demmel and W. Kahan, LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. 11:5, pp. 873-912, Sept 1990)
 *
 *  @see "Accurate singular values and differential qd algorithms," B. Parlett and V. Fernando,
 *  @see Technical Report CPAM-554, Mathematics Department, University of California at Berkeley, July 1992
 *
 *  @see fortranwiki.org/fortran/show/svd
 *  @see LAPACK SUBROUTINE DBDSQR (UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U, LDU, C, LDC, WORK, INFO)
 *
 *  @param a   the bidiagonal matrix A consisting of a diagonal and super-diagonal
 *  @param vt  the right orthogonal matrix from b = bidiagonalize (a)
 *  @param u   the left orthogonal matrix from b = bidiagonalize (a)
 */
class SVD3 (b: BidMatrixD, vt: MatrixD = new MatrixD (0, 0),
                          u:  MatrixD = new MatrixD (0, 0))
      extends SVDecomp
{
    private val DEBUG   = true                         // debug flag
    private val ONLY_S  = false                        // only interested in singular values, not vectors
    private val DO_SORT = false                        // do sort the singular values 
    private val MAXITR  = 6                            // interation factor
    private val n       = b.dim1                       // the size (rows and columns) of the bidiagonal matrix B
    private val ncvt    = vt.dim2                      // the number of columns in matrix VT
    private val nru     = u.dim1                       // the number of rows in matrix U
    private val work    = Array.ofDim [Double] (4*n)   // workspace -- FIX: replace
    private val NM1     = n - 1                        // one less than n
    private val NM12    = NM1 + NM1                    // 2 * NM1
    private val NM13    = NM12 + NM1                   // 3 * NM1
    private val maxit   = MAXITR * n * n               // maximum number of iterations allowed
    private val d       = b.dg                         // the main diagonal
    private val e       = b.sd                         // the super-diagonal (one above main)
 
    private var notflat = true                         // whether matrix B is yet to be deflated
    private var oldll   = -1                           // old saved lower index
    private var oldm    = -1                           // old saved upper index
    private var m       = n-1                          // m points to last element of unconverged part of matrix
    private var idir    = 0                            // the bulge chasing direction

    private var smax    = d.mag max e.mag              // estimate for largest singular value
    private var smin    = 0.0                          // estimate for smallest singular value
    private var smin_l  = 0.0                          // lower bound on smallest singular value 

    private var cs      = 1.0                          // cosine in rotation matrix
    private var sn      = 0.0                          // sine in rotation matrix
    private var r       = 1.0                          // remaining nonzero value
    private var oldcs   = 1.0                          // old saved cosine in rotation matrix
    private var oldsn   = 0.0                          // old saved sine in rotation matrix

    private var sigmn   = 0.0                          // minimum singular value
    private var sigmx   = 0.0                          // maximum singular value
    private var sinr    = 0.0                          // sine right
    private var cosr    = 1.0                          // cosine right
    private var sinl    = 0.0                          // sine left
    private var cosl    = 1.0                          // cosine right

    private val thresh  = calcThreshold ()             // threshold for setting values to zero

    def max3 (x: Double, y: Double, z: Double) = (x max y) max z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the desired threshold for setting elements to zero.
     */
    def calcThreshold (): Double =
    {
         if (TOL >= 0.0) {                                       // relative accuracy desired
             var smin_oa = abs (d(0))
             if (smin_oa > 0.0) {
                 var mu = smin_oa
                 breakable { for (i <- 1 until n) {
                     mu = abs (d(i)) * (mu / (mu + abs(e(i-1))))
                     smin_oa = min (smin_oa, mu)
                     if (smin_oa =~ 0.0) break
                 }} // breakable for
             } // if
             smin_oa = smin_oa / sqrt (n.toDouble)
             return max (TOL * smin_oa, MAXITR * n * n * MIN_NORMAL)
         } // if
         max (abs (TOL) * smax, MAXITR * n * n * MIN_NORMAL)      // absolute accuracy desired
    } // calcThreshold

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' forming a diagonal matrix consisting of singular
     *  values and return the singular values in a vector.
     */
    def factor (): Tuple3 [MatrixD, VectorD, MatrixD] =
    {
        (null, null, null)                                  // FIX
    } // factor
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate the bidiagonal matrix by iteratively turning super-diagonal elements
     *  to zero. Then return the vector of singular values (i.e., the main diagonal).
     */
    def deflate (): VectorD =
    {
        var go   = true                                    // go flag, continue deflation
        var iter = 0                                       // cumulative iterations of inner loop

        do {                                               // begin main iteration loop
            var idir = 0                                   // bulge (nonzero e-values) chasing direction
            if (m == 0) go = false                         // upper index m is at lower limit, done

            if (go) {                                      // find block (ll, m) to work on 
                val ll = findBlock ()                      // e(ll) through e(m-1) are nonzero, e(ll-1) is zero
                if (DEBUG) trace (iter, ll)
                if (ll >= 0) {
                    if (ll == m-1) {                           // block is 2-by-2, handle as a special case
                        deflate_2by2 (ll)                   
                        go = false                             // no blocks left, done
                    } else {                                   // block >= 3-by-3
                        chooseDirection (ll)                   // choose bulge chasing direction
                        if (idir == 1) convergeForward (ll)    // apply convergence tests (set almost zero to zero)
                        else           convergeBackward (ll)
                        oldll = ll                             // save ll and m
                        oldm  = m
                        val shift = computeShift (ll)          // compute amount of shift
                        take_QRstep (ll, shift, idir)          // take one QR step (use rotation to clear an e-value)
                        iter += m - ll                         // increment iteration count
                    } // if
                } // if
            } // if
        } while (go && iter < maxit)

        if (go) {                                          // loop exited due to iteration limit
            val nz = countNonzeroElements ()
            if (nz > 0) {
                println ("deflate: failed to converge - " + nz + " nonzero elements in super-diagonal")
                return null
            } // if
        } // if

        if (DEBUG) println ("diagonal d    = " + d + "\nsup-diag e    = " + e)
   
        makePositive ()                // make singular values positive
        if (DO_SORT) sortValues ()     // sort singular values into decreasing order
        notflat = false                // matrix B is now deflated
        d                              // return the singular values
    } // deflate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate the bidiagonal matrix by iteratively turning super-diagonal elements
     *  to zero. Then return the vector of singular values and the matrices of
     *  singular vectors.
     */
    def deflateV (): Tuple3 [VectorD, MatrixD, MatrixD] =
    {
        if (ONLY_S) {
            println ("deflateV: cannot be called when ONLY_S is true, set it to false")
            return null
        } // if
        if (notflat) deflate ()
        if (vt.dim1 < 1 || u.dim1 < 1) println ("deflateV: warning matrix vt or u is empty")
        (d, vt, u)
    } // deflateV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate 2 by 2 block, handle separately.
     *  @param ll  the lower index
     */
    def deflate_2by2 (ll: Int)
    {
        val svd2 = new SVD_2by2 (d(m-1), e(m-1), d(m))
        val d1   = svd2.deflateV ()
        sigmn = d1._1; sigmx = d1._2; sinr = d1._3; cosr = d1._4; sinl = d1._5; cosl = d1._6 
//      CALL DLASV2 (d(m-1), e(m-1), d(m), sigmn, sigmx, sinr, cosr, sinl, cosl)
        d(m-1) = sigmx
        e(m-1) = 0.0
        d(m)   = sigmn

        if (! ONLY_S) {                         // compute singular vectors, if desired
            if (ncvt > 0) rot (ncvt, vt(m-1), vt(m), cosr, sinr)
            if (nru > 0)  rotCol (nru, u, m-1, m, cosl, sinl)
//          if (ncvt > 0) CALL DROT (ncvt, vt(m-1, 1), ldvt, vt(m, 1), ldvt, cosr, sinr)
//          if (nru > 0)  CALL DROT (nru,  u(1, m-1),  1,    u(1, m) , 1,    cosl, sinl)
        } // if
        m -= 2
    } // deflate_2by2 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of nonzero elements in the super-diagonal.  Call if the
     *  maximum number of iterations exceeded, failure to converge
     */
    def countNonzeroElements (): Int =
    {
         var nzero = 0
         for (i <- 0 until n-1 if ! (e(i) =~ 0.0)) nzero += 1
         if (nzero > 0) println ("deflate failed: nzero = " + nzero)
         nzero
    } // countNonzeroElements

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Trace this outer iteration.
     *  @param iter  the total iteration count
     *  @param ll    the lower index
     */
    private def trace (iter: Int, ll: Int)
    {
        println ("iter          = " + iter)
        println ("diagonal d    = " + d)
        println ("sup-diag e    = " + e)
        println ("block (ll, m) = " + (ll, m))
        println ("-------------------------------------------")
    } // trace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find diagonal block '(ll, m)' of matrix to work on, returning the lower
     *  index 'll'.  Also decrements upper index 'm,' if needed.  'e(j)' must be zero
     *  before and after the block.
     */
    private def findBlock (): Int =
    {
        var ll = 0                          // lower index of block
        if (TOL < 0.0 && abs (d(m)) <= thresh) d(m) = 0.0
        smax = abs (d(m))
        smin = smax
//      for (i <- 1 to m-1) {
        for (i <- 1 to m) {
            ll = m - i
            val abs_d = abs (d(ll))
            val abs_e = abs (e(ll))
            if (TOL < 0.0 && abs_d <= thresh) d(ll) = 0.0
            if (abs_e <= thresh) {
                e(ll) = 0.0                 // matrix splits since e(ll) = 0
                if (ll == m-1) {
                    m -= 1                  // reduce upper index by 1
                    return -1               // return and try again to find block
                } // if
                return ll                   // return and try again
            } // if
            smin = min (smin, abs_d)
            smax = max3 (smax, abs_d, abs_e)
        } // for
        ll                                  // return the lower index ll
    } // findBlock

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Working on new submatrix, choose shift direction
     *  (from larger end diagonal element towards smaller).
     *  @param ll  the lower index
     */
    private def chooseDirection (ll: Int)
    {
        if (ll > oldm || m < oldll) {
            if (abs (d(ll) ) >= abs (d(m))) {
                idir = 1                 // chase bulge from top to bottom
            } else {
                idir = 2                 // chase bulge from bottom to top
            } // if
        } // if
    } // chooseDirection

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run convergence test in forward direction.  First apply standard test to
     *  bottom of matrix
     *  @param ll  the lower index
     */
    private def convergeForward (ll: Int)
    {
        if (abs (e(m-1)) <= abs (TOL) * abs (d(m)) || (TOL < 0.0  && abs (e(m-1)) <= thresh)) {
           e(m-1) = 0.0
           return
        } // if

        if (TOL >= 0.0 ) {    // if relative accuracy desired,  apply convergence criterion forward
            var mu = abs (d(ll))
            smin_l = mu
            for (i <- ll to m-1) {
                if (abs (e(i)) <= TOL * mu) {
                    e(i) = 0.0
                    return
                } // if
                mu     = abs (d(i+1)) * (mu / (mu + abs (e(i))))
                smin_l = min (smin_l, mu)
            } // for
        } // if
    } // convergeForward

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run convergence test in backward direction.  First apply standard test to
     *  top of matrix.
     *  @param ll  the lower index
     */
    private def convergeBackward (ll: Int)
    {
        if (abs (e(ll)) <= abs (TOL) * abs (d(ll)) || (TOL < 0.0 && abs (e(ll)) <= thresh)) {
            e(ll) = 0.0
            return
        } // if

        if (TOL >= 0.0) {    // if relative accuracy desired, apply convergence criterion backward
            var mu = abs (d(m))
            smin_l = mu
            for (i <- m-1 to ll by -1) {
               if (abs (e(i)) <= TOL * mu) {
                  e(i) = 0.0
                  return
               } // if
               mu     = abs (d(i)) * (mu / (mu + abs (e(i))))
               smin_l = min (smin_l, mu)
           } // for
       } // if
    } // convergeForward

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute shift by first, test if shifting would ruin relative accuracy,
     *  and if so set the shift to zero.
     *  @param ll  the lower index
     */
    private def computeShift (ll: Int): Double =
    {
        var shft = 0.0
        var sll  = 0.0
        if (TOL >= 0.0 && n * TOL * (smin_l / smax ) <= max (EPSILON, 0.01 * TOL)) {
            return shft             // use a zero shift to avoid loss of relative accuracy
        } // if

//      Compute the shift from 2-by-2 block at end of matrix

        if (idir == 1) {
            sll = abs (d(ll))
            val svd2 = new SVD_2by2 (d(m-1), e(m-1), d(m))
            val d1   = svd2.deflate (); shft = d1(0); r = d1(1)
//          CALL DLAS2 (d(m-1), e(m-1), d(m), shift, r)
        } else {
            sll = abs (d(m))
            val svd2 = new SVD_2by2 (d(ll), e(ll), d(ll+1))
            val d1   = svd2.deflate (); shft = d1(0); r = d1(1)
//          CALL DLAS2 (d(ll), e(ll), d(ll+1), shift, r)
        } // if

//      Test if shift negligible, and if so set to zero

        if (sll > 0.0 && shft*shft / sll*sll < EPSILON) shft = 0.0
        shft
    } // computeShift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take one 'QR' step to push a super-diagonal element toward zero.
     *  @param ll     the lower index
     *  @param shift  the amount of shift
     *  @param idir   the direction, t2b or b2t
     */
    private def take_QRstep (ll: Int, shift: Double, idir: Int)
    {
        if (shift =~ 0.0) {
            if (idir == 1) {
                zeroShiftQR_t2b (ll)
            } else {
                zeroShiftQR_b2t (ll)
            } // if
        } else {
            if (idir == 1) {
                shiftedQR_t2b (ll, shift)
            } else {
                shiftedQR_b2t (ll, shift)
            } // if
        } // if
    } // take_QRstep

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Chase bulge from top to bottom.  Save cos's and'sin's for later singular
     *  vector updates.
     *  @param ll  the lower index
     */
    private def zeroShiftQR_t2b (ll: Int)
    {
        cs    = 1.0
        oldcs = 1.0
        for (i <- ll to m-1) {
            val q1 = rotate (d(i) * cs, e(i)); cs = q1.cs; sn = q1.sn; r = q1.r
//          CALL DLARTG (d(i) * cs, e(i), cs, sn, r)
            if (i > ll) e(i-1) = oldsn * r
            val q2 = rotate (oldcs * r, d(i+1) * sn); oldcs = q2.cs; oldsn = q2.sn; d(i) = q2.r
//          CALL DLARTG (oldcs * r, d(i+1) * sn, oldcs, oldsn, d(i))
            work(i-ll+1)      = cs
            work(i-ll+1+NM1)  = sn
            work(i-ll+1+NM12) = oldcs
            work(i-ll+1+NM13) = oldsn
        } // for
        val h  = d(m) * cs
        d(m)   = h * oldcs
        e(m-1) = h * oldsn

        if (abs (e(m-1)) <= thresh) e(m-1) = 0.0       // test convergence

        if (! ONLY_S) {                                // update singular vectors, if desired
            if (ncvt > 0) rotateV (true,  true, m-ll+1, ncvt, work, 0, work, n-1, vt.slice(ll, vt.dim1))
            if (nru > 0)  rotateV (false, true, nru, m-ll+1, work, NM12, work, NM13, u.sliceCol(ll, u.dim2))
//          if (ncvt > 0) CALL DLASR ('l', 'v', 'f', m-ll+1, ncvt, work(1), work(n), vt(ll, 1), ldvt)
//          if (nru > 0)  CALL DLASR ('r', 'v', 'f', nru, m-ll+1, work(NM12+1), work(NM13+1), u(1, ll), ldu)
        } // if
    } // zeroShiftQR_t2b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Chase bulge from bottom to top.  Save cos's and sin's for later singular
     *  vector updates.
     *  @param ll  the lower index
     */
    private def zeroShiftQR_b2t (ll: Int)
    {
        cs    = 1.0
        oldcs = 1.0
        for (i <- m to ll+1 by -1) {
            val q1 = rotate (d(i) * cs, e(i-1)); cs = q1.cs; sn = q1.sn; r = q1.r
//          CALL DLARTG (d(i) * cs, e(i-1), cs, sn, r)
            if( i < m ) e(i) = oldsn * r
            val q2 = rotate (oldcs * r, d(i-1) * sn); oldcs = q2.cs; oldsn = q2.sn; d(i) = q2.r
//          CALL DLARTG (oldcs * r, d(i-1) * sn, oldcs, oldsn, d(i))
            work(i-ll)      = cs
            work(i-ll+NM1)  = -sn
            work(i-ll+NM12) = oldcs
            work(i-ll+NM13) = -oldsn
        } // for
        val h = d(ll) * cs
        d(ll) = h * oldcs
        e(ll) = h * oldsn

        if (abs (e(ll)) <= thresh) e(ll) = 0.0         // test convergence

        if (! ONLY_S) {                                // update singular vectors, if desired
            if (ncvt > 0) rotateV (true,  false, m-ll+1, ncvt, work, NM12, work, NM13, vt.slice(ll, vt.dim1))
            if (nru > 0)  rotateV (false, false, nru, m-ll+1, work, 0, work, n-1, u.sliceCol(ll, u.dim2))
//          if (ncvt > 0) CALL DLASR ('l', 'v', 'b', m-ll+1, ncvt, work(NM12+1), work(NM13+1), vt(ll, 1), ldvt)
//          if (nru > 0)  CALL DLASR ('r', 'v', 'b', nru, m-ll+1, work(1),  work(n), u(1, ll), ldu)
        } // if
    } // zeroShiftQR_b2t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Using nonzero shift, chase bulge from top to bottom.  Save cos's and
     *  sin's for later singular vector updates
     *  @param ll     the lower index
     *  @param shift  the amount of shift
     */
    private def shiftedQR_t2b (ll: Int, shift: Double)
    {
        var f = (abs (d(ll)) - shift) * (sign (1.0, d(ll)) + shift / d(ll))
        var g = e(ll)
        for (i <- ll to m-1) {
            val q1 = rotate (f, g); cosr = q1.cs; sinr = q1.sn; r = q1.r
//          CALL DLARTG (f, g, cosr, sinr, r)
            if (i > ll) e(i-1) = r
            f      = cosr * d(i) + sinr * e(i)
            e(i)   = cosr * e(i) - sinr * d(i)
            g      = sinr * d(i+1)
            d(i+1) = cosr * d(i+1)
            val q2 = rotate (f, g); cosl = q2.cs; sinl = q2.sn; r = q2.r
//          CALL DLARTG (f, g, cosl, sinl, r)
            d(i)   = r
            f      = cosl * e(i)   + sinl * d(i+1)
            d(i+1) = cosl * d(i+1) - sinl * e(i)
            if (i < m-1) {
                g      = sinl * e(i+1)
                e(i+1) = cosl * e(i+1)
            } // if
            work(i-ll+1)      = cosr
            work(i-ll+1+NM1)  = sinr
            work(i-ll+1+NM12) = cosl
            work(i-ll+1+NM13) = sinl
        } // for
        e(m-1) = f

        if (abs (e( m-1)) <= thresh ) e(m-1) = 0.0     // test convergence

        if (! ONLY_S) {                                // update singular vectors, if desired
            if (ncvt > 0) rotateV (true,  true, m-ll+1, ncvt, work, 0, work, n-1, vt.slice(ll, vt.dim1))
            if (nru > 0)  rotateV (false, true, nru, m-ll+1, work, NM12, work, NM13, u.sliceCol(ll, u.dim2))
//          if (ncvt > 0) CALL DLASR ('l', 'v', 'f', m-ll+1, ncvt, work(1), work(n), vt(ll, 1), ldvt)
//          if (nru > 0)  CALL DLASR ('r', 'v', 'f', nru, m-ll+1, work(NM12+1), work(NM13+1), u(1, ll), ldu)
        } // if
    } // shiftedQR_t2b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Using nonzero shift, chase bulge from bottom to top.  Save cos's and
     *  sin's for later singular vector updates
     *  @param ll     the lower index
     *  @param shift  the amount of shift
     */
    private def shiftedQR_b2t (ll: Int, shift: Double)
    {
        var f = (abs (d(m)) - shift) * (sign(1.0, d(m)) + shift / d(m))
        var g = e(m-1)
        for (i <- m to ll+1 by -1) {
            val q1 = rotate (f, g); cosr = q1.cs; sinr = q1.sn; r = q1.r
//          CALL DLARTG (f, g, cosr, sinr, r)
            if (i < m) e(i) = r
            f      = cosr * d(i)   + sinr * e(i-1)
            e(i-1) = cosr * e(i-1) - sinr * d(i)
            g      = sinr * d(i-1)
            d(i-1) = cosr * d(i-1)
            val q2 = rotate (f, g); cosl = q2.cs; sinl = q2.sn; r = q2.r
//          CALL DLARTG (F, G, COSL, SINL, R)
            d(i)   = r
            f      = cosl * e(i-1) + sinl * d(i-1)
            d(i-1) = cosl * d(i-1) - sinl * e(i-1)
            if (i > ll+1) {
                g      = sinl * e(i-2)
                e(i-2) = cosl * e(i-2)
            } // if
            work(i-ll)      = cosr
            work(i-ll+NM1)  = -sinr
            work(i-ll+NM12) = cosl
            work(i-ll+NM13) = -sinl
        } // for
        e(ll) = f

        if (abs (e(ll)) <= thresh) e(ll) = 0.0         // test convergence

        if (! ONLY_S) {                                // update singular vectors, if desired
            if (ncvt > 0) rotateV (true,  false, m-ll+1, ncvt, work, NM12, work, NM13, vt.slice(ll, vt.dim1))
            if (nru > 0)  rotateV (false, false, nru, m-ll+1, work, 0, work, n-1, u.sliceCol(ll, u.dim2))
//          if (ncvt > 0) CALL DLASR ('l', 'v', 'b', m-ll+1, ncvt, work(NM12+1), work(NM13+1), vt(ll, 1), ldvt)
//          if (nru > 0)  CALL DLASR ('r', 'v', 'b', nru, m-ll+1, work(1), work(n), u(1, ll), ldu)
        } // if
    } // shiftedQR_b2t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** All singular values converged, so make them positive.
     */
    private def makePositive ()
    {
        for (i <- 0 until n) {
            if (d(i) < 0.0) {
                d(i) = -d(i)

                if (! ONLY_S) {   // change sign of singular vectors, if desired
                    if (ncvt > 0) vt(i) *= -1.0
//                  if (ncvt > 0) CALL DSCAL (ncvt, -1.0, vt(i, 1), ldvt)
                } // if
            }  // if
        } // for
    } // makePositive

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort the singular values into decreasing order.  Selection sort is used
     *  to minimize the swapping of singular vectors.  If only sorting singular
     *  values use 'sort2' that uses 'quicksort'.
     */
    private def sortValues ()
    {
        if (ONLY_S) {      // only interested in singular values
            d.sort2 ()     // sort vector d in descending order (use if ignoring vectors)
            return
        } // if

        for (i <- 0 until n-1) {
            var k = i
            for (j <- i+1 until n if d(j) > d(k)) k = j
            if (i != k) {
                d.swap (i, k)                    // swap singular values in vector d
                if (ncvt > 0) vt.swap (i, k)     // swap singular vectors (rows) in matrix vt
                if (nru > 0)  u.swapCol (i, k)   // swap singular vectors (columns) in matrix u
//              if (ncvt > 0) CALL DSWAP (ncvt, vt(isub, 1), ldvt, vt(n+1-i, 1), ldvt)
//              if (nru > 0)  CALL DSWAP (nru, u(1, isub), 1, u(1, n+1-i), 1)
            } // if
        } // for
    } // sortValues

 } // SVD3 class 


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD3Test` is used to test the `SVD3` class.
 *  Answer: singular values = (2.28825, 0.87403)  
 *  @see http://comnuan.com/cmnn01004/
 */
object SVD3Test extends App
{
    import MatrixD.eye

    val a = new MatrixD ((2, 2), 1.0, 1.0,
                                 0.0, 2.0)
    val b   = new BidMatrixD (a)                     // a is already biagonal
    val vt0 = eye (b.dim1)
    val u0  = eye (b.dim1)

    val svd = new SVD3 (b, vt0, u0)

    println ("----------------------------------------")
    println ("Test SVD3")
    println ("----------------------------------------")
    println ("a = " + a)
    println ("b = " + b)
    println ("----------------------------------------")
    println ("singular values  = " + svd.deflate ())

    val (s, vt, u) = svd.deflateV ()
    println ("----------------------------------------")
    println ("singular val/vec = " + (s, vt, u))
    println ("----------------------------------------")
    println ("u ** s * vt = " + u ** s * vt)

} // SVD3Test 


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD3Test2` is used to test the `SVD3` class.
 *  Answer: singular values = (3.82983, 1.91368, 0.81866)
 */
object SVD3Test2 extends App
{
    import MatrixD.eye

    val a = new MatrixD ((3, 3), 1.0, 1.0, 0.0,
                                 0.0, 2.0, 2.0,
                                 0.0, 0.0, 3.0)
    val b  = new BidMatrixD (a)                     // a is already biagonal
    val vt = eye (a.dim1)
    val u  = eye (a.dim1)

    val svd = new SVD3 (b, vt, u)

    println ("----------------------------------------")
    println ("Test SVD3")
    println ("----------------------------------------")
    println ("a = " + a)
    println ("b = " + b)
    println ("----------------------------------------")
    println ("singular values  = " + svd.deflate ())

//  println ("----------------------------------------")
//  println ("singular val/vec = " + svd.deflateV ())

} // SVD3Test2

