
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 1.5
 *  @date    Fri Oct 7 12:27:00 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm. Originally proposed by Byrd et. al in 1995.
 *  See the first two links for the original paper and authors' software (written
 *  in Fortran) distribution site, respectively. This implementation is translated
 *  from a C++ implementation found in the last link.
 *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gz
 *  @see users.iems.northwestern.edu/~nocedal/lbfgsb.html
 *  @see github.com/PatWie/CppNumericalSolvers/blob/master/include/cppoptlib/solver/lbfgsbsolver.h
 */

package scalation.minima

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, max, min, pow}
import scala.util.control.Breaks.{break, breakable}

import scalation.calculus.Differential.∇
import scalation.linalgebra._
import scalation.linalgebra.MatrixD.eye
import scalation.math.ExtremeD.{MAX_VALUE, NEGATIVE_INFINITY, POSITIVE_INFINITY}
import scalation.math.FunctionV2S

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `L_BFGS_B` the class implements the Limited memory Broyden–Fletcher–
 *  Goldfarb–Shanno for Bound constrained optimization (L-BFGS-B)
 *  Quasi-Newton Algorithm for solving Non-Linear Programming (NLP) problems.
 *  L-BFGS-B determines a search direction by  deflecting the steepest descent direction
 *  vector (opposite the gradient) by *  multiplying it by a matrix that approximates
 *  the inverse Hessian. Furthermore, only a few vectors represent the approximation
 *  of the Hessian Matrix (limited memory). The parameters estimated are also bounded
 *  within user specified lower and upper bounds.
 *
 *  minimize    f(x)
 *  subject to  g(x) <= 0   [ optionally g(x) == 0 ]
 *
 *  @param f        the objective function to be minimized
 *  @param g        the constraint function to be satisfied, if any
 *  @param ineq     whether the constraint is treated as inequality (default) or equality
 *  @param exactLS  whether to use exact (e.g., `GoldenLS`)
 *                            or inexact (e.g., `WolfeLS`) Line Search
 *  @param l        vector of lower bounds for all input parameters
 *  @param u        vector of upper bounds for all input parameters
 */
class L_BFGS_B (f: FunctionV2S, g: FunctionV2S = null, ineq: Boolean = true, exactLS: Boolean = false,
                private var l: VectoD = null, private var u: VectoD = null)
      extends Minimizer
{
    private val DEBUG           = false             // the debug flag
    private val WEIGHT          = 1000.0            // weight on penalty for constraint violation
    private var ww, mm : MatriD = null              // workspace matrices
    private var theta           = 0.0               // a scaling parameter
    private var dim             = 0                 // dimension of the input vector
    private var hs              = 5                 // history size, number of historical vectors to store

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort pairs (k, v) according to v ascending order.
     *  @param v  ArrayBuffer of Tuple2 to be sorted by the 2nd element
     */
    private def sortIndices (v: ArrayBuffer [(Int, Double)]): VectoI =
    {
        val sv  = v.sortBy (_._2)
        val idx = new VectorI (sv.length)
        for (i <- idx.range) idx(i) = sv(i)._1
        idx
    } // sortIndices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Algorithm CP: Computation of the Generalized Cauchy Point. See page 8 of
     *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gvz
     *  @param x   the parameter vector
     *  @param gr  the gradient vector
     */
    private def getGCP (x: VectorD, gr: VectoD): Pair =
    {
        val setOfT = new ArrayBuffer[(Int, Double)]()
        val d      = -gr
        for (j <- 0 until dim) {
            if (gr(j) == 0) setOfT.append ((j, MAX_VALUE))
            else {
                val tmp = if (gr(j) < 0) (x(j) - u(j)) / gr(j)
                          else           (x(j) - l(j)) / gr(j)
                setOfT.append ((j, tmp))
                if (tmp == 0) d(j) = 0
            } // if
        } // for
        val sortedIndices = sortIndices (setOfT)
        val xCauchy       = x.copy ()

        val p             = ww.t * d
        val c             = new VectorD (ww.dim2)
        var fPrime        = -d dot d
        var fDoublePrime  = max (-theta * fPrime - (p dot mm * p), EPSILON)
        val f_dp_orig     = fDoublePrime
        var dt_min        = -fPrime / fDoublePrime
        var t_old         = 0.0

        var i = 0
        breakable {for (j <- 0 until dim) {
            i = j
            if (setOfT(sortedIndices(j))._2 > 0) break
        }} // breakable for
        var b  = sortedIndices (i)
        var t  = setOfT(b)._2
        var dt = t

        while (dt_min >= dt && i < dim) {
            xCauchy(b)   =  if      (d(b) > 0) u(b)
                            else if (d(b) < 0) l(b)
                            else               xCauchy(b)
            val zb        = xCauchy(b) - x(b)
            c            += p * dt
            val wbt       = ww(b)
            fPrime       += dt * fDoublePrime + gr(b) * gr(b) + theta * gr(b) * zb - gr(b) * (wbt dot mm * c)
            fDoublePrime +=  -theta  *  gr(b) *  gr(b)
                             - 2.0   * (gr(b) * (wbt dot mm * p))
                             - gr(b) *  gr(b) * (wbt dot mm * wbt)
            fDoublePrime  = max (EPSILON * f_dp_orig, fDoublePrime)
            p            += wbt * gr(b)
            d(b)          = 0
            dt_min        = -fPrime / fDoublePrime
            t_old         = t
            i            += 1
            if (i < dim) {
                b  = sortedIndices (i)
                t  = setOfT (b)._2
                dt = t - t_old
            } // if
        } //

        dt_min = max (dt_min, 0.0)
        t_old += dt_min

        for (ii <- i until xCauchy.dim) {
            val si       = sortedIndices (ii)
            xCauchy(si) = x(si) + t_old * d(si)
        } // for
        c += p * dt_min
        (xCauchy, c)
    } // getGCP

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the alpha* parameter, a positive scalar. See Equation 5.8 on page 11 of
     *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gvz
     *  @param x_cp     vector of cauchy point
     *  @param du       vector containing intermediate results used to find alpha*
     *  @param freeVar  an ArrayBuffer storing the indices of free variable
     */
    private def findAlpha (x_cp: VectoD, du: VectoD, freeVar: ArrayBuffer [Int]): Double =
    {
        var alphastar = 1.0
        val n         = freeVar.size
        assert (du.dim == n)
        for (i <- 0 until n) {
            val fi = freeVar(i)
            alphastar = if (du(i) > 0) min (alphastar, (u(fi) - x_cp(fi)) / du(i))
                        else           min (alphastar, (l(fi) - x_cp(fi)) / du(i))
        } // for
        return alphastar
    } // findAlpha

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Minimization of the subspace of free variables. See Section 5 on page 9 of
     *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gvz
     *  @param x        the parameter vector
     *  @param gr       the gradient vector
     *  @param xCauchy  the vector of cauchy points
     *  @param c        vector obtained from getGCP used to initialize the subspace
     *                  minimization process
     */
    private def subspaceMinimization (x: VectoD, gr: VectoD, xCauchy: VectorD, c: VectoD): VectorD =
    {
        val thetaInverse = 1.0 / theta
        val freeVarIdx   = new ArrayBuffer [Int]()
        for (i <- 0 until xCauchy.dim if (xCauchy(i) != u(i) && xCauchy(i) != l(i))) freeVarIdx.append (i)
        val freeVarCount = freeVarIdx.size
        val wwzz         = new MatrixD (ww.dim2, freeVarCount)
        for (i <- 0 until freeVarCount) wwzz.setCol(i, ww(freeVarIdx(i)))
        val rr           = (gr + (xCauchy - x) * theta - ww * (mm * c))
        val r            = new VectorD (freeVarCount)
        for (i <- 0 until freeVarCount) r(i) = rr(freeVarIdx(i))

        var v  = mm * (wwzz * r)
        var nn = wwzz * wwzz.t * thetaInverse
        nn     = eye (nn.dim1, nn.dim1) - mm * nn

        val lu = new Fac_LU (nn)
        lu.factor ()
        v      = lu.solve (v)

        val du          = r * -thetaInverse - wwzz.t * v * thetaInverse * thetaInverse
        val alpha_star  = findAlpha (xCauchy, du, freeVarIdx)
        val dStar       = du * alpha_star
        val subspaceMin = xCauchy.copy
        for (i <- 0 until freeVarCount) subspaceMin (freeVarIdx(i)) += dStar(i)
        subspaceMin
    } // subspaceMinimization

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the mean gradient norm squared
     *  @param x   the parameter vector
     *  @param gr  the gradient vector
     */
    private def getMgn (x: VectoD, gr: VectoD): Double =
    {
        val x_gr       = x - gr
        val checkLower = VectorD (for (i <- l.range) yield max (x_gr(i)      , l(i)))
        val checkUpper = VectorD (for (i <- u.range) yield min (checkLower(i), u(i)))
        val mgn        = (checkUpper - x).normSq / dim
        mgn
    } // getMgn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Force the values within 'v' to stay within the pre-defined bounds.
     *  @param v  the Vector containing values to be adjusted
     */
    private def forceBounds (v: VectoD)
    {
        for(i <- 0 until v.dim) {
            if      (v(i) > u(i)) v(i) = u(i)
            else if (v(i) < l(i)) v(i) = l(i)
        } // for
    } // forceBounds

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Modify the number of historical vectors to store.
     *  @param hs_  the new history size
     */
    def setHistorySize (hs_ : Int) = hs = hs_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function f plus a weighted penalty based on the constraint
     *  function g.
     *  @param x  the coordinate values of the current point
     */
    override def fg (x: VectorD): Double =
    {
        val f_x = f(x)
        if (g == null) {                  // unconstrained
            f_x
        } else {                          // constrained, g(x) <= 0
            val penalty = if (ineq) max (g(x), 0.0) else abs (g(x))
            f_x + abs (f_x) * WEIGHT * penalty * penalty
        } // if
    } // fg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact 'GoldenSectionLS' or inexact 'WolfeLS' Line Search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
    {
        def f_1D (z: Double): Double = fg(x + dir * z)    // create a 1D function
        val ls = if (exactLS) new GoldenSectionLS (f_1D)  // Golden Section Line Search
                 else new WolfeLS (f_1D)                  // Wolfe line search ((c1 = .0001, c2 = .9)
        ls.search (step)                                  // perform a Line Search
    } // lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the following Non-Linear Programming (NLP) problem using L-BFGS-B:
     *  min { f(x) | g(x) <= 0 }.
     *  @param x0           the starting point
     *  @param alphaInit    the initial step size
     *  @param toler        the tolerance
     */
    def solve (x0: VectorD, alphaInit : Double = STEP, toler: Double = TOL): VectorD =
    {
        if (DEBUG) println ("L_BFGS_B.solve: starting at x0 = " + x0)

        dim = x0.size

        if (l == null) l = VectorD.fill (dim)(NEGATIVE_INFINITY)
        if (u == null) u = VectorD.fill (dim)(POSITIVE_INFINITY)

        theta = 1.0

        ww = new MatrixD (dim, 0)
        mm = new MatrixD (0, 0)

        val yHistory = new ArrayBuffer [VectoD] ()
        val sHistory = new ArrayBuffer [VectoD] ()
        var yHistoryMx: MatriD = null
        var sHistoryMx: MatriD = null
        var (x, gr)  = (x0, ∇ (fg, x0))
        var fv       = fg(x)
        var mgn      = 0.0
        var count    = 0
        val countMax = 10

        breakable{ for (k <- 1 to MAX_ITER) {
            val f_old   = fv
            val x_old   = x
            val g_old   = gr
            val mgn_old = mgn

            val (xCauchy, c) = getGCP (x, gr)                           // STEP 2: compute the cauchy point
            forceBounds (xCauchy)

            val subspaceMin = subspaceMinimization(x, gr, xCauchy, c)   // STEP 3: compute a search direction d_k by the primal method for the sub-problem
            forceBounds (subspaceMin)

            val rate = lineSearch (x, subspaceMin-x, alphaInit)         // STEP 4: perform linesearch and STEP 5: compute gradient
            x = x - (x - subspaceMin) * rate                            // update current guess and function information
            forceBounds (x)

            for (x_i <- x if (x_i.isNaN || x_i.isInfinite)) return x_old
            fv = fg(x)
            if (fv.isNaN || fv.isInfinite) {
                if (DEBUG) println ("Bad value produced by the objective function, return previous good x")
                return x_old
            } // if
            gr  = ∇ (fg, x)
            mgn = getMgn (x, gr)
            if (mgn < toler || count > countMax) return x
            if (abs(mgn - mgn_old) < toler)      count += 1

            val newY = gr - g_old                                       // prepare for next iteration
            val newS = x - x_old
            val test = abs (newS dot newY)                              // STEP 6
            if (test > EPSILON * newY.normSq) {
                if (yHistory.size >= hs) { yHistory.remove (0); sHistory.remove (0) }
                yHistory append newY
                sHistory append newS
                theta      = (newY dot newY) / (newY dot newS)          // STEP 7
                yHistoryMx = MatrixD (for (y <- yHistory) yield y)
                sHistoryMx = MatrixD (for (s <- sHistory) yield s)
                ww         = yHistoryMx ++^ (sHistoryMx * theta)
                val aa     = sHistoryMx.t * yHistoryMx
                val ll     = aa.lowerT
                ll.setDiag (0.0)
                val dd     = new MatrixD (aa.dim1, aa.dim2)
                dd.setDiag (-aa.getDiag())
                val mm2    = (dd ++^ ll.t) ++ (ll ++^ (sHistoryMx.t * sHistoryMx * theta))
                mm         = mm2.inverse
            } // if

            if (DEBUG) println ("solve: (k = " + k + ") move from " + x_old + " to " + x + " where fg(x) = " + fv)

            if (abs (f_old - fv) < toler) break                         // successive function values too similar
        }} // breakable for
        x
    } // solve

} // L_BFGS_B class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `L_BFGS_BTest` object is used to test the `L_BFGS_B` class.
 *  > runMain scalation.minima.L_BFGS_BTest
 */
object L_BFGS_BTest extends App
{
    def x0 = new VectorD (2)
    val l  = VectorD.fill (x0.dim)(3.5)
    val u  = VectorD.fill (x0.dim)(5.0)

    println ("\nMinimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    var solver = new L_BFGS_B (f, l = l, u = u)
    var x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value f(x) = " + f(x))

    println ("\nMinimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def g (x: VectorD): Double = pow (x(0), 4.0) + (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    solver = new L_BFGS_B (g, l = l, u = u)
    x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value g(x) = " + g(x))

} // L_BFGS_BTest object

