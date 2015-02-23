
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Fri Sep 19 12:42:31 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.analytics;

import java.util.Arrays;
import static java.lang.System.out;
import static java.lang.Math.pow;

import Jama.Matrix;
import Jama.Matrix.*;
import static jalation.analytics.VectorD.*;
import static jalation.util.Error.flaw;

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression` class supports multiple linear regression.  In this case,
 *  'x' is multi-dimensional [1, x1, ... xk].  Fit the parameter vector 'b' in the
 *  regression equation
 *  <p>
 *      y  =  b dot x + e  =  b0 + b1 * x1 + ... bk * xk + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *      b  =  x_pinv * y
 *  <p>
 *  where 'x_pinv' is the pseudo-inverse.  By default QR Decomposition (more robust) is
 *  used to compute 'x_pinv', with Gaussian Elimination as an option (set useQR to false).
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 */
public class Regression
       implements Predictor
{
    private static final boolean DEBUG = false;                   // debug flag

    private final Matrix    x;
    private final double [] y;
    private final int       k;
    private final int       m;
    private final double    md;
    private final double    r_df;

    private double [] b;
    private double [] e;
    private double    rSquared;
    private double    rBarSq;
    private double    fStat;
    private Matrix    x_pinv;

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a Multiple Linear Regression solver.
     *  @param x_     the input/design m-by-n matrix augmented with a first column of ones
     *  @param y_     the response vector
     *  @param useQR  use QR Decomposition for pseudo-inverse, else Gaussian Elimination
     */
    public Regression (Matrix x_, double [] y_, boolean useQR)
    {
        x  = x_;
        y  = y_;
        k  = x.getColumnDimension () - 1;    // number of variables (k = n-1)
        m  = x.getRowDimension ();           // number of data points (rows)
        md = (double) m;                     // number of data points (rows), as a double

        if (y != null && m != y.length) flaw ("constructor", "dimensions of x and y are incompatible");
        if (m <= k+1) flaw ("constructor", "not enough data rows in matrix to use regression");

        r_df     = (m-1.0) / (m-k-1.0);      // ratio of degrees of freedom
        b        = null;                     // parameter vector [b0, b1, ... bk]
        rSquared = -1.0;                     // coefficient of determination (quality of fit)
        rBarSq   = -1.0;                     // Adjusted R-squared
        fStat    = -1.0;                     // F statistic (quality of fit)

        // pre-compute the pseudo-inverse of Matrix x
//      if (useQR) {
//          val (q, r) = (new QRDecomp (x)).getQR; x_pinv = r.inverse * q.t
//      } else {
            Matrix xt      = x.transpose ();
            Matrix xtx_inv = xt.times (x).inverse ();
            x_pinv = xtx_inv.times (xt);
//      } // if
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      y  =  b dot x + e  =  [b0, ... bk] dot [1, x1 , ... xk] + e
     *  using the least squares method.
     */
    public double [] train ()
    {
        b = x_pinv.times (y);                                    // parameter vector [b0, b1, ... bk]
        e = minus (y, x.times (b));                              // residual/error vector
        double sse = dot (e, e);                                 // residual/error sum of squares
        double sst = dot (y, y) - pow (sum (y), 2.0) / md;       // total sum of squares
        double ssr = sst - sse;                                  // regression sum of squares
        rSquared = ssr / sst;                                    // coefficient of determination (R-squared)
        rBarSq   = 1.0 - (1.0-rSquared) * r_df;                  // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (m-k-1.0)  / (sse * k);                 // F statistic (msr / mse)
        return new double [] { rSquared, rBarSq, fStat };
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      yy  =  b dot x + e  =  [b0, ... bk] dot [1, x1 , ... xk] + e
     *  using the least squares method.
     *  @param yy  the new response vector
     */
    public double [] train (double [] yy)
    {
        b = x_pinv.times (yy);                                   // parameter vector [b0, b1, ... bk]
        e = minus (yy, x.times (b));                             // residual/error vector
        double sse  = dot (e, e);                                // residual/error sum of squares
        double sst  = dot (yy, yy) - pow (sum (yy), 2.0) / md;   // total sum of squares
        double ssr  = sst - sse;                                 // regression sum of squares
        rSquared = ssr / sst;                                    // coefficient of determination
        rBarSq   = 1.0 - (1.0-rSquared) * r_df;                  // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (m-k-1.0)  / (sse * k);                 // F statistic (msr / mse)
        return new double [] { rSquared, rBarSq, fStat };
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit rSquared).
     */
    public double [] fit ()
    {
        return b;
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b0, b1, b2) dot (1, z1, z2).
     *  @param z  the new vector to predict
     */
    public double predict (double [] z)
    {
         return dot (b, z);
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     *
    public backElim (): Tuple4 [Int, VectorD, Double, Double] =
    {
        var j_max   = -1                     // index of variable to eliminate
        var b_max: VectorD = null            // parameter values for best solution
        var rSq_max = -1.0                   // currently maximizing R squared
        var fS_max  = -1.0                   // could optimize on F statistic

        for (j <- 1 to k) {
            val keep = m.toInt               // i-value large enough to not exclude any rows in slice
            val rg_j = new Regression (x.sliceExclude (keep, j), y)       // regress with x_j removed
            rg_j.train ()
            val (b, rSq, fS, rBar) =  rg_j.fit
            if (rSq > rSq_max) { j_max = j; b_max = b; rSq_max = rSq; fS_max = fS}
        } // for
        (j_max, b_max, rSq_max, fS_max)
    } // backElim
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-colinearity by regressing xj against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the varaince of xj can be predicted
     *  from the other variables, so xj is a candidate for removal from the model.
     *
    public vif: VectorD =
    {
        val vifV = new VectorD (k)           // VIF vector
        for (j <- 1 to k) {
            val keep = m.toInt               // i-value large enough to not exclude any rows in slice
            val x_j  = x.col(j)                                           // x_j is jth column in x
            val rg_j = new Regression (x.sliceExclude (keep, j), x_j)     // regress with x_j removed
            rg_j.train ()
            vifV(j-1) =  1.0 / (1.0 - rg_j.fit._2)                        // store vif for x_1 in vifV(0)
        } // for
        vifV
    } // vif
     */

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The main method tests `Regression` class using the following regression equation.
     *  <p>
     *      y  =  b dot x  =  b0 + b1*x1 + b2*x2.
     *  <p>
     *  Test regression and backward elimination.
     *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
     */
    public static void main (String [] args)
    {
        // 5 data points: constant term, x1 coordinate, x2 coordinate
        double [][] vals = { { 1.0, 36.0,  66.0 },               // 5-by-3 matrix
                             { 1.0, 37.0,  68.0 },
                             { 1.0, 47.0,  64.0 },
                             { 1.0, 32.0,  53.0 },
                             { 1.0,  1.0, 101.0 }};
        Matrix x    = new Matrix (vals);
        double [] y = { 745.0, 895.0, 442.0, 440.0, 1598.0 };

        out.print ("x = "); x.print (12, 4);
        out.println ("y = " + Arrays.toString (y));

        Regression rg = new Regression (x, y, false);
        out.println ("R^2 = " + Arrays.toString (rg.train ()));
        out.println ("b   = " + Arrays.toString (rg.fit ()));

        double [] z  = { 1.0, 20.0, 80.0 };              // predict y for one point
        double yp    = rg.predict (z);
        out.println ("predict (" + Arrays.toString (z) + ") = " + yp);

//      out.println ("backElim = " + rg.backElim ())     // eliminate least predictive variable
    } // main

} // Regression class

