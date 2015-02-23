
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Tue Sep 16 14:45:38 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.analytics;

import java.util.Arrays;
import static java.lang.System.out;

import static jalation.analytics.VectorD.*;
import static jalation.util.Error.flaw;

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegression` class supports simple linear regression.
 *  Fit the parameter vector 'b' in the regression equation
 *  <p>
 *      y  =   b0 + b1 * x + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 */
public class SimpleRegression
       implements Predictor
{
    /** debug flag
     */
    private static final boolean DEBUG = true;

    /** the x data vector
     */
    private final double [] x;

    /** the y data vector
     */
    private final double [] y;

    /** parameter vector (b0, b1)
     */
    private final double [] b = new double [2];

    /** number of data points
     */
    private final int n;

    /** number of data points, as a double
     */
    private final double nd;

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a solver for simple linear regression.
     *  @param x_  the input/design matrix augmented with a first column of ones
     *  @param y_  the response vector
     */
    public SimpleRegression (double [] x_, double [] y_)
    {
        x = x_;
        y = y_;
        if (x.length != y.length) flaw ("constructor", "dimensions of x and y are incompatible");

        n  = x.length;              // number of data points
        nd = (double) n;            // number of data points, as a double
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  simple regression equation
     *      y = b0 = b1 * x + e
     *  using the least squares method.  Return the quality of fit.
     *  @see http://www.analyzemath.com/statistics/linear_regression.html
     */
    public double [] train ()
    {
        double sx  = sum (x);                     // sum of x values
        double sy  = sum (y);                     // sum of y values
        double ssx = dot (x, x);                  // sum of squares x
        double ssy = dot (y, y);                  // sum of squares y
        double sxy = dot (x, y);                  // sum of cross products

        if (DEBUG) {
            out.println ("sx  = " + sx);
            out.println ("sy  = " + sy);
            out.println ("ssx = " + ssx);
            out.println ("ssy = " + ssy);
            out.println ("sxy = " + sxy);
        } // if

        b[1] = (nd * sxy - sx * sy) / (nd * ssx - sx * sx);  // slope
        b[0] = (sy - b[1] * sx) / nd;                        // intercept

        double [] e = res ();                                // residual/error vector
        double sse  = dot (e, e);                            // residual/error sum of squares
        double sst  = ssy - sy * sy / nd;                    // total sum of squares
        double [] rSquared = { (sst - sse) / sst };          // coefficient of determination R^2
        return rSquared;
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (i.e., the parameter vector b).
     */
    public double [] fit ()
    {
        return b;
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b0 + b1 * z.
     *  @param z  the new input (scalar) used for prediction
     */
    public double predict (double z)
    {
       return b[0] + b[1] * z;
    } // predict

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b0 + b1 * z.
     *  @param z  the new input (vector) used for prediction
     */
    public double predict (double [] z)
    {
       return predict (z[0]);
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the residual/error vector.
     */
    private double [] res ()
    {
        double [] e = new double [n];
        for (int i = 0; i < n; i++) e[i] = y[i] - (b[0] + x[i] * b[1]);
        return e;
    } // res

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The main method tests the SimpleRegression class.
     *  @see http://www.analyzemath.com/statistics/linear_regression.html
     */
    public static void main (String [] args)
    {
        double [] x = { 0.0, 1.0, 2.0, 3.0, 4.0 };      // x vector
        double [] y = { 2.0, 3.0, 5.0, 4.0, 6.0 };      // y vector

        out.println ("x = " + Arrays.toString (x));
        out.println ("y = " + Arrays.toString (y));

        SimpleRegression rg = new SimpleRegression (x, y);
        out.println ("R^2 = " + rg.train ());
        out.println ("fit = " + Arrays.toString (rg.fit ()));

        double z  = 5.0;                      // predict y for one point
        double yp = rg.predict (z);
        out.println ("predict (" + z + ") = " + yp);
    } // main

} // SimpleRegression class

