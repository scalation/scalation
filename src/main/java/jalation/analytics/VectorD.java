
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Fri Sep 19 12:42:31 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.analytics;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorD` class provides common operations on vectors (double []).
 */
public class VectorD
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add two vectors.
     *  @param x  the first vector
     *  @param y  the second vector
     */
    public static double [] add (double [] x, double [] y)
    {
        double [] z = new double [x.length];
        for (int i = 0; i < x.length; i++) z[i] = x[i] + y[i];
        return z;
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract two vectors.
     *  @param x  the first vector
     *  @param y  the second vector
     */
    public static double [] minus (double [] x, double [] y)
    {
        double [] z = new double [x.length];
        for (int i = 0; i < x.length; i++) z[i] = x[i] - y[i];
        return z;
    } // subtract

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements in the vector.
     *  @param x  the vector to sum
     */
    public static double sum (double [] x)
    {
        double sum = 0.0;
        for (int i = 0; i < x.length; i++) sum += x[i];
        return sum;
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of two vectors.
     *  @param x  the first vector
     *  @param y  the second vector
     */
    public static double dot (double [] x, double [] y)
    {
        double sum = 0.0;
        for (int i = 0; i < x.length; i++) sum += x[i] * y[i];
        return sum;
    } // dot

} // VectorD

