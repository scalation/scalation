
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Tue Sep 16 14:45:38 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.analytics;

import static java.util.Arrays.fill;

import static jalation.util.Error.flaw;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClassifierInt` abstract class provides a common foundation for several
 *  classifiers that operate on integer-valued data.
 *  @param x   the integer-valued training data vectors stored as rows of a matrix
 *  @param y   the training classification vector, where y_i = class for row i of the matrix x
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
abstract class ClassifierInt
         implements Classifier
{
    /** the number of data vectors in training-set (# rows)
     */
    protected final int m;

    /** the number of features/variables (# columns)
     */
    protected final int n;

    /** the training-set size as a Double
     */
    protected final double md;

    /** the feature-set size as a Double
     */
    protected final double nd;

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct an Integer-based classifier.
     *  @param x   the integer-valued training data vectors stored as rows of a matrix
     *  @param y   the training classification vector, where y_i = class for row i of the matrix x
     *  @param fn  the names for all features/variables
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     */
    public ClassifierInt (int [][] x, int [] y, String [] fn, int k, String [] cn)
    {
        m  = x.length;
        n  = x[0].length;
        md = (double) m;
        nd = (double) n;

        if (y.length != m)  flaw ("constructor", "y.dim must equal training-set size (m)");
        if (fn.length != n) flaw ("constructor", "fn.length must equal feature-set size (n)");
        if (k >= m)         flaw ("constructor", "k must be less than training-set size (m)");
        if (cn.length != k) flaw ("constructor", "cn.length must equal number of classes (k)");

    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return default values for binary input data (value count (vc) set to 2).
     */
    public int [] vc_default ()
    {
        int [] vc = new int [n];
        fill (vc, 2);
        return vc;
    } // default 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param xx  the integer-valued test vectors stored as rows of a matrix
     *  @param yy  the test classification vector, where yy_i = class for row i of xx
     */
    public double test (int [][] xx, int [] yy)
    {
        int mm = xx.length;
        if (yy.length != mm) flaw ("test", "yy.dim must equal test-set size (mm)");
        int correct = 0;
        for (int i = 0; i < mm; i++) if (classify (xx[i]) == yy[i]) correct += 1;
        return correct / (double) mm;
    } // test

} // ClassifierInt abstract class

