
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Tue Sep 16 14:45:38 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.analytics;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Classifier` interface provides a common framework for several classifiers.
 */
interface Classifier
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors and their classifications, build a classifier.
     */
    public void train ();

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, determine which class it belongs to.
     *  @param z  the vector to classify
     */
    public int classify (int [] z);

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, determine which class it belongs to.
     *  @param z  the vector to classify
     */
    public int classify (double [] z);

} // Classifier interface

