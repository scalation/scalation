
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{VectorD, VectorI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Classifier` trait provides a common framework for several classifiers.
 */
trait Classifier
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors and their classifications, build a classifier.
     */
    def train ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, determine which class it belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectorI): Tuple2 [Int, String]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, determine which class it belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectorD): Tuple2 [Int, String]

} // Classifier trait

