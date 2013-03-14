
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.VectorD
import scalation.linalgebra_gen.Vectors.VectorI

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This trait provides a common framework for several classifiers.
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
    def classify (z: VectorI): Int = classify (z.toDouble)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, determine which class it belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectorD): Int

} // Classifier trait

