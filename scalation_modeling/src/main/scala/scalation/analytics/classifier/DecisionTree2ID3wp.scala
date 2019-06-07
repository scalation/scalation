
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Susan George
 *  @version 1.6
 *  @date    Wed May 22 14:17:49 EDT 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.collection.mutable.Set

import scalation.linalgebra.{MatriI, MatrixI, VectoI}
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3wp` class extends `DecisionTree2ID3` with pruning capabilities.
 *  The base class uses the ID3 algorithm to construct a decision tree for classifying
 *  instance vectors.
 *  @param x       the input/data matrix with instances stored in rows
 *  @param y       the response/classification vector, where y_i = class for row i of matrix x
 *  @param fn_     the names for all features/variables
 *  @param k       the number of classes
 *  @param cn_     the names for all classes
 *  @param height  the maximum height of tree (max edge count root to leaf)
 */
class DecisionTree2ID3wp (x: MatriI, y: VectoI, fn_ : Strings = null,
                          k: Int = 2, cn_ : Strings = null, height: Int = Int.MaxValue)
      extends DecisionTree2ID3 (x, y, fn_, k, cn_)
{
    private val DEBUG = true                                     // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find candidate nodes that may be pruned, i.e., those that are parents
     *  of leaf nodes, restricted to those that don't have any children that
     *  are themselves internal nodes.
     */
    def candidates: Set [Node] =
    {
        val can = Set [Node] ()
        for (n <- leaves) {
            val p = n.parent
            if (leafChildren (p)) can += p
        } // for
        can
    } // candidates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether all the children of node 'n' are leaf nodes.
     *  @param n  the node in question
     */
    def leafChildren (n: Node): Boolean =
    {
        for (c <- n.branch.values if ! c.isLeaf) return false
        true
    } // leafChildren

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Of all the pruning candidates, find the one with the least gain.
     *  @param can  the nodes that are canidates for pruning
     */
    def bestCandidate (can: Set [Node]): (Node, Double) =
    {
        var min = Double.MaxValue
        var best: Node = null
        for (n <- can if n.gn < min) { min = n.gn; best = n }
        (best, min)
    } // bestCandidate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prune 'nPrune' nodes from the tree, the ones providing the least gain.
     *  @param nPrune  the number of nodes to be pruned.
     */
    def prune (nPrune: Int = 1)
    {
        for (i <- 0 until nPrune) {
            val can = candidates
            if (DEBUG) println (s"can = $can")
            val (best, gn) = bestCandidate (can)
            println (s"prune: make node $best with gain $gn into a leaf")
            makeLeaf (best)
        } // for
    } // prune

} // DecisionTree2ID3wp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3wp` companion object provides a factory function.
 */
object DecisionTree2ID3wp extends App
{
    import ClassifierInt.pullResponse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a decision tree for the given combined matrix where the last column
     *  is the response/classification vector.
     *  @param xy      the combined data matrix (features and response)
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param height  the maximum height of tree (max edge count root to leaf)
     */
    def apply (xy: MatriI, fn: Strings, k: Int, cn: Strings,
               height: Int = Int.MaxValue): DecisionTree2ID3wp =
    {
        val (x, y) = pullResponse (xy)
        new DecisionTree2ID3wp (x, y, fn, k, cn, height)
    } // apply

} // DecisionTree2ID3wp object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3wpTest` object is used to test the `DecisionTreeID3wp` class.
 *  > runMain scalation.analytics.classifier.DecisionTree2ID3wpTest
 */
object DecisionTree2ID3wpTest extends App
{
    import ExampleTennis.{xy, fn, k, cn}

    val tree = DecisionTree2ID3wp (xy, fn, k, cn)
    tree.train ()
    banner ("Orignal Tree: entropy = " + tree.calcEntropy ())
    tree.printTree ()

    tree.prune (2)
    banner ("Pruned Tree: entropy = " + tree.calcEntropy ())
    tree.printTree ()

} // DecisionTree2ID3wpTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3wpTest2` object is used to test the `DecisionTreeID3wp` class.
 *  > runMain scalation.analytics.classifier.DecisionTree2ID3wpTest2
 */
object DecisionTree2ID3wpTest2 extends App
{
    import ClassifierInt.pullResponse

    val fname = BASE_DIR + "breast_cancer.csv"
    val xy    = MatrixI (fname)
    val fn    = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                       "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val cn    = Array ("benign", "malignant")
    val k     = cn.size

    banner ("create, train and print a ID3 decision tree")
    println (s"dataset xy: ${xy.dim1}-by-${xy.dim2} matrix")
    val (x, y) = pullResponse (xy)
    val ymin   = y.min ()
    println (s"unadjusted ymin = $ymin")
    if (ymin != 0) y -= ymin
    val tree = new DecisionTree2ID3wp (x, y, fn, k, cn)
    tree.train ()
    tree.printTree ()
    tree.prune ()
    tree.printTree ()

} // DecisionTree2ID3wpTest2 object

