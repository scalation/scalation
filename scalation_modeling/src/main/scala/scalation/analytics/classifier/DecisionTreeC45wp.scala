
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Kevin Warrick, John Miller, Susan George
 *  @version 1.6
 *  @date    Wed Jan  9 15:07:13 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.collection.mutable.ArrayBuffer

import scalation.linalgebra.{MatriD, MatrixD, MatrixI, VectoI, VectorI}
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream
import scalation.util.banner

import ClassifierReal.makeIsCont
import Probability.{entropy, toProbability}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45` class implements a Decision Tree classifier using the
 *  C4.5 algorithm with pruning.  The classifier is trained using a data matrix 'x' and
 *  a classification vector 'y'.  Each data vector in the matrix is classified into
 *  one of 'k' classes numbered '0, ..., k-1'.  Each column in the matrix represents
 *  a feature (e.g., Humidity).  The 'vc' array gives the number of distinct values
 *  per feature (e.g., 2 for Humidity).
 *-----------------------------------------------------------------------------
 *  At node for feature 'x_f', create children for possible discrete values of 'x_f'
 *  (For continuous, pick a threshold to split into lower and higher values).  Upon
 *  splitting, some matrices need to be created for which 'x_f' column is removed and
 *  each child only contains rows for its given value of 'x_f'.
 *-----------------------------------------------------------------------------
 *  @param x       the data vectors stored as rows of a matrix
 *  @param y       the class array, where y_i = class for row i of the matrix x
 *  @param isCont  `Boolean` value to indicate whether according feature is continuous
 *  @param fn_     the names for all features/variables
 *  @param k       the number of classes
 *  @param cn_     the names for all classes
 *  @param vc      the value count array indicating number of distinct values per feature
 *  @param td      the maximum tree depth allowed (defaults to 0 => n, -1 => no depth constrint)
 */
class DecisionTreeC45wp (x: MatriD, y: VectoI, isCont_ : Array [Boolean],
                        fn_ : Strings = null, k: Int = 2, cn_ : Strings = null,
                        vc_ : Array [Int] = null, td_ : Int = -1)
      extends DecisionTreeC45 (x, y, isCont_, fn_, k, cn_, vc_, td_)
{
    private var optPruneEntropy   = Double.MaxValue              // variable to hold optimal entropy of node to prune
    private var toPruneNode: Node2 = null                         // stores the node to be pruned

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract column from matrix, filtering out (x-value, y-value) pairs that are not on path.
     *  @param f     the feature to consider (e.g., 2 (Humidity))
     *  @param path  the path -- FIX explain why it is needed to remove
     */
    protected def dataset (f: Int, path: List [(Int, Int)]): Array [(Double, Int)] =
    {
        val col = x.col(f)().zipWithIndex
        col.filter (t => path.forall (tt => x(t._2, tt._1) == tt._2)).map (t => (t._1, y(t._2))).toArray
    } // dataset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prune the tree.  If entropy of node considered for pruning < thresholdToPrune,
     *  then remove the node from the decision tree.
     *  @param threshold   user-specified threshold which controls pruning.
     *  @param fold        defines cross-validation folds
     */
    def prune (thresholdToPrune: Double, fold: Int = 5): DecisionTreeC45 =
    {
        println ("Pruning")
        val unprunedTree = this                                  // get instance of current tree which is an unpruned tree
        val prunedTree   = new DecisionTreeC45 (x, y, isCont_, fn_, 2, cn_, vc_, td_)
        prunedTree.root  = unprunedTree.root.copy (vc_)          // set root of pruned tree same as unpruned tree but a diffrent instance
        unprunedTree.leaves.copyToBuffer (prunedTree.leaves)
        performPruning (prunedTree, thresholdToPrune)

        println (unprunedTree.root)
        println (prunedTree.root)
        println ("Entropy of unpruned tree: " + Node2.calcEntropy (unprunedTree.leaves))
        println ("Entropy of pruned tree:   " + Node2.calcEntropy (prunedTree.leaves))

        compareModel (fold, thresholdToPrune)                           // compare pruned and unpruned tree using CV
        prunedTree
    } // prune
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prune the tree if entropy of the identified pruneNode is less than threshold
     *  @param prunedTree  tree to prune
     *  @param threshold   user-specified threshold which controls pruning
     */
    private def performPruning (prunedTree: DecisionTreeC45, thresholdToPrune: Double): Boolean =
    {
        var isPruned = false
        val ret = findNodeToPrune (prunedTree)                      // returns the node along with entropy difference
        if (ret._1 != null) {
            val nodeToPrune = ret._1.asInstanceOf [FeatureNode]     // node to prune
            val diffEntropy = ret._2                                // min entropy difference
            println ("Node identified to be pruned: " + nodeToPrune + " : " + diffEntropy)
            if (diffEntropy < thresholdToPrune) {                          // if entropy diffrence < threshold,remove the node from tree
                val dset = dataset (nodeToPrune.f, nodeToPrune.path)
                val m    = mode (dset.map (_._2))
                var pt   = getPrunedTree (prunedTree, prunedTree.root, nodeToPrune, m)    // get the pruned tree
                isPruned = true
                toPruneNode = null
                optPruneEntropy = Double.MaxValue
                if (! pt._2) {
                    println ("Entropy of prunedTree " + Node2.calcEntropy (prunedTree.leaves) +
                             " : " + prunedTree.leaves.size)
                    performPruning (prunedTree, thresholdToPrune)          // repeat this process until entropy of node > threshold
                } // if
            } // if
      } // if
      isPruned
    } // performPruning

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This method identifies the node with min entropy as node to prune.
     *  @param prunedTree  finds if any child of the current node has its c
     */
    private def findNodeToPrune (prunedTree: DecisionTreeC45): (Node2, Double) =
    {
        var tLeavesList = ArrayBuffer [LeafNode] ()
        prunedTree.leaves.copyToBuffer (tLeavesList)
        for (n <- prunedTree.leaves) {
            if (tLeavesList contains n) {
                val pnode = n.parent
                var isChildrenLeaf = checkIfChildrenLeaf (pnode)
                if (isChildrenLeaf) {
                    val sibling       = tLeavesList.filter (leaf => leaf.parent == pnode)
                    tLeavesList       = tLeavesList diff sibling
                    val parentEntropy = entropy (pnode.nu)                // calculate entropy of the parent node
                    val childEntropy  = Node2.calcEntropy (sibling)        // calculate entropy of all leaf nodes under the parent
                    val delta         = parentEntropy - childEntropy      // find difference between parent and children entropy
                    if (delta < optPruneEntropy) {                        // get the min entropy difference
                        optPruneEntropy = delta
                        toPruneNode     = pnode
                    } // if
                } else {
                    tLeavesList = tLeavesList.filter (leaf => leaf.parent != pnode)
                } // if
            } // if
        } // for
        (toPruneNode, optPruneEntropy)
    } // findNodeToPrune

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Checks if all the children of a feature node are instances of LeafNode.
     *  @param node  checks if all the children of this node are instances of LeafNode
     */
    private def checkIfChildrenLeaf (node: FeatureNode): Boolean =
    {
        var isChildrenLeaf = true
        var it = node.branch.valuesIterator
        it.foreach ((cNode) => if (! cNode.isInstanceOf [LeafNode]) isChildrenLeaf = false)
        isChildrenLeaf
    } // checkIfChildrenLeaf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This method returns the pruned tree by deleting the node identified to prune.
     *  @param tree     tree to prune
     *  @param curNode  current node
     *  @param delNode  node to remove
     *  @param m        most frequent classification of 'delNode'
     */
    private def getPrunedTree (tree: DecisionTreeC45, curNode: Node2, delNode: FeatureNode, m: Int):
           (DecisionTreeC45, Boolean) =
    {
        var isRoot     = false
        val prunedTree = tree
        val n          = curNode
        if (delNode.path.size > 0) {                                      // checks if the node to be pruned is root
            val parBranch = delNode.path(0)._2
            var parPath   = delNode.path.drop (1)
            if (n.isInstanceOf [FeatureNode]) {
                val parentNode = n.asInstanceOf [FeatureNode]
                if (parentNode.path equals parPath) {
                    convertFeature2Leaf (prunedTree, parentNode, parBranch, m)   // converts feature node to leaf node
                } else {
                    var it = parentNode.branch.valuesIterator
                    it.foreach ((cNode) => if (cNode.isInstanceOf [FeatureNode]) {
                        val temp = cNode.asInstanceOf [FeatureNode]
                        getPrunedTree (prunedTree, temp, delNode, m)
                    }) // foreach
                } // if
            } // if
        } else {
            println ("At Root level: cannot be further pruned")
            isRoot = true
        } // if
        (prunedTree, isRoot)
    } // getPrunedTree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a feature node to leaf node.
     *  @param tree    the tree to prune
     *  @param parent  the parent node
     *  @param branc   the index of child under parent node to be converted to leaf
     *  @param m       mode of child under index == branch
     */
    private def convertFeature2Leaf (tree: DecisionTreeC45, parent: FeatureNode, branc: Int, m: Int): LeafNode =
    {
        val fn = parent.branch (branc).asInstanceOf [FeatureNode]
        tree.leaves = tree.leaves.filterNot (_.parent == fn)        // remove leaf nodes under fn from leaves
        val ln = new LeafNode (m, fn.nu)
        ln.parent = parent
        parent.branch.put (branc, ln)                               // updates the branch with new leaf
        tree.leaves += ln                                           // add the new leaf node to leaves
        ln
    } // convertFeature2Leaf
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This method is used to compare pruned versus unpruned via cross validation.
     *  @param folds      specfies the fold required for CV
     *  @param threshold  specifies the user-defined threshold for pruning
     */
    def compareModel (folds: Int, threshold: Double) =
    {
        banner ("Compare Models")

        // for random cross validation
        var u_score     = new MatrixI (k, k)                                  // unpruned scores
        var p_score     = new MatrixI (k, k)                                  // pruned scores
        val permutedVec = PermutedVecI (VectorI.range (0, x.dim1), ranStream)
        val randOrder   = permutedVec.igen
        val itestA      = randOrder.split (folds)                             // array of testset indices

        for (it <- 0 until folds) {
            val itest = itestA(it)().toArray                                  // get test data
            val testX = x.selectRows (itest)
            val testY = y.select (itest)

            val itrain = Array.range (0, x.dim1) diff itest                   // get training data
            val trainX = x.selectRows (itrain)
            val trainY = y.select (itrain)

            val vcTemp    = (for (j <- 0 until trainX.dim2) yield trainX.col(j).max ().toInt + 1).toArray
            
            banner ("Model for fold = " + it)
            var uTree = new DecisionTreeC45 (trainX, trainY, isCont_, fn, 2, cn, vcTemp, td_)    // create an unpruned tree with (n-1) fold data
            uTree.train (itest)
            var yp = VectorI (for (i <- testX.range1) yield
                                  uTree.classify (testX(i))._1)               // test the unpruned tree with the remaining 1 fold data
            u_score += new ConfusionMat (testY, yp, k).confusion              // get the score metrics for unpruned tree

            var pTree = new DecisionTreeC45 (trainX, trainY, isCont_, fn, 2, cn, vcTemp, td_)    // create pruned tree with (n-1) fold data
            pTree.root = uTree.root.copy (vcTemp)
            uTree.leaves.copyToBuffer (pTree.leaves)
            performPruning (pTree, threshold)
            var yp1 = VectorI (for (i <- testX.range1) yield
                                   pTree.classify (testX(i))._1)              // test the pruned tree with the remaining 1 fold data
            p_score += new ConfusionMat(testY,yp1,k).confusion                // get the score metrics for pruned tree

            println ("Entropy Unpruned = " + Node2.calcEntropy (uTree.leaves) +
                     " Entropy Pruned = "  + Node2.calcEntropy (pTree.leaves))
        } // for

        u_score /= folds                                                      // average of unpruned scores
        p_score /= folds                                                      // average of pruned scores

        println ("Unpruned tree: \t TN = " + u_score(0, 0) + " FP = " + u_score(0, 1) +
                                  " FN = " + u_score(1, 0) + " TP = " + u_score(1, 1))
        println ("Pruned tree:   \t TN = " + p_score(0, 0) + " FP = " + p_score(0, 1) +
                                  " FN = " + p_score(1, 0) + " TP = " + p_score(1, 1))
    } // compareModel

} // DecisionTreeC45wp class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45wpTest` object is used to test the `DecisionTreeC45wp` class.
 *  > runMain scalation.analytics.classifier.DecisionTreeC45wpTest
 */
object DecisionTreeC45wpTest extends App
{
    import ClassifierReal.pullResponse
  
    val fname = BASE_DIR + "breast_cancer.csv"
    val xy    = MatrixD (fname)
    val fn    = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                      "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val cn    = Array ("benign", "malignant")
    val isCon = makeIsCont (xy.dim2-1)                            // continuous column flag
    val k     = cn.size
    val vc    = (for (j <- 0 until xy.dim2-1) yield xy.col(j).max ().toInt + 1).toArray
//  val td = 5

    println (s"dataset xy: ${xy.dim1}-by-${xy.dim2} matrix")
    val (x, y) = pullResponse (xy)
    val ymin   = y.min ()
    println (s"unadjusted ymin = $ymin")
    if (ymin != 0) y -= ymin
    val tree   = new DecisionTreeC45wp (x, y, isCon, fn, k, cn, vc)
    tree.train ()
    tree.printTree (vc)
    tree.prune (0.98, 5)

} // DecisionTreeC45wpTest object

