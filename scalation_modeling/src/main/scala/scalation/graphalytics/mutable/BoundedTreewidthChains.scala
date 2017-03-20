
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Liang Ding
 *  @version 1.3
 *  @date    Sat Nov 14 20:44:53 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Developed from pseudo-code in the following paper:
 *  "Learning Bounded Treewidth Bayesian Networks"
 *  @see pluto.huji.ac.il/~galelidan/papers/ElidanGouldJMLR.pdf
 */

//  U N D E R   D E V E L O P M E N T

package scalation.graphalytics.mutable

import scalation.graphalytics.Pair

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BoundedTreeWidthChains` object is used to find bounded tree width
 *  chains to add edges to a Bayesian Network graph.
 */
object BoundedTreeWidthChains
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Learn the optimal non-contaminating chain with respect to topological
     *  node ordering.  This corresponds to Algorithm 6 in the paper.
     *  @param g      the maximum scoring graph of bounded treewidth
     *  @param order  the topological ordering of vertices and blocks
     */
    def learnChain (g: MGraph [Double], order: (Array [Int], Array [Int])): Array [Pair] =
    {
        Array ((0, 1))                             // FIX - to be implemented
    } // learnChain

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update edges of 'mp' (M+) when adding (s → t) to 'g'.  This corresponds
     *  to Algorithm 3 in the paper.
     *  @param g   the maximum scoring graph of bounded treewidth
     *  @param mp  the triangulated moralized graph of g
     */
    def edgeUpdate (g: MGraph [Double], mp: MGraph [Double], e: Pair)
    {
                                            // FIX - to be implemented
    } // edgeUpdate

} // BoundedTreeWidthChains object

