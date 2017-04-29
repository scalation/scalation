
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Mon Nov 23 15:38:54 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  "Drawing Rooted Trees in Linear Time"
 *  @see onlinelibrary.wiley.com/doi/10.1002/spe.713/pdf
 */

//  U N D E R   D E V E L O P M E N T

package scalation.graphalytics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeLayout` class is used to display multi-way trees with the root at the
 *  top and leaves at the bottom.  It is translated from the pseudo-code given in
 *  @see Appendix A of Christoph Buchheim, Michael Junger and Sebastian Leipert paper.
 *  @see onlinelibrary.wiley.com/doi/10.1002/spe.713/pdf
 *  @param t  the tree to be displayed
 */
class TreeLayout (t: Tree)
{
    private val n         = t.size
    private val x         = Array.ofDim [Int] (n)
    private val y         = Array.ofDim [Int] (n)
    private val mod       = Array.ofDim [Int] (n)
    private val thread    = Array.ofDim [Int] (n)
    private val prelim    = Array.ofDim [Int] (n)
    private val _ancestor = Array.ofDim [Int] (n)
    private val _change   = Array.ofDim [Int] (n)
    private val _shift    = Array.ofDim [Int] (n)
    private val number    = Array.ofDim [Int] (n)
    private var change    = 0
    private var shift     = 0
    private var distance  = 10
    private var defaultAncestor = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Layout the nodes in tree 't' creating 'x' and 'y' coordinates for each.
     */
    def layout (): (Array [Int], Array [Int]) =
    {
        for (v <- 0 until n) {                      // for all nodes v of t
            mod(v) = 0; thread(v) = 0
            _ancestor(v) = v
            val r = t.root.nid                      // the root of t
            firstWalk (r)
            println ("prelim = " + prelim.deep)
            secondWalk (r, -prelim(r))
        } // for
        (x, y)
    } // layout

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a preliminary x-coordinate for node 'v'.
     *  @param v  the current node id
     */
    def firstWalk (v: Int)
    {
        val vn = t(v)                                     // get the tree node with id v
        if (vn.isLeaf) {
            prelim(v) = 0
            val wn = vn.leftSibling
            if (wn != null) prelim(v) = prelim(wn.nid) + distance

        } else {
            val vn = t(v)
            defaultAncestor = vn.child(0).nid         // the leftmost child of v
            for (wn <- vn.child) {
                val w = wn.nid
                firstWalk (w)
                apportion (w)
            } // for
            executeShifts (v)
            val midpoint = (prelim(vn.child(0).nid) + prelim(vn.child(vn.child.size-1).nid)) / 2
            val wn = vn.rightSibling
            if (wn != null) {
                prelim(v) = prelim(wn.nid) + distance
                mod(v)    = prelim(v) - midpoint
            } else {
                prelim(v) = midpoint
            } // if
        } // if
    } // firstWalk

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apportion/combine new subtree with the previous subtrees.
     *  Note: i/o means inside/outside; p/m means +/- (right/left)
     *  @param v  the current node id
     */
    def apportion (v: Int)
    {
        var (vir, vor, vil, vol) = (0, 0, 0, 0)
        var (sir, sor, sil, sol) = (0, 0, 0, 0)

        val vn = t(v)                                     // get the tree node with id v
        val wn = vn.leftSibling
        if (wn != null) {
            vir = v
            vor = v
            vil = wn.nid
            vol = leftmostSibling (vir)         // the leftmost sibling of vir

            sir = mod(vir)
            sor = mod(vor)
            sil = mod(vil)
            sol = mod(vol)

            while (nextRight (vil) == 0 && nextLeft (vir) == 0) {
                vil = nextRight (vil)
                vir = nextLeft (vir)
                vol = nextLeft (vol)
                vor = nextRight (vor)
                _ancestor(vor) = v
                shift = (prelim(vil) + sil) - (prelim(vir) + sir) + distance
                if (shift > 0) {
                    moveSubtree (ancestor (vil, v), v, shift)
                    sir += shift
                    sor += shift
                } // if

                sil += mod(vil)
                sir += mod(vir)
                sol += mod(vol)
                sor += mod(vor)
            } // while
        } // if

        if (nextRight (vil) == 0 && nextRight (vor) == 0) {
            thread(vor) = nextRight( vil)
            mod(vor)    = mod(vor) + sil - sor
        } // if
        if (nextLeft (vir) == 0 && nextLeft (vol) == 0) {
            thread(vol)     = nextLeft (vir)
            mod(vol)       += sir - sol
            defaultAncestor = v
        } // if
    } // apportion

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the leftmost sibling of 'v'.
     *  @param v  the current node id
     */
    def leftmostSibling (v: Int): Int =
    {
        val vn = t(v)
        if (vn.parent != null) vn.parent.child(0).nid else -1
    } // leftmostSibling

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the leftmost child of 'v', otherwise the thread of 'v'.
     *  @param v  the current node id
     */
    def nextLeft (v: Int): Int =
    {
        val vn = t(v)
        if (vn.child.size > 0) vn.child(0).nid
        else thread(v)
    } // nextLeft

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the rightmost child of 'v', otherwise the thread of 'v'.
     *  @param v  the current node id
     */
    def nextRight (v: Int): Int =
    {
        val vn = t(v)
        if (vn.child.size > 0) vn.child(vn.child.size-1).nid
        else thread(v)
    } // nextLeft

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the subtree rooted at 'wp'.
     *  @param wm     the left node id
     *  @param wp     the right node id
     *  @param shift  the amount of shift in x-coordinate
     */
    def moveSubtree (wm: Int, wp: Int, shift: Int)
    {
        val subtrees = 1                       // FIX: number(wp) - number(wm)
        _change(wp) -= shift/subtrees
        _shift(wp)  += shift
        _change(wm) += shift/subtrees
        prelim(wp)  += shift
        mod(wp)     += shift
    } // moveSubtree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Executed the shifts computed in 'moveSubtree'.
     *  @param v  the current node id
     */
    def executeShifts (v: Int)
    {
        shift  = 0
        change = 0
        val vn = t(v)
        for (wn <- vn.child.reverse) {               // children from right to left
            val w = wn.nid
            prelim(w) += shift
            mod(w)    += shift
            change    += _change(w)
            shift     += _shift(w) + change
        } // for
    } // executeShifts

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return ancestor of 'vim' or default ancestor.
     *  @param vim  the inside left node id
     *  @param v    the current node id
     */
    def ancestor (vim: Int, v: Int): Int =
    {
        val v_anc = _ancestor(vim)
        if (t(v_anc).parent == t(v).parent) v_anc else defaultAncestor
    } // ancestor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign 'x' and 'y' coordinates for each node.
     *  @param v  the current node id
     *  @param m  the x position modifier
     */
    def secondWalk (v: Int, m: Int)
    {
        val vn = t(v)
        x(v) = prelim(v) + m
        y(v) = vn.lev
        for (wn <- vn.child) secondWalk (wn.nid, m + mod(v))
    } // secondWalk

} // TreeLayout class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeLayoutTest` object is used to test the `TreeLayout` class.
 *  > run-main scalation.graphalytics.TreeLayoutTest
 */
object TreeLayoutTest extends App
{
   println ("--------------------------------------------------------------")

    val pred = Array (-1, 0, 0, 1, 1, 2, 2)
    val labl = Array (10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0)
    val tree = Tree (pred, labl, 3.0, "t")
    tree.printTree
    tree.aniTree ()

    val tl = new TreeLayout (tree)
    val (x, y) = tl.layout ()

    println ("-------------------------------------------------------------")
    println ("x = " + x.deep)
    println ("-------------------------------------------------------------")
    println ("y = " + y.deep)
    println ("-------------------------------------------------------------")
    
} // TreeLayoutTest object

