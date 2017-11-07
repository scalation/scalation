
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.4
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IsomorphismChecker` trait check the validity of the isomorphic matches
 *  between the query graph 'q' and subgraphs of the data graph 'g'.
 */
trait IsomorphismChecker
{ 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the set of bijections indicating the isomorphic matches.
     *  @param psi  the set of bijections to check
     *  @param g    the data graph  G(V, E, l)
     *  @param q    the query graph Q(U, D, k)
     */
    def checkIsomorphisms (bijections: Set [Array [Int]], q: Graph, g: Graph): Boolean =
    {
        for (psi <- bijections) {
            if (! checkLabels (psi, q, g)) return false
            if (! checkEdges (psi, q, g))  return false
        } // for
        true
    } // checkIsomorphisms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the set of bijections for matching semantics (labels).
     *  @param psi  the set of bijections to check
     *  @param g    the data graph  G(V, E, l)
     *  @param q    the query graph Q(U, D, k)
     */
    def checkLabels (psi: Array [Int], q: Graph, g: Graph): Boolean =
    {
        for (i <- q.ch.indices) if (q.label(i) != g.label(psi(i))) return false
        true
    } // checkLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the set of bijections for matching structure.
     *  @param psi  the set of bijections to check
     *  @param g    the data graph  G(V, E, l)
     *  @param q    the query graph Q(U, D, k)
     */
    def checkEdges (psi: Array [Int], q: Graph, g: Graph): Boolean =
    {
        for (u <- q.ch.indices; v <- q.ch(u) if ! g.ch(psi(u)).contains (psi(v))) return false
        true
    } // checkEdges

} // IsomorphismChecker trait

