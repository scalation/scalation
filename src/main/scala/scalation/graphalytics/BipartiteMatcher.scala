
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Ayushi Jain, John Miller
 *  @version 1.2
 *  @date    Thu Feb 3 2013
 *  @see     LICENSE (MIT style license file).
 */
package scalation.graphalytics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BipartiteMatcher` class provides an implementation of finding maximal
 *  Bipartite Matching.
 *  @see http://www.geeksforgeeks.org/maximum-bipartite-matching/
 *  @param  bpGraph  the bipartitte graph
 */
class BipartiteMatcher (bpGraph: Array [Array [Boolean]]) 
{
    private val M = bpGraph.length                     // the number of rows
    private val N = bpGraph (0).length                 // the number of columns
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** A DFS based recursive function that returns true if a matching for vertex
     *  u is possible.
     *  @param u       vertex u to be check, whether matching is possible of u to any unmatched v
     *  @param seen    array of vertex v, to keep track of whether v is already seen by some other u
     *  @param matchR  array which contain information of vertex v is assigned to vertex u
     */
    def bpm (u: Int, seen: Array [Boolean], matchR: Array [Int]): Boolean = 
    {
        for (v <- 0 until N) {                                         // try every job one by one
            if (bpGraph (u)(v) && ! seen (v)) {                        // if applicant u is interested in job v and v is not visited
                seen (v) = true                                        // mark v as visited
                // If job 'v' is not assigned to an applicant OR
                // previously assigned applicant for job v (which is matchR[v])
                // has an alternate job available.
                // Since v is marked as visited in the above line, matchR[v]
                // in the following recursive call will not get job 'v' again
                if (matchR (v) < 0 || bpm (matchR (v), seen, matchR)) {
                    matchR (v) = u 
                    return true
                } // if
            } // if
        } // for
        false
    } // bpm
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns maximum number of matching from M to N.
     *  The value of matchR(i) is the applicant number assigned to job i,
     *  the value -1 indicates nobody is assigned. 
     */
    def maxBPM (): Int = 
    {    
        val matchR = Array.fill (N) (-1)                // initially all jobs are available
        var result = 0                                  // count of jobs assigned to applicants
        for (u <- 0 until M) {
            var seen = Array.fill (N) (false)           // mark all jobs as not seen for next applicant.
            if (bpm (u, seen, matchR) ) result += 1     // find if the applicant 'u' can get a job
        } // for
        result
    } // maxBPM

} // BipartiteMatcher class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'BipartiteMatcherTest' object is used to test `BipartiteMatcher` class.
 */
object BipartiteMatcherTest extends App
{
    val bpGraph = Array (Array (false, true,  true,  false, false, false),
                         Array (true,  false, false, true,  false, false),
                         Array (false, false, true,  false, false, false),
                         Array (false, false, true,  true,  false, false),
                         Array (false, false, false, false, false, false),
                         Array (false, false, false, false, false, true))

    val bpMatch = new BipartiteMatcher (bpGraph)
    println ("Maximum number of applicants that can get job is " +  bpMatch.maxBPM () )

} // BipartiteMatcherTest

