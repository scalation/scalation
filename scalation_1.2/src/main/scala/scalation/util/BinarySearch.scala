
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Apr 28 13:55:48 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BinarySearch` object provides a method for binary search.
 */
object BinarySearch
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find a key in the array/vector using binary search and return the position
     *  of the key (-1, if not found) along with the number of probes required.
     *  Works (switch comments) for both mutable `Array`s and immutable `Vector`s.
     *  @param a    the sorted array/vector to be searched
     *  @param key  the key to find in 'a'
     */
//  def search [T <% Ordered [T]] (a: Vector [T], key: T): Tuple2 [Int, Int] =
    def search [T <% Ordered [T]] (a: Array [T], key: T): Tuple2 [Int, Int] =
    {
        var probes = 0                                 // count number of probes
        var min    = 0                                 // lowest possible index
        var max    = a.length - 1                      // highest possible index
        while (min <= max) {
            val m = (max + min) / 2                    // compute mid-point
            probes += 1                                // increment probes count
            if (a(m) == key) return (m, probes)        // found the key
            if (a(m) < key) min = m + 1                // a_m too low, search m+1 to max
            else            max = m - 1                // a_m too high, search min to m-1
        } // while
        (-1, probes)
    } // search

} // BinarySearch object

import scala.math.floor
import scalation.linalgebra.VectorD
import scalation.math.log2
import scalation.plot.Plot
import scalation.random.{Randi0, RandomVecI}
import BinarySearch.search

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BinarySearchTest` object performs performance testing on `BinarySearch`. 
 *  > run-main scalation.util.BinarySearchTest
 */
object BinarySearchTest extends App
{
    val max_it = 1000000               // max number of iterations for a given size
    val m      = 1000                  // 'k'th size
    val nvec   = new VectorD (m)       // size vector
    val pvec   = new VectorD (m)       // probes count vector
    val evec   = new VectorD (m)       // probes estimate vector
    val dvec   = new VectorD (m)       // probes estimate vector

    for (k <- 0 until m) {
        val n     = (k + 1) * 1000
        val rvg   = RandomVecI (n, 10*n, unique = false)
        val rng   = Randi0 (n-1)
        var total = 0
        val av    = rvg.igen; av.sort ()
//      val a     = av().toVector
        val a     = av().toArray

        for (it <- 0 until max_it) {
            val i   = rng.igen
            val key = a(i)
            val (pos, probes) = search (a, key)
            total += probes
//          println (s"key = $key, i = $i, search: pos = $pos, probes = $probes")
        } // for

        // estimate of the number probes for the expected case
        // @see Knuth, "The Art of Computer Programming," Vol. 3, p. 413
        val ln  = log2 (n)
        val fln = floor (ln)
        val estimate = ln - 1.04 + (fln + 2.0) / n.toDouble

        val avg_probes = total / max_it.toDouble
        nvec(k)  = k
        pvec(k)  = avg_probes
        evec(k)  = estimate
        val diff = estimate - avg_probes
        dvec(k)  = diff
        println (s"$k: estimate = $estimate, avg-probes = $avg_probes, diff = $diff")
   } // for
   println (s"mean diff = ${dvec.mean}")

   new Plot (nvec, pvec, evec)

} // BinarySearchTest object

