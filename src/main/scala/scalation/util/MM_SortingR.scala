
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @builder scalation.util.bld.BldMM_Sorting
 *  @version 1.2
 *  @date    Sat Sep 26 20:25:19 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import scala.util.Random
import scalation.math.Real
import scalation.math.Real._0

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_SortingR` class provides direct and indirect methods to:
 *  <p>
 *      find 'k'-th median ('k'-th smallest element) using QuickSelect 
 *      sort large arrays using QuickSort
 *      sort small arrays using SelectionSort
 *  <p>
 *  Direct methods are faster, but modify the array, while indirect methods are
 *  slower, but do not modify the array.  This class is specialized for Real.
 *  @see `Sorting` for a generic version of this class.
 *  @param a  the array to operate on
 */
class MM_SortingR (a: MM_ArrayR)
{
    private val SHOW_REVERSALS = false            // flag for 'isSorted' method

    private val n = a.length                      // length of array a

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Direct Median and Sorting
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'k'-median of the 'p' to 'r' partition of array 'a' using
     *  the QuickSelect algorithm.
     *  @see http://en.wikipedia.org/wiki/Quickselect
     *  @param p  the left cursor
     *  @param r  the right cursor
     *  @param k  the type of median (k-th smallest element)
     */
    def median (p: Int, r: Int, k: Int): Real =
    {
        if (p == r) return a(p)
        swap (r, med3 (p, (p+r)/2, r))            // use median-of-3, comment out for simple pivot
        val q = partition (p, r)                  // partition into left (<=) and right (>=)
        if (q == k-1)     return a(q)             // found k-median
        else if (q > k-1) median (p, q - 1, k)    // recursively find median in left partition
        else              median (q + 1, r, k)    // recursively find median in right partition
    } // median

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'k'-median ('k'-th smallest element) of array 'a'.
     *  @param k  the type of median (e.g., k = (n+1)/2 is the median)
     */
    def median (k: Int = (n+1)/2): Real = median (0, n-1, k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Partition the array from 'p' to 'q' into a left partition (<= 'x') and
     *  a right partition (>= 'x').
     *  @param p  the left cursor
     *  @param q  the right cursor
     */
    def partition (p: Int, r: Int): Int =
    {
        val x = a(r)                            // pivot
        var i = p - 1
        for (j <- p until r if a(j) <= x) { i += 1; swap (i, j) }
        swap (i + 1, r)
        i + 1
    } // partition 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively sort the 'p' to 'r' partition of array 'a' using QuickSort.
     *  @see http://mitpress.mit.edu/books/introduction-algorithms
     *  @param p  the left cursor
     *  @param r  the right cursor
     */
    def qsort (p: Int, r: Int)
    {
        if (r - p > 5) {
            swap (r, med3 (p, (p+r)/2, r))      // use median-of-3, comment out for simple pivot
            val q = partition (p, r)            // partition into left (<=) and right (>=)
            qsort (p, q - 1)                    // recursively sort left partition
            qsort (q + 1, r)                    // recursively sort right partition
        } else {
            selsort (p, r)                      // use simple sort when small
        } // if
    } // qsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort array 'a' using QuickSort.
     */
    def qsort () { qsort (0, n-1) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort the 'p' to 'r' partition of array 'a' using SelectionSort.
     *  @param p  the left cursor
     *  @param r  the right cursor
     */
    def selsort (p: Int = 0, r: Int = n-1)
    {
        for (i <- p until r) {
            var k = i
            for (j <- i+1 to r if a(j) < a(k)) k = j
            if (i != k) swap (i, k)
        } // for
    } // selsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap the elements at 'i' and 'j', i.e., a(i) <-> a(j).
     *  @param i  the first index position
     *  @param j  the second index position
     */
    @inline private def swap (i: Int, j: Int) { val t = a(i); a(i) = a(j); a(j) = t }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the median of three elements.
     *  @param i  element 1
     *  @param j  element 2
     *  @param k  element 3
     */
    @inline private def med3 (i: Int, j: Int, k: Int): Int =
    {
        if (a(i) < a(j))
            if (a(j) < a(k)) j else if (a(i) < a(k)) k else i
        else
            if (a(j) > a(k)) j else if (a(i) > a(k)) k else i
    } // med3

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the array 'a' is sorted in ascending order.
     */
    def isSorted: Boolean =
    {
        for (i <- 1 until n if a(i-1) > a(i)) {
            if (SHOW_REVERSALS) {
                println ("isSorted: failed @ (i-1, a) = " + (i-1, a(i-1)))
                println ("isSorted: failed @ (i, a)   = " + (i, a(i)))
            } // if
            return false
        } // for
        true
    } // isSorted


    // Directly sorting in decreasing order ----------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Partition the array from 'p' to 'q' into a left partition (<= 'x') and
     *  a right partition (>= 'x').
     *  For sorting in decreasing order.
     *  @param p  the left cursor
     *  @param q  the right cursor
     */
    def partition2 (p: Int, r: Int): Int =
    {
        val x = a(r)                            // pivot
        var i = p - 1
        for (j <- p until r if a(j) >= x) { i += 1; swap (i, j) }
        swap (i + 1, r)
        i + 1
    } // partition2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively sort the 'p' to 'r' partition of array 'a' using QuickSort.
     *  Sort in decreasing order.
     *  @see http://mitpress.mit.edu/books/introduction-algorithms
     *  @param p  the left cursor
     *  @param r  the right cursor
     */
    def qsort2 (p: Int, r: Int)
    {
        if (r - p > 5) {
            swap (r, med3 (p, (p+r)/2, r))      // use median-of-3, comment out for simple pivot
            val q = partition2 (p, r)           // partition into left (<=) and right (>=)
            qsort2 (p, q - 1)                   // recursively sort left partition
            qsort2 (q + 1, r)                   // recursively sort right partition
        } else {
            selsort2 (p, r)                     // use simple sort when small
        } // if
    } // qsort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort array 'a' using QuickSort.  Sort in decreasing order.
     */
    def qsort2 () { qsort2 (0, n-1) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort the 'p' to 'r' partition of array 'a' using SelectionSort.
     *  Sort in decreasing order.
     *  @param p  the left cursor
     *  @param r  the right cursor
     */
    def selsort2 (p: Int = 0, r: Int = n-1)
    {
        for (i <- p until r) {
            var k = i
            for (j <- i+1 to r if a(j) > a(k)) k = j
            if (i != k) swap (i, k)
        } // for
    } // selsort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the array 'a' is sorted in descending order.
     */
    def isSorted2: Boolean =
    {
        for (i <- 1 until n if a(i-1) < a(i)) {
            println ("isSorted2: failed @ (i-1, a) = " + (i-1, a(i-1)))
            println ("isSorted2: failed @ (i, a)   = " + (i, a(i)))
            return false
        } // for
        true
    } // isSorted2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Indirect Median and Sorting
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly find the 'k'-median of the 'p' to 'r' partition of array 'a'
     *  using the QuickSelect algorithm.
     *  @see http://en.wikipedia.org/wiki/Quickselect
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     *  @param k   the type of median (k-th smallest element)
     */
    def imedian (rk: Array [Int], p: Int, r: Int, k: Int): Real =
    {
        if (p == r) return a(rk(p))
        iswap (rk, r, med3 (p, (p+r)/2, r))           // use median-of-3, comment out for simple pivot, ?imed3
        val q = ipartition (rk, p, r)                 // partition into left (<=) and right (>=)
        if (q == k-1)     return a(rk(q))             // found k-median
        else if (q > k-1) imedian (rk, p, q - 1, k)   // recursively find median in left partition
        else              imedian (rk, q + 1, r, k)   // recursively find median in right partition
    } // imedian

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly find the 'k'-median ('k'-th smallest element) of array 'a'.
     *  @param k  the type of median (e.g., k = (n+1)/2 is the median)
     */
    def imedian (k: Int = (n+1)/2): Real =
    {
        val rk = Array.range (0, n)                   // rank order
        imedian (rk, 0, n-1, k)
    } // imedian

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly partition the array from 'p' to 'r' into a left partition
     *  (<= 'x') and a right partition (>= 'x').
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     */
    def ipartition (rk: Array [Int], p: Int, r: Int): Int =
    {
        val x = a(rk(r))                              // pivot
        var i = p - 1
        for (j <- p until r if a(rk(j)) <= x) { i += 1; iswap (rk, i, j) }
        iswap (rk, i + 1, r)
        i + 1
    } // ipartition

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively and indirectly sort the 'p' to 'r' partition of array 'a'
     *  using  QuickSort.
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     */
    def iqsort (rk: Array [Int], p: Int, r: Int)
    {
        if (r - p > 5) {
            iswap (rk, r, med3 (p, (p+r)/2, r))       // use median-of-3, comment out for simple pivot, ?imed3
            val q = ipartition (rk, p, r)             // partition into left (<=) and right (>=)
            iqsort (rk, p, q - 1)                     // recursively sort left partition
            iqsort (rk, q + 1, r)                     // recursively sort right partition
        } else {
            iselsort (rk, p, r)                       // use simple sort when small
        } // if
    } // iqsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort array 'a' using QuickSort, returning the rank order.
     */
    def iqsort (): Array [Int] = 
    {
        val rk = Array.range (0, n)                   // rank order
        iqsort (rk, 0, n-1)                           // re-order rank
        rk                                            // return rank
    } // iqsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort the 'p' to 'r' partition of array 'a' using SelectionSort.
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     */
    def iselsort (rk: Array [Int], p: Int, r: Int)
    {
        for (i <- p to r) {
            var k = i
            for (j <- i+1 to r if a(rk(j)) < a(rk(k))) k = j
            if (i != k) iswap (rk, i, k)
        } // for
    } // iselsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort array 'a' using SelectionSort, returning the rank order.
     */
    def iselsort (): Array [Int] =
    {
        val rk = Array.range (0, n)                   // rank order
        iselsort (rk, 0, n-1)                         // re-order rank
        rk                                            // return rank
    } // iselsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly swap the elements at 'i' and 'j', i.e., rk(i) <-> rk(j).
     *  @param rk  the rank order
     *  @param i   the first index position
     *  @param j   the second index position
     */
    @inline private def iswap (rk: Array [Int], i: Int, j: Int)
    {
        val t = rk(i); rk(i) = rk(j); rk(j) = t
    } // iswap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the indirect index of the median of three elements.
     *  @param rk  the rank order
     *  @param i   element 1
     *  @param j   element 2
     *  @param k   element 3
     */
    @inline private def imed3 (rk: Array [Int], i: Int, j: Int, k: Int): Int =
    {
        if (a(rk(i)) < a(rk(j)))
            if (a(rk(j)) < a(rk(k))) j else if (a(rk(i)) < a(rk(k))) k else i
        else
            if (a(rk(j)) > a(rk(k))) j else if (a(rk(i)) > a(rk(k))) k else i
    } // imed3

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the array 'a' is indirectly sorted in ascending order.
     *  @param rk   the rank order
     */
    def isiSorted (rk: Array [Int]): Boolean =
    {
        for (i <- 1 until n if a(rk(i-1)) > a(rk(i))) {
            println ("isiSorted: failed @ (i-1, rk, a) = " + (i-1, rk(i-1), a(rk(i-1))))
            println ("isiSorted: failed @ (i,   rk, a) = " + (i, rk(i), a(rk(i))))
            return false
        } // for
        true
    } // isiSorted

    // Indirectly sorting in decreasing order --------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort array 'a' using QuickSort.
     *  Sort in decreasing order.
     */
    def iqsort2 (): Array [Int] =
    {
        val rk = Array.range (0, n)                             // rank order
        println ("iqsort2 method not yet implemented")          // FIX
        rk
    } // iqsort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort the 'p' to 'r' partition of array 'a' using SelectionSort.
     *  Sort in decreasing order.
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     */
    def iselsort2 (rk: Array [Int], p: Int, r: Int)
    {
        for (i <- p to r) {
            var k = i
            for (j <- i+1 to r if a(rk(j)) > a(rk(k))) k = j
            if (i != k) iswap (rk, i, k)
        } // for
    } // iselsort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort array 'a' using SelectionSort, returning the rank order.
     *  Sort in decreasing order.
     */
    def iselsort2 (): Array [Int] =
    {
        val rk = Array.range (0, n)                   // rank order
        iselsort2 (rk, 0, n-1)                        // re-order rank
        rk                                            // return rank
    } // iselsort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the array 'a' is indirectly sorted in ascending order.
     *  @param rk   the rank order
     */
    def isiSorted2 (rk: Array [Int]): Boolean =
    {
        for (i <- 1 until n if a(rk(i-1)) < a(rk(i))) {
            println ("isiSorted2: failed @ (i-1, rk, a) = " + (i-1, rk(i-1), a(rk(i-1))))
            println ("isiSorted2: failed @ (i,   rk, a) = " + (i, rk(i), a(rk(i))))
            return false
        } // for
        true
    } // isiSorted2

} // MM_SortingR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_SortingR` companion object provides shortcuts for calling methods from
 *  the `MM_SortingR` class.
 */
object MM_SortingR
{
    // Direct median and sorting --------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the median value in the array.
     *  @param a  the array to be examined
     */
    def median (a: MM_ArrayR, k: Int): Real = (new MM_SortingR (a)).median (k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fast, ascending, unstable sort.
     *  @param a  the array to be sorted
     */
    def qsort (a: MM_ArrayR) { (new MM_SortingR (a)).qsort () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slow, ascending, stable sort.
     *  @param a  the array to be sorted
     */
    def selsort (a: MM_ArrayR) { (new MM_SortingR (a)).selsort () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fast, descending, unstable sort.
     *  @param a  the array to be sorted
     */
    def qsort2 (a: MM_ArrayR) { (new MM_SortingR (a)).qsort2 () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slow, descending, stable sort.
     *  @param a  the array to be sorted
     */
    def selsort2 (a: MM_ArrayR) { (new MM_SortingR (a)).selsort2 () }

    // Indirect median and sorting -------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly find the median value in the array.
     *  @param a  the array to be examined
     */
    def imedian (a: MM_ArrayR, k: Int): Real = (new MM_SortingR (a)).imedian (k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fast, ascending, unstable indirect sort.
     *  @param a  the array to be sorted
     */
    def iqsort (a: MM_ArrayR): Array [Int] = (new MM_SortingR (a)).iqsort ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slow, ascending, stable indirect sort.
     *  @param a  the array to be sorted
     */
    def iselsort (a: MM_ArrayR): Array [Int] = (new MM_SortingR (a)).iselsort ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fast, descending, unstable indirect sort.
     *  @param a  the array to be sorted
     */
//  def iqsort2 (a: MM_ArrayR) { (new MM_SortingR (a)).iqsort2 () }   // FIX: implement

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slow, descending, stable indirect sort.
     *  @param a  the array to be sorted
     */
    def iselsort2 (a: MM_ArrayR): Array [Int] = (new MM_SortingR (a)).iselsort2 ()

} // MM_SortingR


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_SortingRTest` object is used to test the correctness and performance
 *  of the 'median' and 'imedian' methods in the `MM_SortingR` class.
 */
object MM_SortingRTest extends App
{
    var md = _0
    val rn = new Random ()
    val n  = 1000000
    val a  = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    val aa = MM_ArrayR.ofDim (n)

    // test direct k-medians (will modify the data array)

    println ("--------------------------------------------------------------")
    println ("Test direct: a = " + a.deep)
    for (k <- 1 to 5) {
        val med = new MM_SortingR (MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5))
        println ("median (" + k + ") = " + med.median (k))
    } // for
    val med = new MM_SortingR (MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5))
    println ("median ()  = " + med.median ())

    // test indirect k-medians (will not modify the data array)

    println ("--------------------------------------------------------------")
    println ("Test indirect: a = " + a.deep)
    val imed = new MM_SortingR (a)
    for (k <- 1 to 5) {
        println ("imedian (" + k + ") = " + imed.imedian (k))
    } // for
    println ("imedian ()  = " + imed.imedian ())
    println ("Unmodified: a = " + a.deep)

    // test the performance of direct k-medians

    println ("--------------------------------------------------------------")
    println ("Performance Test direct: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val med = new MM_SortingR (aa)
        print ("median:    "); time { md = med.median () }
        println ("median = " + md)
    } // for

    // test the performance of indirect k-medians

    println ("--------------------------------------------------------------")
    println ("Performance Test indirect: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val imed = new MM_SortingR (aa)
        print ("imedian:    "); time { md = imed.imedian () }
        println ("median = " + md)
    } // for

} // MM_SortingRTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_SortingRTest2` object is used to test the correctness and performance
 *  of the 'qsort' and 'iqsort' sorting methods in the `MM_SortingR` class.
 */
object MM_SortingRTest2 extends App
{
    import scala.util.Sorting.quickSort

    var rk: Array [Int] = null                              // to hold rank order
    val n  = 1000000 
    val rn = new Random ()
    val a  = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    val aa = MM_ArrayR.ofDim (n) 

    // test direct sorting (will modify the data array)

    println ("--------------------------------------------------------------")
    val a1 = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    println ("Test direct: a1 = " + a1.deep)
    val srt = new MM_SortingR (a1)
    srt.qsort ()
    println ("qsort a1 = " + a1.deep)
    println ("isSorted = " + srt.isSorted)

    // test indirect sorting (will not modify the data array)

    println ("--------------------------------------------------------------")
    val a2 = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    println ("Test indirect: a2 = " + a2.deep)
    val isrt = new MM_SortingR (a2)
    rk = isrt.iqsort ()
    println ("iqsort rk = " + rk.deep)                      // rank order
    println ("isiSorted = " + isrt.isiSorted (rk))

    // test the performance of direct sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test direct: aa.length = " + aa.length)
    for (k <- 0 until 20) {
//      for (i <- 0 until n) aa(i) = rn.nextDouble ()
//      print ("quicksort:   "); time { quickSort (aa) }    // Scala's QuickSort
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val srt = new MM_SortingR (aa)
        print ("qsort:       "); time { srt.qsort () }
        println ("isSorted = " + srt.isSorted)
    } // for

    // test the performance of indirect sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test indirect: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val isrt = new MM_SortingR (aa)
        print ("iqsort:       "); time { rk = isrt.iqsort () }
        println ("isiSorted = " + isrt.isiSorted (rk))
    } // for

} // MM_SortingRTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_SortingRTest3` object is used to test the correctness and performance
 *  of the 'selsort' and 'iselsort' sorting methods in the `MM_SortingR` class.
 */
object MM_SortingRTest3 extends App
{
    import scala.util.Sorting.quickSort

    var rk: Array [Int] = null                              // to hold rank order
    val n  = 10000
    val rn = new Random ()
    val a  = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    val aa = MM_ArrayR.ofDim (n)

    // test direct sorting (will modify the data array)

    println ("--------------------------------------------------------------")
    val a1 = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    println ("Test direct: a1 = " + a1.deep)
    val srt = new MM_SortingR (a1)
    srt.selsort ()
    println ("selsort a1 = " + a1.deep)
    println ("isSorted   = " + srt.isSorted)

    // test indirect sorting (will not modify the data array)

    println ("--------------------------------------------------------------")
    val a2 = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    println ("Test indirect: a2 = " + a2.deep)
    val isrt = new MM_SortingR (a2)
    rk = isrt.iselsort ()
    println ("iselsort rk = " + rk.deep)                      // rank order
    println ("isiSorted   = " + isrt.isiSorted (rk))

    // test the performance of direct sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test direct: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val srt = new MM_SortingR (aa)
        print ("selsort:     "); time { srt.selsort () }
        println ("isSorted = " + srt.isSorted)
    } // for

    // test the performance of indirect sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test indirect: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val isrt = new MM_SortingR (aa)
        print ("iselsort:     "); time { rk = isrt.iselsort () }
        println ("isiSorted = " + isrt.isiSorted (rk))
    } // for

} // MM_SortingRTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_SortingRTest4` object is used to test the correctness and performance
 *  of the 'qsort2' and 'iqsort2' sorting methods in the `MM_SortingR` class.
 */
object MM_SortingRTest4 extends App
{
    import scala.util.Sorting.quickSort

    var rk: Array [Int] = null                              // to hold rank order
    val n  = 1000000
    val rn = new Random ()
    val a  = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    val aa = MM_ArrayR.ofDim (n)

    // test direct sorting (will modify the data array)

    println ("--------------------------------------------------------------")
    val a1 = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    println ("Test direct: a1 = " + a1.deep)
    val srt = new MM_SortingR (a1)
    srt.qsort2 ()
    println ("qsort2 a1 = " + a1.deep)
    println ("isSorted2 = " + srt.isSorted2)

    // test indirect sorting (will not modify the data array)

    println ("--------------------------------------------------------------")
    val a2 = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    println ("Test indirect: a2 = " + a2.deep)
    val isrt = new MM_SortingR (a2)
    rk = isrt.iqsort2 ()
    println ("iqsort2 rk = " + rk.deep)                      // rank order
    println ("isiSorted2 = " + isrt.isiSorted2 (rk))

    // test the performance of direct sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test direct: aa.length = " + aa.length)
    for (k <- 0 until 20) {
//      for (i <- 0 until n) aa(i) = rn.nextDouble ()
//      print ("quicksort:   "); time { quickSort (aa) }    // Scala's QuickSort
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val srt = new MM_SortingR (aa)
        print ("qsort2:       "); time { srt.qsort2 () }
        println ("isSorted2 = " + srt.isSorted2)
    } // for

    // test the performance of indirect sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test indirect: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val isrt = new MM_SortingR (aa)
        print ("iqsort2:       "); time { rk = isrt.iqsort2 () }
        println ("isiSorted2 = " + isrt.isiSorted2 (rk))
    } // for

} // MM_SortingRTest4Test

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_SortingRTest5` object is used to test the correctness and performance
 *  of the 'selsort2' and 'iselsort2' sorting methods in the `MM_SortingR` class.
 *  Thet sort in decreasing order.
 */
object MM_SortingRTest5 extends App
{
    import scala.util.Sorting.quickSort

    var rk: Array [Int] = null                              // to hold rank order
    val n  = 10000
    val rn = new Random ()
    val a  = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    val aa = MM_ArrayR.ofDim (n)

    // test direct sorting (will modify the data array)

    println ("--------------------------------------------------------------")
    val a1 = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    println ("Test direct: a1 = " + a1.deep)
    val srt = new MM_SortingR (a1)
    srt.selsort2 ()
    println ("selsort2 a1 = " + a1.deep)
    println ("isSorted2   = " + srt.isSorted2)

    // test indirect sorting (will not modify the data array)

    println ("--------------------------------------------------------------")
    val a2 = MM_ArrayR (9, 1, 8, 2, 7, 3, 6, 4, 5)
    println ("Test indirect: a2 = " + a2.deep)
    val isrt = new MM_SortingR (a2)
    rk = isrt.iselsort2 ()
    println ("iselsort2 rk = " + rk.deep)                      // rank order
    println ("isiSorted2   = " + isrt.isiSorted2 (rk))

    // test the performance of direct sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test direct: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val srt = new MM_SortingR (aa)
        print ("selsort2:     "); time { srt.selsort2 () }
        println ("isSorted2 = " + srt.isSorted2)
    } // for

    // test the performance of indirect sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test indirect: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val isrt = new MM_SortingR (aa)
        print ("iselsort2:     "); time { rk = isrt.iselsort2 () }
        println ("isiSorted2 = " + isrt.isiSorted2 (rk))
    } // for

} // MM_SortingRTest5 object

