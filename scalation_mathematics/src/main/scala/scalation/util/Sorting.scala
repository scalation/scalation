
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat Oct 26 21:44:36 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import scala.math.min
import scala.reflect.ClassTag
import scala.util.Random

import scalation.math.log2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sorting` class provides direct and indirect methods to:
 *  <p>
 *      find 'k'-th median ('k'-th smallest element) using `QuickSelect` 
 *      sort large arrays using `QuickSort`
 *      sort large arrays using `MergeSort` (slower than quicksort, but stable)
 *      sort small arrays using `SelectionSort`
 *  <p>
 *  Direct methods are faster, but modify the array, while indirect methods
 *  are slower, but do not modify the array.  This class is generic.
 *  @see `SortingC` for a version of this class specialized for `Complex`.
 *  @see `SortingD` for a version of this class specialized for `Double`.
 *  @see `SortingI` for a version of this class specialized for `Int`.
 *  @see `SortingL` for a version of this class specialized for `Long`.
 *  @see `SortingQ` for a version of this class specialized for `Rational`.
 *  @see `SortingR` for a version of this class specialized for `Real`.
 *  @see `SortingS` for a version of this class specialized for `StrNum`.
 *  @param a  the array to operate on
 */
class Sorting [T <% Ordered[T]: ClassTag] (a: Array [T])
{
    private val n = a.length                             // length of array a

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Direct Median and Sorting
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'k'-median of the 'p' to 'r' partition of array 'a' using
     *  the `QuickSelect` algorithm.
     *  @see en.wikipedia.org/wiki/Quickselect
     *  @param p  the left cursor
     *  @param r  the right cursor
     *  @param k  the type of median (k-th smallest element)
     */
    def median (p: Int, r: Int, k: Int): T =
    {
        if (p == r) return a(p)
        swap (r, med3 (p, (p+r)/2, r))              // use median-of-3, comment out for simple pivot
        val q = partition (p, r)                    // partition into left (<=) and right (>=)
        if (q == k-1)     return a(q)               // found k-median
        else if (q > k-1) median (p, q - 1, k)      // recursively find median in left partition
        else              median (q + 1, r, k)      // recursively find median in right partition
    } // median

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'k'-median ('k'-th smallest element) of array 'a'.
     *  @param k  the type of median (e.g., k = (n+1)/2 is the median)
     */
    def median (k: Int = (n+1)/2): T = median (0, n-1, k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the smallest 'k' elements (top-k) of array 'a'.
     *  @param k  the number of samllest elements to return
     */
    def top (k: Int): Array [T] =
    {
        median (k)
        a.slice (0, k)
    } // top

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Partition the array from 'p' to 'q' into a left partition (<= 'x') and
     *  a right partition (>= 'x').
     *  @param p  the left cursor
     *  @param q  the right cursor
     */
    def partition (p: Int, r: Int): Int =
    {
        val x = a(r)                              // pivot
        var i = p - 1
        for (j <- p until r if a(j) <= x) { i += 1; swap (i, j) }
        swap (i + 1, r)
        i + 1
    } // partition 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively sort the 'p' to 'r' partition of array 'a' using `QuickSort`.
     *  @see http://mitpress.mit.edu/books/introduction-algorithms
     *  @param p  the left cursor
     *  @param r  the right cursor
     */
    def qsort (p: Int, r: Int)
    {
        if (r - p > 5) {
            swap (r, med3 (p, (p+r)/2, r))        // use median-of-3, comment out for simple pivot
            val q = partition (p, r)              // partition into left (<=) and right (>=)
            qsort (p, q - 1)                      // recursively sort left partition
            qsort (q + 1, r)                      // recursively sort right partition
        } else {
            selsort (p, r)                        // use simple sort when small
        } // if
    } // qsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort array 'a' using `QuickSort` (unstable sorting algorithm).
     */
    def qsort () { qsort (0, n-1) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort array 'a' using `MergeSort` (stable sorting algorithm).
     *  Uses a bottom-up approach.
     *  @see https://en.wikipedia.org/wiki/Merge_sort
     */
    def msort ()
    {
        val aa = Array (a, Array.ofDim [T] (n))             // merge back and forth (aa(0), aa(1))
        val ln = log2 (n).toInt + 2                         // number of merge passes
        var w  = 1                                          // width of run
        for (j <- 1 until ln) {
            val (from, to) = ((j+1) % 2, j % 2)
            for (i <- 0 until n by w+w) merge (aa(from), i, min (i+w, n), min (i+w+w, n), aa(to))
            w *= 2                                          // double the run length
        } // for
        if (ln % 2 == 0) Array.copy (aa(1), 0, a, 0, n)     // move into original array
    } // msort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Merge sorted runs of one array into another.
     *  Examaple: merge | 1, 3 | with | 2, 4 | to get | 1, 2, 3, 4 |.
     *  Run 1: x(ii until jj)
     *  Run 2: x(jj until kk)
     *  @param x   the source (from) array
     *  @param ii  the start of first run
     *  @param jj  the start of second run
     *  @param kk  the end (exclusive) of second run
     *  @param y   the destination (to) array
     */
    def merge (x: Array [T], ii: Int, jj: Int, kk: Int, y: Array [T])
    {
        var (i, j) = (ii, jj)
        for (k <- ii until kk) {
            if (i < jj && (j >= kk || x(i) <= x(j))) { y(k) = x(i); i += 1 }
            else                                     { y(k) = x(j); j += 1 }
        } // for
    } // merge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort the 'p' to 'r' partition of array 'a' using `SelectionSort`.
     *  @param p    the left cursor
     *  @param r    the right cursor
     */
    def selsort (p: Int, r: Int)
    {
        for (i <- p to r) {
            var k = i
            for (j <- i+1 to r if a(j) < a(k)) k = j
            if (i != k) swap (i, k)
        } // for
    } // selsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort array 'a' using `SelectionSort`.
     */
    def selsort () { selsort (0, n-1) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Partially sort array 'a' using `SelectionSort` returning the first/smallest
     *  'k' elements.  Can be used for top-k (smallest) or bottom-k (largest) selection.
     *  @param k    the number of elements to sort and return
     *  @param asc  whether to sort in ascending (true) or descending (false) order
     */
    def selsort (k: Int, asc: Boolean = true): Array [T] =
    { 
        for (i <- 0 until k) {
            var l = i
            if (asc) for (j <- i+1 until n if a(j) < a(l)) l = j
            else     for (j <- i+1 until n if a(j) > a(l)) l = j
            if (i != l) swap (i, l)
        } // for
        a.slice (0, k)
    } // selsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Indirect Median and Sorting
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly find the 'k'-median of the 'p' to 'r' partition of array 'a'
     *  using the `QuickSelect` algorithm.
     *  @see http://en.wikipedia.org/wiki/Quickselect
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     *  @param k   the type of median (k-th smallest element)
     */
    def median (rk: Array [Int], p: Int, r: Int, k: Int): T =
    {
        if (p == r) return a(rk(p))
        swap (rk, r, med3 (p, (p+r)/2, r))             // use median-of-3, comment out for simple pivot
        val q = partition (rk, p, r)                   // partition into left (<=) and right (>=)
        if (q == k-1)     return a(rk(q))              // found k-median
        else if (q > k-1) median (rk, p, q - 1, k)     // recursively find median in left partition
        else              median (rk, q + 1, r, k)     // recursively find median in right partition
    } // median

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly find the 'k'-median ('k'-th smallest element) of array 'a'.
     *  @param k  the type of median (e.g., k = (n+1)/2 is the median)
     */
    def imedian (k: Int = (n+1)/2): T =
    {
        val rk = Array.range (0, n)                    // rank order
        median (rk, 0, n-1, k)
    } // imedian

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly partition the array from 'p' to 'r' into a left partition
     *  (<= 'x') and a right partition (>= 'x').
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     */
    def partition (rk: Array [Int], p: Int, r: Int): Int =
    {
        val x = a(rk(r))                               // pivot
        var i = p - 1
        for (j <- p until r if a(rk(j)) <= x) { i += 1; swap (rk, i, j) }
        swap (rk, i + 1, r)
        i + 1
    } // partition

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively and indirectly sort the 'p' to 'r' partition of array 'a'
     *  using `QuickSort`.
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     */
    def qsort (rk: Array [Int], p: Int, r: Int)
    {
        if (r - p > 5) {
            swap (rk, r, med3 (p, (p+r)/2, r))         // use median-of-3, comment out for simple pivot
            val q = partition (rk, p, r)               // partition into left (<=) and right (>=)
            qsort (rk, p, q - 1)                       // recursively sort left partition
            qsort (rk, q + 1, r)                       // recursively sort right partition
        } else {
            selsort (rk, p, r)                         // use simple sort when small
        } // if
    } // qsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort array 'a' using `QuickSort`, returning the rank order.
     */
    def iqsort (): Array [Int] = 
    {
        val rk = Array.range (0, n)                    // rank order
        qsort (rk, 0, n-1)                             // re-order rank
        rk                                             // return rank
    } // iqsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort the 'p' to 'r' partition of array 'a' using `SelectionSort`.
     *  @param rk  the rank order
     *  @param p   the left cursor
     *  @param r   the right cursor
     */
    def selsort (rk: Array [Int], p: Int, r: Int)
    {
        for (i <- p to r) {
            var k = i
            for (j <- i+1 to r if a(rk(j)) < a(rk(k))) k = j
            if (i != k) swap (rk, i, k)
        } // for
    } // selsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly sort array 'a' using `SelectionSort`, returning the rank order.
     */
    def iselsort (): Array [Int] =
    {
        val rk = Array.range (0, n)                  // rank order
        selsort (rk, 0, n-1)                         // re-order rank
        rk                                           // return rank
    } // iselsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Partially, indirectly sort array 'a' using `SelectionSort` returning the
     *  first/smallest 'k' elements.  Can be used for top-k (smallest) or bottom-k
     *  (largest) selection.
     *  @param k    the number of elements to sort and return
     *  @param asc  whether to sort in ascending (true) or descending (false) order
     */
    def iselsort (k: Int, asc: Boolean = true): Array [Int] =
    {
        val rk = Array.range (0, n)                  // rank order
        for (i <- 0 until k) {
            var l = i
            if (asc) for (j <- i+1 until n if a(rk(j)) < a(rk(l))) l = j
            else     for (j <- i+1 until n if a(rk(j)) > a(rk(l))) l = j
            if (i != l) swap (rk, i, l)
        } // for
        rk.slice (0, k)                              // return rank
    } // iselsort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the array 'a' is sorted.
     */
    def isSorted: Boolean =
    {
        for (i <- 1 until n if a(i-1) > a(i)) {
            println ("isSorted: failed @ (i-1, a) = " + (i-1, a(i-1)))
            println ("isSorted: failed @ (i, a) = " + (i, a(i)))
            return false
        } // for
        true
    } // isSorted

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the array 'a' is indirectly sorted.
     *  @param rk  the rank order
     */
    def isSorted (rk: Array [Int]): Boolean =
    {
        for (i <- 1 until n if a(rk(i-1)) > a(rk(i))) {
            println ("isSorted: failed @ (i-1, rk, a) = " + (i-1, rk(i-1), a(rk(i-1))))
            println ("isSorted: failed @ (i,   rk, a) = " + (i, rk(i), a(rk(i))))
            return false
        } // for
        true
    } // isSorted

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap the elements at 'i' and 'j', i.e., a(i) <-> a(j).
     *  @param i  the first index position
     *  @param j  the second index position
     */
    @inline private def swap (i: Int, j: Int) { val t = a(i); a(i) = a(j); a(j) = t }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indirectly swap the elements at 'i' and 'j', i.e., 'rk(i)' <-> 'rk(j)'.
     *  @param rk  the rank order
     *  @param i   the first index position
     *  @param j   the second index position
     */
    @inline private def swap (rk: Array [Int], i: Int, j: Int)
    {
        val t = rk(i); rk(i) = rk(j); rk(j) = t
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the median of three elements.
     *  @param i  element 1
     *  @param j  element 2
     *  @param k  element 3
     */
    @inline private def med3 (i: Int, j: Int, k: Int): Int =
    {
        if (a(i) < a(j)) if (a(j) < a(k)) j else if (a(i) < a(k)) k else i
        else             if (a(j) > a(k)) j else if (a(i) > a(k)) k else i
    } // med3

} // Sorting class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sorting` companion object provides shortcuts for calling methods from
 *  the `Sorting` class.  Only works for `Array [Double]`.
 */
object Sorting
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'k'-median in array 'a'.
     *  @param a  the array to search
     *  @param k  specification for k-th smallest element
     */
    def median (a: Array [Double], k: Int): Double = (new Sorting (a)).median (k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Quicksort to sort the array 'a'.
     *  @param a  the array to sort
     */
    def qsort (a: Array [Double]) { (new Sorting (a)).qsort () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Selection Sort to sort the array 'a'.
     *  @param a  the array to sort
     */
    def selsort (a: Array [Double]) { (new Sorting (a)).iselsort () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'k'-median in array 'a', non-destructively.
     *  @param a  the array to search
     *  @param k  specification for k-th smallest element
     */
    def imedian (a: Array [Double], k: Int): Double = (new Sorting (a)).imedian (k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Quicksort to indirectly sort array 'a'.
     *  @param a  the array to indirectly sort
     */
    def iqsort (a: Array [Double]): Array [Int] = (new Sorting (a)).iqsort ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Selection Sort to indirectly sort array 'a'.
     *  @param a  the array to indirectly sort
     */
    def iselsort (a: Array [Double]): Array [Int] = (new Sorting (a)).iselsort ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the rank order for array 'a', return its elements in that order.
     *  The rank order may be established using indirect sorting.
     *  @param a  the source array
     */
    def reorder (a: Array [Double], rank: Array [Int]): Array [Double] =
    {
        val b = Array.ofDim [Double] (a.length)
        for (i <- b.indices) b(i) = a(rank(i))
        b
    } // reorder

} // Sorting


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SortingTest` object is used to test the correctness and performance
 *  of the methods in the `Sorting` class that find 'k'-medians.
 *  > runMain scalation.util.SortingTest
 */
object SortingTest extends App
{
    var md = 0.0
    val rn = new Random ()
    val n  = 1000000
    val a  = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0)
    val aa = Array.ofDim [Double] (n)

    // test direct k-medians (will modify the data array)

    println ("--------------------------------------------------------------")
    println ("Test direct: a = " + a.deep)
    for (k <- 1 to 5) {
        val med = new Sorting (Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0))
        println ("median (" + k + ") = " + med.median (k))
    } // for
    val med = new Sorting (Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0))
    println ("median ()  = " + med.median ())

    // test indirect k-medians (will not modify the data array)

    println ("--------------------------------------------------------------")
    println ("Test indirect: a = " + a.deep)
    val imed = new Sorting (a)
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
        val med = new Sorting (aa)
        print ("median:    "); time { md = med.median () }
        println ("median = " + md)
    } // for

    // test the performance of indirect k-medians

    println ("--------------------------------------------------------------")
    println ("Performance Test indirect: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val imed = new Sorting (aa)
        print ("imedian:    "); time { md = imed.imedian () }
        println ("median = " + md)
    } // for

} // SortingTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SortingTest2` object is used to test the correctness and performance
 *  of the sorting methods in the `Sorting` class.
 *  > runMain scalation.util.SortingTest2
 */
object SortingTest2 extends App
{
    import scala.util.Sorting.quickSort

    var rk: Array [Int] = null                              // to hold rank order
    val n  = 1000000 
    val rn = new Random ()
    val a  = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0)
    val aa = Array.ofDim [Double] (n) 

    // test direct sorting (will modify the data array)

    println ("--------------------------------------------------------------")
    val a1 = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0)
    println ("Test direct: a1 = " + a1.deep)
    val srt = new Sorting (a1)
    srt.qsort ()
    println ("qsort a1 = " + a1.deep)
    println ("isSorted = " + srt.isSorted)

    // test indirect sorting (will not modify the data array)

    println ("--------------------------------------------------------------")
    val a2 = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0)
    println ("Test indirect: a2 = " + a2.deep)
    val isrt = new Sorting (a2)
    rk = isrt.iqsort ()
    println ("iqsort rk = " + rk.deep)                      // rank order
    println ("isSorted =  " + isrt.isSorted (rk))

    // test the performance of direct sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test direct: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        print ("quicksort:   "); time { quickSort (aa) }    // Scala's `QuickSort`
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val srt = new Sorting (aa)
        print ("qsort:       "); time { srt.qsort () }
        println ("isSorted = " + srt.isSorted)
    } // for

    // test the performance of indirect sorting

    println ("--------------------------------------------------------------")
    println ("Performance Test indirect: aa.length = " + aa.length)
    for (k <- 0 until 20) {
        for (i <- 0 until n) aa(i) = rn.nextDouble ()
        val isrt = new Sorting (aa)
        print ("iqsort:      "); time { rk = isrt.iqsort () }
        println ("isSorted = " + isrt.isSorted (rk))
    } // for

} // SortingTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SortingTest3` object is used to test the correctness of top-k (smallest)
 *  and bottom-k (largest) algorithms that are based on partial selection sort.
 *  > runMain scalation.util.SortingTest3
 */
object SortingTest3 extends App
{
    // test top-k and bottom-k using partial selection sort and quick-select
    val k   = 3

    println ("--------------------------------------------------------------")
    val a1   = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0)
    println ("Test direct: a1 = " + a1.deep)
    val srt1 = new Sorting (a1)
    val b1   = srt1.selsort (k)                               // smallest, sorted k
    println (s"selsort ($k) = ${b1.deep}")                    // top-k

    val a2   = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0)
    val srt2 = new Sorting (a2)
    val b2   = srt2.selsort (k, false)                        // laregest, sorted k
    println (s"selsort ($k, false) = ${b2.deep}")             // bottom-k, selsort

    val a3   = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0)
    val srt3 = new Sorting (a3)
    val b3   = srt3.iselsort (k, false)
    println (s"iselsort ($k, false) = ${b3.deep}")            // indirect top-k

    val a4   = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0)
    val srt4 = new Sorting (a4)
    val b4   = srt2.top (k)
    println (s"top ($k) = ${b4.deep}")                        // top-k, quick-select

} // SortingTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SortingTest4` object is used to test Merge Sort.
 *  > runMain scalation.util.SortingTest4
 */
object SortingTest4 extends App
{
    // test Merge Sort

    println ("--------------------------------------------------------------")
    val a1   = Array (9.0, 1.0, 8.0, 2.0, 7.0, 3.0, 6.0, 4.0, 5.0, 10.0, 11.0, 3.0)
    println ("Test direct: a1 = " + a1.deep)
    val srt1 = new Sorting (a1)
    srt1.msort ()
    println (s"msort = ${a1.deep}")

} // SortingTest4 object

