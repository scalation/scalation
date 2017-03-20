
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Tue Jun 21 14:37:45 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Worker` class allows tasks to be executed in parallel.
 *  @param task  the task/block of code to be executed
 *  @param i     have the task work on the 'i'th segment of data
 */
class Worker (task: (Int) => Any, i: Int) extends Thread
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the `Thread`s 'run' method to execute the 'task' in parallel.
     */
    override def run () { task (i) }

} // Worker class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WorkerTest` object tests the `Worker` class vs. Scala's built-in '.par'
 *  on the matrix multiplication problem.
 *  > run-main scalation.frame.WorkerTest
 */
object WorkerTest extends App
{
    import scalation.linalgebra.MatrixD
    import scalation.random.RandomMatD
    import scalation.util.time

    val rmg = RandomMatD (1000, 1000)
    val a = rmg.gen
    val b = rmg.gen
    val bt = b.t
    val c = new MatrixD (a.dim1, b.dim2)
    var cc: MatrixD = null

    def times_mpmp                           // .par: parallel on both loops
    {
        val aa = a()
        for (i <- a.range1.par; j <- b.range2.par) {
            val a_i = aa(i); val bt_j = bt()(j)
            var sum = 0.0
            for (k <- a.range2) sum += a_i(k) * bt_j(k)
            c(i, j) = sum
        } // for
    } // times_mpmp
    
    def times_mpm                            // .par: parallel on outer loop
    {
        val aa = a()
        for (i <- a.range1.par; j <- b.range2) {
            val a_i = aa(i); val bt_j = bt()(j)
            var sum = 0.0
            for (k <- a.range2) sum += a_i(k) * bt_j(k)
            c(i, j) = sum
        } // for
    } // times_mpm
    
    def times_mmp                            // .par: parallel on inner loop
    {
        val aa = a()
        for (i <- a.range1; j <- b.range2.par) {
            val a_i = aa(i); val bt_j = bt()(j)
            var sum = 0.0
            for (k <- a.range2) sum += a_i(k) * bt_j(k)
            c(i, j) = sum
        } // for
    } // times_mmp
    
    def times_mv (i: Int)                    // worker task
    {
        val a_i = a(i)()
        for (j <- b.range2) {
            val bt_j = bt()(j)
            var sum = 0.0
            for (k <- b.range2) sum += a_i(k) * bt_j(k)
            c(i, j) = sum
        } // for
    } // times_mv

    for (it <- 0 to 5) {
        print ("1: "); time { cc = a * b }            // serial
        //println ("a * b = " + cc)

        print ("2: "); time { times_mpmp }
        //println ("a * b = " + c)

        print ("3: "); time { times_mpm }
        //println ("a * b = " + c)

        print ("4: "); time { times_mmp }
        //println ("a * b = " + c)

        print ("5: "); time { for (i <- 0 until a.dim1) (new Worker (times_mv, i)).start }
        //println ("a * b = " + c)
    } // for

} // WorkerTest object

