
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.4
 *  @date    Sat Mar 21 20:34:23 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 * Coroutine implementation options: (1) Java Threads,
 * Prototypes for (2) Scala Actors, (3) Akka Actors, (4) Scala Continuations
 * This one uses Java Threads and a Cached Thread Pool
 */

package scalation.process

import java.util.concurrent.{Executors, ExecutorService, Future, Semaphore, ThreadPoolExecutor}

import scalation.util.{Error, Identifiable}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Coroutine` class supports (one-at-a-time) quasi-concurrent programming.
 *  A coroutine runs/acts until it yields control from 'this' to 'that' coroutine.
 *  When resumed, a coroutines continues its execution where it left off.
 */
abstract class Coroutine (label: String = "cor")
         extends Runnable with Error
{
    import Coroutine._

    private val _sema     = new Semaphore (0)      // waiting semaphore
    private var started   = false                  // whether this coroutine has started

    nCreated += 1
    private val id = label + "." + nCreated
    if (DEBUG) println ("constructor: " + id + " waits to be STARTed")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Coroutine counts.
     */
    def counts: Tuple3 [Int, Int, Int] = (nCreated, nStarted, nTerminated)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Thread's 'run' method delegates to the 'act' method.  Upon interruption
     *  the 'act' method is run again from the beginning.
     */
    def run ()
    {
        nStarted += 1
        try {
            act ()
        } catch { case ex: InterruptedException => 
            if (DEBUG) println ("run: INTERRUPTED coroutine " + id)
        } // try
        nTerminated +=1
        if (DEBUG) println ("run: TERMINATE coroutine " + id)
    } // run

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Actor model features the 'act' method, even though threads are used.
     *  This abstract method must be implemented in application models.
     */
    def act ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Yield control from 'this' to 'that' coroutine.
     *  @param that  the other coroutine to yield control to
     *  @param quit  whether 'this' coroutine is to terminate (true)
     *                                  or wait to be resumed (false)
     */
    def yyield (that: Coroutine, quit: Boolean = false)
    {
        if (that != null) {
            if (that.started) {
                if (DEBUG) println ("yyield: " + id + " RESUMEs that coroutine " + that.id)
                that.resume ()
            } else {
                if (DEBUG) println ("yyield: " + id + " STARTs that new coroutine " + that.id)
                that.start ()
            } // if
        } // if

        if (quit) {
            if (DEBUG) println ("yyield: " + id + " TERMINATEs")
            return
        } else {
            _sema.acquire ()             // wait until resumed
        } // if
    } // yyield

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start this coroutine, i.e., invoke its 'run' -> 'act' method. This
     *  function returns a future.
     */
    def start (): Future [_] =
    {
        started = true
        if (pool == null) flaw ("start", "the coroutine system must be started using Coroutine.startup; expect undefined behavior.")
        pool.submit (this)
    } // start

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Interrupt this waiting coroutine.
     */
    def interrupt ()
    {
        Thread.currentThread ().interrupt ()
    } // interrupt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Resume this coroutine.
     */
    private def resume ()
    {
        _sema.release ()
    } // resume

} // Coroutine class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Coroutine` companion object provides functions to start-up and shutdown
 *  the coroutine system as well as counters for the `Coroutine` class.
 */
object Coroutine extends Error
{
    private val DEBUG                 = false   // debug flag
    private val CORE_THREADS          = 0       // number of core threads
    private val SHUTDOWN_TIMEOUT      = 60      // shutdown timeout, in seconds
    private var pool: ExecutorService = null    // thread pool

    private var nCreated = 0                    // number of Coroutines created
    private var nStarted = 0                    // number of Coroutines started
    private var nTerminated = 0                 // number of Coroutines terminated

    startup ()                                  // automatic startup at program start

//  sys.addShutdownHook ({                      // automatic shutdown at program end
//      pool.shutdown ()
//      pool.shutdownNow ()
//  })

    private def threadPoolExecutor = pool.asInstanceOf [ThreadPoolExecutor]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start-up the coroutine system.  This function can also set the core 
     *  number of threads for the internal cached thread pool.
     *  @param nCoreThreads  the new core size 
     */
    private def startup (nCoreThreads: Int = CORE_THREADS)
    {
        if (pool == null) {
            pool = Executors.newCachedThreadPool ()
            if (nCoreThreads != CORE_THREADS) threadPoolExecutor.setCorePoolSize (nCoreThreads)
        } else {
            flaw ("startup", "coroutine system is already started")
        } // if
    } // startup

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shutdown the coroutine thread pool and return the largest number of threads
     *  that have ever simultaneously been in the pool.  Must be called at program
     *  end or application will hang.
     */
    def shutdown (): Int =
    {
        var lps = 0                             // largest pool size
        if (DEBUG) println ("Coroutine.shutdown")
        if (pool != null) {
            pool.shutdown ()                    // prevent new submissions to pool
            pool.shutdownNow ()                 // interrupt all threads remaining in pool
            lps  = threadPoolExecutor.getLargestPoolSize
            pool = null
        } else flaw ("shutdown", "coroutine system is already shutdown")
        lps
    } // shutdown

} // Coroutine companion object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoroutineTest` object is used to test the `Coroutine` class.
 *  Should print:
 *    `Cor1`: phase 1
 *    `Cor2`: phase 1
 *    `Cor1`: phase 2
 *    `Cor2`: phase 2
 */
object CoroutineTest extends App
{
    class Cor1 extends Coroutine
    {
        override def act ()
        {
            println ("Cor1: phase 1")
            yyield (cor2)
            println ("Cor1: phase 2")
            yyield (cor2, true)
        } // act
    } // Cor1

    class Cor2 extends Coroutine
    {
        override def act ()
        {
            println ("Cor2: phase 1")
            yyield (cor1)
            println ("Cor2: phase 2")
            yyield (null, true)
        } // act
    } // Cor2

//    Coroutine.startup ()

    val cor1 = new Cor1 ()
    val cor2 = new Cor2 ()

    println ("start coroutines")
    cor1.start ()

} // CoroutineTest

