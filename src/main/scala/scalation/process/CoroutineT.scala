
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Thu May 15 13:17:16 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 * Implementation options: Scala Actors, Akka Actors, Scala Continuations or Java Threads
 * This one uses Java Threads.
 */

package scalation.process

import java.util.concurrent.Semaphore

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoroutineT` class supports quasi-concurrent programming.  A coroutine
 *  runs/acts until it yields control to some other coroutine.  When resumed,
 *  a coroutines continues execution where it left off.
 */
abstract class CoroutineT extends Thread
{
    private val DEBUG   = true                 // debug flag
    private val _sema   = new Semaphore (0)    // waiting semaphore
    private var started = false                // whether this coroutine has started

    if (DEBUG) println ("wait to be started")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start this coroutine, i.e., invoke its 'run' method.
     */
    override def start () { started = true; super.start () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Resume this coroutine.
     */
    def rresume () { _sema.release () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Yield control to the 'other' coroutine.
     *  @param other  the other coroutine to yield control to
     *  @param quit   whether to wait via a receive or quit via exit
     */
    def yyield (other: CoroutineT, quit: Boolean = false)
    {
        if (other != null) {
            if (other.started) {
                if (DEBUG) println ("resume the other coroutine")
                other.rresume ()
            } else {
                if (DEBUG) println ("start the other coroutine")
                other.start ()
            } // if
        } // if

        if (quit) {
            return
        } else {
            _sema.acquire ()             // wait until resumed
        } // if
    } // yyield

} // CoroutineT


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoroutineTTest` object is used to test the `CoroutineT` class.
 *  Should print:
 *    Cor1: phase 1
 *    Cor2: phase 1
 *    Cor1: phase 2
 *    Cor2: phase 2
 */
object CoroutineTTest extends App
{
    class Cor1 extends CoroutineT
    {
        override def run ()
        {
            println ("Cor1: phase 1")
            yyield (cor2)
            println ("Cor1: phase 2")
            yyield (cor2, true)
        } // run
    } // Cor1

    class Cor2 extends CoroutineT
    {
        override def run ()
        {
            println ("Cor2: phase 1")
            yyield (cor1)
            println ("Cor2: phase 2")
            yyield (null, true)
        } // run
    } // Cor2

    val cor1 = new Cor1 ()
    val cor2 = new Cor2 ()

    println ("start coroutines")
    cor1.start ()

} // CoroutineTTest

