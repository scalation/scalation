
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Thu May 15 13:17:16 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 * Implementation options: Scala Actors, Akka Actors, Scala Continuations or Java Threads
 * This one uses Scala Actors
 */

package scalation.process

import actors.Actor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Coroutine` class supports quasi-concurrent programming.  A coroutine
 *  runs/acts until it yields control to some other coroutine.  When resumed,
 *  a coroutines continues execution where it left off.
 */
abstract class Coroutine extends Actor
{
    private val DEBUG   = true           // debug flag
    private val RESUME  = "RESUME"       // resume message
    private var started = false          // whether this coroutine has started

    if (DEBUG) println ("wait to be started")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start this coroutine, i.e., invoke its 'act' method.
     */
    override def start (): Coroutine =
    {
        started = true
        super.start ()
        this
    } // start

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Yield control to the 'other' coroutine.
     *  @param other  the other coroutine to yield control to
     *  @param quit   whether to wait via a receive or quit via exit
     */
    def yyield (other: Coroutine, quit: Boolean = false)
    {
        if (other != null) {
            if (other.started) {
                if (DEBUG) println ("resume the other coroutine")
                other ! RESUME
            } else {
                if (DEBUG) println ("start the other coroutine")
                other.start ()
            } // if
        } // if

        if (quit) {
            exit ()
        } else {
            receive { case RESUME => }         // wait until resumed
        } // if
    } // yyield

} // Coroutine


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoroutineTest` object is used to test the `Coroutine` class.
 *  Should print:
 *    Cor1: phase 1
 *    Cor2: phase 1
 *    Cor1: phase 2
 *    Cor2: phase 2
 */
object CoroutineTest extends App
{
    class Cor1 extends Coroutine
    {
        def act ()
        {
            println ("Cor1: phase 1")
            yyield (cor2)
            println ("Cor1: phase 2")
            yyield (cor2, true)
        } // act
    } // Cor1

    class Cor2 extends Coroutine
    {
        def act ()
        {
            println ("Cor2: phase 1")
            yyield (cor1)
            println ("Cor2: phase 2")
            yyield (null, true)
        } // act
    } // Cor2

    val cor1 = new Cor1 ()
    val cor2 = new Cor2 ()

    println ("start coroutines")
    cor1.start ()                    // just start the first one

} // CoroutineTest

