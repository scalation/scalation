
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Thu May 15 13:17:16 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 * Implementation options: Scala Actors, Akka Actors, Scala Continuations or Java Threads
 * This one uses Akka Actors
 */

// U N D E R   D E V E L O P M E N T

package scalation.process

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

case object RESUME

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoroutineK` class supports quasi-concurrent programming.  A coroutine
 *  runs/acts until it yields control to some other coroutine.  When resumed,
 *  a coroutines continues execution where it left off.
 */
abstract class CoroutineK extends Actor
{
    private val DEBUG = true       // debug flag

    if (DEBUG) { println ("wait to be started"); Console.flush () }
    receive                        // wait to be started via first RESUME message  
    act ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Act/run quasi-concurrently.
     */
    def act ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Yield control to the 'other' coroutine.
     *  @param other  the other coroutine to yield control to
     *  @param quit   whether to wait via a receive or quit via exit
     */
    def yyield (other: ActorRef, quit: Boolean = false)
    {
        if (other != null) {
            if (DEBUG) { println ("resume the other coroutine"); Console.flush () }
            other ! RESUME
        } // if

        if (quit) {
            context.stop (self)
            if (other == null) context.system.shutdown ()
        } else {
            receive            // wait to be resumed by RESUME message
        } // if
    } // yyield

    def receive = { case RESUME => }

} // CoroutineK


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoroutineKTest` object is used to test the `CoroutineK` class.
 *  Should print:  FIX: execution order and completeness is indeterminate
 *    Cor1: phase 1
 *    Cor2: phase 1
 *    Cor1: phase 2
 *    Cor2: phase 2
 */
object CoroutineKTest extends App
{
    class Cor1 extends CoroutineK
    {
        def act ()
        {
            println ("Cor1: phase 1"); Console.flush ()
            yyield (cor2)
            println ("Cor1: phase 2"); Console.flush ()
            yyield (cor2, true)
        } // act
    } // Cor1

    class Cor2 extends CoroutineK
    {
        def act ()
        {
            println ("Cor2: phase 1"); Console.flush ()
            yyield (cor1)
            println ("Cor2: phase 2"); Console.flush ()
            yyield (null, true)
        } // act
    } // Cor2

    val asys = ActorSystem ()     // actor system

    val cor1 = asys.actorOf (Props [Cor1], "cor1")
    val cor2 = asys.actorOf (Props [Cor2], "cor2")

    println ("start coroutines"); Console.flush ()
//    cor1 ! RESUME

} // CoroutineKTest

