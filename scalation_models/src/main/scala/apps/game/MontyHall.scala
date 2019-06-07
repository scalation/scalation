
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Aug 20 20:02:06 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package apps.game

import scalation.random.{Bernoulli, Randi}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MontyHall` object is used to test whether staying or switching picks when
 *  selecting a prize behind one of three doors in "Let's Make a Deal" is the better
 *  strategy.
 *  @see en.wikipedia.org/wiki/Monty_Hall_problem
 *  > runMain apps.game.MontyHall
 */
object MontyHall extends App
{
    val DEBUG     = false                                // debug flag
    val rng       = Randi (0, 2)                         // door selection (0, 1 or 2) random generator 
    val coin      = Bernoulli ()                         // coin flip generator
    val stream    = 0                                    // random number stream, try changing (upto 999)
    var winStay   = 0                                    // count wins with stay stategy
    var winSwitch = 0                                    // count wins with switch strategy
    val limit     = if (DEBUG) 100 else 100000

    for (it <- 1 to limit) {                             // test the stategies 100,000 times
        val car  = rng.igen                              // car randomly placed behind this door
        val pick = rng.igen                              // contestant randomly picks a door
        val show = (car, pick) match {                   // Monty Hall show other non-car door
            case (0, 1) | (1, 0) => 2                    
            case (0, 2) | (2, 0) => 1
            case (1, 2) | (2, 1) => 0
            case _               => (pick + 1 + coin.igen) % 3
        } // match
        if (DEBUG) println (s"car = $car, pick = $pick, show = $show")
       
        if (pick == car) winStay   += 1                  // stay with initial pick
        else             winSwitch += 1                  // switch to the other door 
    } // for

    println ("winStay   = " + winStay)
    println ("winSwitch = " + winSwitch)

} // MontyHall object

