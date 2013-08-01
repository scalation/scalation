
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Thu Aug 25 13:58:23 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes Cards.scala
 *  @run     scala -cp ../../classes:classes game.Cards
 */

package game

import scalation.random.Randi
import scalation.util.Swap.swap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to represent a deck of cards.
 */
class Cards
{
    private val NUM_CARDS = 52
    private val card      = Array [Int] (NUM_CARDS)
    private val rn        = Randi (0, NUM_CARDS - 1)
    private var top       = -1
    private val suit      = Array ('S', 'H', 'C', 'D')
    
    for (i <- 0 until NUM_CARDS) card(i) = i

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Draw the top card from the deck.
     */
    def draw (): Int = 
    {
        top += 1
        if (top >= NUM_CARDS) -1 else card(top)
    } // draw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shuffle the deck of cards.
     */
    def shuffle ()
    {
        for (i <- 0 until NUM_CARDS) swap (card, i, rn.igen)
        top = -1
    } // shuffle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the card number (0 to 51) to the face value (1 to 13) and
     *  suit (0(S), 1(H), 2(C), 3(D)).
     *  @param i  the card number
     */
    def value (i : Int): Tuple2 [Int, Char] = (i % 13 + 1, suit (i / 13))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the card deck to a string.
     */
    override def toString = "Cards ( " + (for (c <- card) yield value (c)).deep + " )"

} // Cards class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Cards class.
 */
object Cards extends App
{
    val deck = new Cards ()
    println (deck)
    deck.shuffle ()
    println ("Shuffle the deck of cards")
    println (deck)
    val hand = for (i <- 1 to 5) yield deck.value (deck.draw ())
    println ("hand = " + hand)

} // Cards object

