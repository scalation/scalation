
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Thu Aug 25 13:58:23 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.game

import scalation.random.Randi
import scalation.util.Swap.swap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Cards` class is used to represent a deck of playing cards.
 */
class Cards
{
    private val NUM_CARDS = 52                               // number of cards in deck
    private val card      = Array.range (0, NUM_CARDS)       // the cards themseleves
    private val rn        = Randi (0, NUM_CARDS - 1)         // random number generator
    private var top       = 0                                // index of top card
    private val suit      = Array ('S', 'H', 'C', 'D')       // Spades, Hearts, Clubs, Diamonds

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Draw the top card from the deck and return it.  Return -1 if no cards are
     *  left in the deck.
     */
    def draw (): Int = if (top == NUM_CARDS) -1 else { val c = card(top); top += 1; c }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shuffle the deck of cards.
     */
    def shuffle ()
    {
        for (i <- card.indices) swap (card, i, rn.igen)
        top = 0
    } // shuffle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the card number 'c' (0 to 51) to the face value (1 to 13) and
     *  suit (0(S), 1(H), 2(C), 3(D)).
     *  @param c  the card number
     */
    def value (c : Int): Tuple2 [Int, Char] = (c % 13 + 1, suit (c / 13))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the card deck to a string.
     */
    override def toString = "Cards ( " + (for (c <- card) yield value (c)).deep + " )"

} // Cards class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Cards` object is used to test the `Cards` class.
 */
object Cards extends App
{
    val deck = new Cards ()
    println ("\nOrdered deck of cards:")
    println (deck)
    deck.shuffle ()
    println ("\nShuffled deck of cards:")
    println (deck)
    val hand = for (i <- 1 to 5) yield deck.value (deck.draw ())
    println ("\n5 card hand = " + hand)

} // Cards object

