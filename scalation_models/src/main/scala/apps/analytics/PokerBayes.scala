
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat Oct 19 21:00:32 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package apps.analytics

import scalation.analytics.classifier.BASE_DIR
import scalation.analytics.classifier.{AugNaiveBayes, NaiveBayes}
import scalation.linalgebra.{MatrixI, VectorI}
import scalation.util.{getFromURL_File, time}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PokerBayes` object is a sample application that uses the `NaiveBayes`
 *  and `AugNaiveBayes` classes.  Classify a poker hand consisting of 5 cards.
 *  The hand is to be classified as one of the following types of hands:
 *
 *    0: Nothing in hand; not a recognized poker hand 
 *    1: One pair; one pair of equal ranks within five cards
 *    2: Two pairs; two pairs of equal ranks within five cards
 *    3: Three of a kind; three equal ranks within five cards
 *    4: Straight; five cards, sequentially ranked with no gaps
 *    5: Flush; five cards with the same suit
 *    6: Full house; pair + different rank three of a kind
 *    7: Four of a kind; four equal ranks within five cards
 *    8: Straight flush; straight + flush
 *    9: Royal flush; {Ace, King, Queen, Jack, Ten} + flush
 *
 *  @see archive.ics.uci.edu/ml/machine-learning-databases/poker/poker-hand.names
 *  @see archive.ics.uci.edu/ml/datasets/Poker+Hand
 *  > run-main apps.analytics.PokerBayes
 */
object PokerBayes extends App
{
    val fname = BASE_DIR + "poker-hand-training-true.data"        // 25010 instances
//  val fname = getDataPath + "poker-hand-testing.data"           // 1000000 instances - download if needed

    val ROWS = 1000000                                            // number of instances
    val COLS = 11                                                 // 10 for x, 1 for y
    val k    = 10                                                 // 10 possible classes
    val xy   = new MatrixI (ROWS, COLS)                           // data table [x matrix, y vector]

    // read data from CSV file: each line becomes a vector in matrix xy
    var i = 0
    for (line <- getFromURL_File (fname)) { xy(i) = VectorI (line.split (',')); i += 1 }
    for (i <- 0 until xy.dim1; j <- 0 until xy.dim2 - 1) xy(i, j) -= 1           // start at 0 => shift by -1

//  println ("xy = " + xy)

    // names for all features/variables
    val fn = Array ("Card1S", "Card1R",                           // Suite and Rank of Card 1
                    "Card2S", "Card2R",                           // Suite and Rank of Card 2
                    "Card3S", "Card3R",                           // Suite and Rank of Card 3
                    "Card4S", "Card4R",                           // Suite and Rank of Card 4
                    "Card5S", "Card5R")                           // Suite and Rank of Card 5

    // names for all classes
    val cn = Array ("Nothing", "One pair",   "Two pairs",      "Three of a kind", "Straight",
                    "Flush",   "Full house", "Four of a kind", "Straight flush",  "Royal flush")

    val vc = VectorI (4, 13, 4, 13, 4, 13, 4, 13, 4, 13)          // value count: distinct values for each feature

    val nb  = NaiveBayes (xy, fn, k, cn, vc)                      // create a Naive Bayes classifier
    val anb = NaiveBayes (xy, fn, k, cn, vc)                      // create an Augmented Naive Bayes classifier
  
    time { nb.train () }                                          // train the classifier
    time { anb.train () }                                         // train the classifier

    val z = VectorI (3, 3, 2, 3, 0, 8, 3, 5, 2, 5)                // new data vector/hand to classify
    println (s"nb.classify ($z)  = ${nb.classify (z)}")           // answer = 2 (Two Pair)
    println (s"anb.classify ($z) = ${anb.classify (z)}")          // answer = 2 (Two Pair)

} // PokerBayes object

