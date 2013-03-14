
import scalation.mathstat._

object BinomialTest extends Application
{
    val p = .7

    for (p <- .5 to .9 by .1) {

        println ("-------------------- p = " + p)

        val bin3 = Binomial (p, 3)
        println ("bin3: P(v >= 2) = " + (bin3.pf(2) + bin3.pf(3)))

        val bin5 = Binomial (p, 5)
        println ("bin5: P(v >= 3) = " + (bin5.pf(3) + bin5.pf(4) + bin5.pf(5)))

        val bin7 = Binomial (p, 7)
        println ("bin7: P(v >= 4) = " + (bin7.pf(4) + bin7.pf(5) + bin7.pf(6) + bin7.pf(7)))

    } // for

} // BinomialTest

