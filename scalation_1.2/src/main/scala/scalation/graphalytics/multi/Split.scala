
package scalation.graphalytics.multi

import scala.collection.mutable.{Set => SET}

// run-main scalation.graphalytics.multi.Split
object Split extends App
{
    val g = ExampleMuGraphD.g1

    def split (v: SET [Int], el1: Double, el2: Double): (SET [Int], SET [Int]) =
    {
        (v.flatMap (g.children (_, el1)), v.flatMap (g.children (_, el2)))
    } // split

    def join (uv: (SET [Int], SET [Int])): SET [(Int, Int)] =
    {
        for (i <- uv._1; j <- uv._2) yield (i, j)
    } // join

    val x = split (g.labelMap (11.0), -1.0, -2.0)
    println (s"x = $x")
    val y = join (x)
    println (s"y = $y")

} // Split object

