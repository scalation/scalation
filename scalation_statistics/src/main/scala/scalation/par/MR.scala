
package scalation.par

import scala.collection.mutable.HashMap
//import scala.collection.mutable.{MutableList => List}
import scala.collection.immutable.List

class MR [Value]
{
    val document: String = """
A MapReduce program is composed of a Map() procedure (method) that performs filtering and sorting (such as sorting students by first name into queues, one queue for each name) and a Reduce() method that performs a summary operation (such as counting the number of students in each queue, yielding name frequencies). The "MapReduce System" (also called "infrastructure" or "framework") orchestrates the processing by marshalling the distributed servers, running the various tasks in parallel, managing all communications and data transfers between the various parts of the system, and providing for redundancy and fault tolerance.
"""

//    val map1 = new HashMap [String, List [String]] ()
//    val map2 = new HashMap [String, String] ()

    def map(): HashMap [String, List [Value]] =
    {
        val map1  = new HashMap [String, List [Value]] ()
        val words = document.split (" ")
        for (w <- words) {
            val list: List [Value] = map1.getOrElse (w, List [Value] ()) :+ "1".asInstanceOf [Value]
            map1 += w -> list
        } // for
        map1
    } // map

    def reduce (map1: HashMap [String, List [Value]]): HashMap [String, Value] =
    {
        val map2 = new HashMap [String, Value] ()
        for ((k, v) <- map1) map2 += k -> v.size.toString.asInstanceOf [Value]
        map2
    } // reduce

} // MR calss


// run-main scalation.par.MRTest

object MRTest extends App
{
    val mr = new MR [String] ()
    val mp1 = mr.map ()
    println (mp1)
    val mp2 = mr.reduce (mp1)
    println (mp2)
    
} // MRTest object

