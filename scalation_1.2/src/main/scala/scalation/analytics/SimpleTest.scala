
package scalation.analytics

import scalation.linalgebra.{VectoD, VectorD}

class SimpleTest [VecT <: VectoD] (y: VecT)
{
    def sq: VectoD = y * y

} // SimpleTest class

// run-main scalation.analytics.SimpleTest

object SimpleTest extends App
{
    val x = VectorD (2.0, 3.0)
    val s = new SimpleTest (x)
    println ("sq (x) = " + s.sq)

} // SimpleTest object

