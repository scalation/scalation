
import scalation.linalgebra.VectorD

object TestEquals extends App
{
    println ("test:")
    val a = Array (1, 2)
    val b = Array (1, 2)
    println ("a equals b " + (a equals b))
    println ("a == b " + (a == b))
    println ("a != b " + (a != b))
    println ("a.deep == b.deep " + (a.deep == b.deep))

    val x = new VectorD (1., 2.)
    val y = new VectorD (1., 2.)
    val z = new VectorD (2., 2.)
    println ("x equals y " + (x equals y))
    println ("x == y " + (x == y))
    println ("x != y " + (x != y))
    println ("x equals z " + (x equals z))
    println ("x == z " + (x == z))
    println ("x != z " + (x != z))

} // TestEquals

