
import scala.collection.mutable._


case class MyClass (v: Int) extends Ordered [MyClass] 
{
    def this()= this(5)

    val value = v

    def compare (that: MyClass) = this.value.compare (that.value)
//    def compare (x:MyClass,y:MyClass):Int=x.value.compare(y.value)

    override def toString()= "the value is "+v

//    implicit def myClass2Ordering(n: MyClass): Ordering[MyClass] = n.asInstanceOf[Ordering[MyClass]]	
} // MyClass

//case class MyC (time: Double, id: Int) extends Tuple2 (time, id)
//{
//    def compare (that: MyC) = this.time.compare (that.time)
//}


object PQ extends Application
{
//    implicit def myClass2Ordering(n: MyClass): Ordering[MyClass] = n.asInstanceOf[Ordering[MyClass]]

//  val pq = new PriorityQueue [Int] ()
//  val pq = new PriorityQueue [Tuple2 [Double, Int]] ()

    val c1 = new MyClass (5)
    val c2 = new MyClass (6)
    println ("c1 < c2 = " + (c1 < c2))
	
    
    val pq = new PriorityQueue [MyClass] ()(Ordering.ordered[MyClass])
    println ("pq = " + pq)

} //PQ

