
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import java.time.Instant

import scala.reflect.ClassTag

import scalation.math.{Complex, Rational, Real}
import scalation.math.{double_exp, int_exp, long_exp}
import scalation.math.StrO.StrNum
import scalation.math.TimeO.TimeNum
import scalation.math.{noComplex, noDouble, noInt, noLong, noRational, noReal, noStrNum, noTimeNum}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vec` trait establishes a common base type for all ScalaTion `linalgebra`
 *  vectors (e.g., `VectorD`, VectorI).
 */
trait Vec
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size (number of elements) of the vector.
     */
    def size: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the range of all indices (0 to one less than dim).
     */
    def indices: Range

} // Vec trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vec` object provides a minimal set of functions that apply across all
 *  types of ScalaTion `linalgebra` vectors. 
 *  @see `scalation.columnar_db.Relation`
 */
object Vec
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'i'th element of vector 'x'.
     *  @param x  the vector to access
     *  @param i  the index position 
     */
    def apply (x: Vec, i: Int): Any =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC] (i)
        case _: VectorD => x.asInstanceOf [VectorD] (i)
        case _: VectorI => x.asInstanceOf [VectorI] (i)
        case _: VectorL => x.asInstanceOf [VectorL] (i)
        case _: VectorQ => x.asInstanceOf [VectorQ] (i)
        case _: VectorR => x.asInstanceOf [VectorR] (i)
        case _: VectorS => x.asInstanceOf [VectorS] (i)
        case _: VectorT => x.asInstanceOf [VectorT] (i)
        case _  =>  println (s"apply: vector type ${x.getClass} is not supported"); 0
        } // match
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'iv' elements of vector 'x'.
     *  @param x   the vector to access
     *  @param iv  the index positions
     */
    def apply (x: Vec, iv: VectoI): Vec =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].select (iv)
        case _: VectorD => x.asInstanceOf [VectorD].select (iv)
        case _: VectorI => x.asInstanceOf [VectorI].select (iv)
        case _: VectorL => x.asInstanceOf [VectorL].select (iv)
        case _: VectorQ => x.asInstanceOf [VectorQ].select (iv)
        case _: VectorR => x.asInstanceOf [VectorR].select (iv)
        case _: VectorS => x.asInstanceOf [VectorS].select (iv)
        case _: VectorT => x.asInstanceOf [VectorT].select (iv)
        case _  =>  println (s"apply: vector type ${x.getClass} is not supported"); null
        } // match
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the 'i'th element of vector 'x'.
     *  @param x  the vector to update
     *  @param i  the index position
     *  @param s  the scalar to assign
     */
    def update [T <: Any] (x: Vec, i: Int, s: T)
    {
        s match {
        case _: Complex  => x.asInstanceOf [VectorC] (i) = s.asInstanceOf [Complex]
        case _: Double   => x.asInstanceOf [VectorD] (i) = s.asInstanceOf [Double]
        case _: Int      => x.asInstanceOf [VectorI] (i) = s.asInstanceOf [Int]
        case _: Long     => x.asInstanceOf [VectorL] (i) = s.asInstanceOf [Long]
        case _: Rational => x.asInstanceOf [VectorQ] (i) = s.asInstanceOf [Rational]
        case _: Real     => x.asInstanceOf [VectorR] (i) = s.asInstanceOf [Real]
        case _: StrNum   => x.asInstanceOf [VectorS] (i) = s.asInstanceOf [StrNum]
        case _: String   => x.asInstanceOf [VectorS] (i) = s.asInstanceOf [String]
        case _: TimeNum  => x.asInstanceOf [VectorT] (i) = s.asInstanceOf [TimeNum]
        case _  =>  println (s"update: scalar type ${s.getClass} is not supported")
        } // match
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate vectors 'x' and 'y'.
     *  @param x  the first vector
     *  @param y  the second vector
     */
    def ++ (x: Vec, y: Vec): Vec =
    {
        if (x == null && y == null) null
        else if (x == null) y
        else if (y == null) x
        else x match {
            case _: VectorC => x.asInstanceOf [VectorC] ++ y.asInstanceOf [VectorC]
            case _: VectorD => x.asInstanceOf [VectorD] ++ y.asInstanceOf [VectorD]
            case _: VectorI => x.asInstanceOf [VectorI] ++ y.asInstanceOf [VectorI]
            case _: VectorL => x.asInstanceOf [VectorL] ++ y.asInstanceOf [VectorL]
            case _: VectorQ => x.asInstanceOf [VectorQ] ++ y.asInstanceOf [VectorQ]
            case _: VectorR => x.asInstanceOf [VectorR] ++ y.asInstanceOf [VectorR]
            case _: VectorS => x.asInstanceOf [VectorS] ++ y.asInstanceOf [VectorS]
            case _: VectorT => x.asInstanceOf [VectorT] ++ y.asInstanceOf [VectorT]
            case _  =>  println (s"++ vector type ${x.getClass} is not supported"); null
            } // match
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Copy of vector 'x' with scalar 's' appended.
     *  FIX - different in Yang Fan's code.
     *  @param x  the vector
     *  @param s  the scalar to append
     */
    def :+ [T <: Any] (x: Vec, s: T): Vec =
    {
        s match {
        case _: Complex  => if (x == null) VectorC (s.asInstanceOf [Complex])
                            else x.asInstanceOf [VectorC] ++ s.asInstanceOf [Complex]
        case _: Double   => if (x == null) VectorD (s.asInstanceOf [Double])
                            else x.asInstanceOf [VectorD] ++ s.asInstanceOf [Double]
        case _: Int      => if (x == null) VectorI (s.asInstanceOf [Int])
                            else x.asInstanceOf [VectorI] ++ s.asInstanceOf [Int]
        case _: Long     => if (x == null) VectorL (s.asInstanceOf [Long])
                            else x.asInstanceOf [VectorL] ++ s.asInstanceOf [Long]
        case _: Rational => if (x == null) VectorQ (s.asInstanceOf [Rational])
                            else x.asInstanceOf [VectorQ] ++ s.asInstanceOf [Rational]
        case _: Real     => if (x == null) VectorR (s.asInstanceOf [Real])
                            else x.asInstanceOf [VectorR] ++ s.asInstanceOf [Real]
        case _: StrNum   => if (x == null) VectorS (s.asInstanceOf [StrNum])
                            else x.asInstanceOf [VectorS] ++ s.asInstanceOf [StrNum]
        case _: String   => if (x == null) VectorS (StrNum (s.asInstanceOf [String]))
                            else x.asInstanceOf [VectorS] ++ StrNum (s.asInstanceOf [String])
        case _: TimeNum  => if (x == null) VectorT (s.asInstanceOf [TimeNum])
                            else x.asInstanceOf [VectorT] ++ s.asInstanceOf [TimeNum]
        case _  =>  println (s":+ scalar type ${s.getClass} is not supported"); null
        } // match
    } // :+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter vector 'x' based on predicate 'p', returning a new vector.
     *  @param x  the vector to filter
     *  @param p  the predicate ('Boolean' function) to apply
     */
//  def filter [T: ClassTag: Numeric] (x: Vec, p: T => Boolean): Vec =
    def filter [T: ClassTag] (x: Vec, p: T => Boolean): Vec =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].filter (p.asInstanceOf [Complex => Boolean])
        case _: VectorD => x.asInstanceOf [VectorD].filter (p.asInstanceOf [Double => Boolean])
        case _: VectorI => x.asInstanceOf [VectorI].filter (p.asInstanceOf [Int => Boolean])
        case _: VectorL => x.asInstanceOf [VectorL].filter (p.asInstanceOf [Long => Boolean])
        case _: VectorQ => x.asInstanceOf [VectorQ].filter (p.asInstanceOf [Rational => Boolean])
        case _: VectorR => x.asInstanceOf [VectorR].filter (p.asInstanceOf [Real => Boolean])
        case _: VectorS => x.asInstanceOf [VectorS].filter (p.asInstanceOf [StrNum => Boolean])
        case _: VectorT => x.asInstanceOf [VectorT].filter (p.asInstanceOf [TimeNum => Boolean])
        case _  =>  println (s"filter: vector type ${x.getClass} is not supported"); null
        } // match
    } // filter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter out missing values from vector 'x'.
     *  @param x  the vector to filter
     */
    def filterMissing (x: Vec): Vec =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].filterMissing
        case _: VectorD => x.asInstanceOf [VectorD].filterMissing
        case _: VectorI => x.asInstanceOf [VectorI].filterMissing
        case _: VectorL => x.asInstanceOf [VectorL].filterMissing
        case _: VectorQ => x.asInstanceOf [VectorQ].filterMissing
        case _: VectorR => x.asInstanceOf [VectorR].filterMissing
        case _: VectorS => x.asInstanceOf [VectorS].filterMissing
        case _: VectorT => x.asInstanceOf [VectorT].filterMissing
        case _  =>  println (s"filterMissing: vector type ${x.getClass} is not supported"); null
        } // match
    } // filter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter vector 'x' based on predicate 'p', returning the positions in the vector.
     *  @param x  the vector to filter
     *  @param p  the predicate ('Boolean' function) to apply
     */
//  def filterPos [T: ClassTag: Numeric] (x: Vec, p: T => Boolean): Seq [Int] =
    def filterPos [T: ClassTag] (x: Vec, p: T => Boolean): Seq [Int] =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].filterPos (p.asInstanceOf [Complex => Boolean])
        case _: VectorD => x.asInstanceOf [VectorD].filterPos (p.asInstanceOf [Double => Boolean])
        case _: VectorI => x.asInstanceOf [VectorI].filterPos (p.asInstanceOf [Int => Boolean])
        case _: VectorL => x.asInstanceOf [VectorL].filterPos (p.asInstanceOf [Long => Boolean])
        case _: VectorQ => x.asInstanceOf [VectorQ].filterPos (p.asInstanceOf [Rational => Boolean])
        case _: VectorR => x.asInstanceOf [VectorR].filterPos (p.asInstanceOf [Real => Boolean])
        case _: VectorS => x.asInstanceOf [VectorS].filterPos (p.asInstanceOf [StrNum => Boolean])
        case _: VectorT => x.asInstanceOf [VectorT].filterPos (p.asInstanceOf [TimeNum => Boolean])
        case _  =>  println (s"filterPos: vector type ${x.getClass} is not supported"); null
        } // match
    } // filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select elements from vector 'x' at the given index positions.
     *  @param x    the vector to select from
     *  @param pos  the positions to select
     */
    def select (x: Vec, pos: Seq [Int]): Vec =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].select (pos.toArray)
        case _: VectorD => x.asInstanceOf [VectorD].select (pos.toArray)
        case _: VectorI => x.asInstanceOf [VectorI].select (pos.toArray)
        case _: VectorL => x.asInstanceOf [VectorL].select (pos.toArray)
        case _: VectorQ => x.asInstanceOf [VectorQ].select (pos.toArray)
        case _: VectorR => x.asInstanceOf [VectorR].select (pos.toArray)
        case _: VectorS => x.asInstanceOf [VectorS].select (pos.toArray)
        case _: VectorT => x.asInstanceOf [VectorT].select (pos.toArray)
        case _  =>  println (s"select: vector type ${x.getClass} is not supported"); null
        } // match
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a vector of a different type to `VectorI`.
     *  @param x  the vector to convert
     */
    def toInt (x: Vec): VectoI =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].toInt
        case _: VectorD => x.asInstanceOf [VectorD].toInt
        case _: VectorI => x.asInstanceOf [VectorI]
        case _: VectorL => x.asInstanceOf [VectorL].toInt
        case _: VectorQ => x.asInstanceOf [VectorQ].toInt
        case _: VectorR => x.asInstanceOf [VectorR].toInt
        case _: VectorS => x.asInstanceOf [VectorS].toInt
        case _: VectorT => x.asInstanceOf [VectorT].toInt
        case _  =>  println (s"toInt: vector type ${x.getClass} is not supported"); null
        } // match
    } // toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a vector of a different type to `VectorD`.
     *  @param x  the vector to convert
     */
    def toDouble (x: Vec): VectoD =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].toDouble
        case _: VectorD => x.asInstanceOf [VectorD]
        case _: VectorI => x.asInstanceOf [VectorI].toDouble
        case _: VectorL => x.asInstanceOf [VectorL].toDouble
        case _: VectorQ => x.asInstanceOf [VectorQ].toDouble
        case _: VectorR => x.asInstanceOf [VectorR].toDouble
        case _: VectorS => x.asInstanceOf [VectorS].toDouble
        case _: VectorT => x.asInstanceOf [VectorT].toDouble
        case _  =>  println (s"toDouble: vector type ${x.getClass} is not supported"); null
        } // match
    } // toDouble

    // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert compressed RleVector to dense Vector.
     *  FIX - RleVectorT to be implemented
     *  @param x  the vector to convert
     */
    def toDense (x: Vec): Vec =
    {
        x match {
        case _: VectorC | _: RleVectorC => x match { case _: RleVectorC => x.asInstanceOf [RleVectorC].toDense; case _ => x }
        case _: VectorD | _: RleVectorD => x match { case _: RleVectorD => x.asInstanceOf [RleVectorD].toDense; case _ => x }
        case _: VectorI | _: RleVectorI => x match { case _: RleVectorI => x.asInstanceOf [RleVectorI].toDense; case _ => x }
        case _: VectorL | _: RleVectorL => x match { case _: RleVectorL => x.asInstanceOf [RleVectorL].toDense; case _ => x }
        case _: VectorQ | _: RleVectorQ => x match { case _: RleVectorQ => x.asInstanceOf [RleVectorQ].toDense; case _ => x }
        case _: VectorR | _: RleVectorR => x match { case _: RleVectorR => x.asInstanceOf [RleVectorR].toDense; case _ => x }
        case _: VectorS | _: RleVectorS => x match { case _: RleVectorS => x.asInstanceOf [RleVectorS].toDense; case _ => x }
//      case _: VectorT | _: RleVectorT => x match { case _: RleVectorT => x.asInstanceOf [RleVectorT].toDense; case _ => x }
        case _  =>  println (s"toDense: vector type ${x.getClass} is not supported"); null
        } // match
    } // toDense

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the minimum of vector 'x'.
     *  @param x  the vector whose min is sought
     */
    def min (x: Vec): Any =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].min ()
        case _: VectorD => x.asInstanceOf [VectorD].min ()
        case _: VectorI => x.asInstanceOf [VectorI].min ()
        case _: VectorL => x.asInstanceOf [VectorL].min ()
        case _: VectorQ => x.asInstanceOf [VectorQ].min ()
        case _: VectorR => x.asInstanceOf [VectorR].min ()
        case _: VectorS => x.asInstanceOf [VectorS].min ()
        case _: VectorT => x.asInstanceOf [VectorT].min ()
        case _  =>  println (s"min: vector type ${x.getClass} is not supported"); null
        } // match
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the maximum of vector 'x'.
     *  @param x  the vector whose max is sought
     */
    def max (x: Vec): Any =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].max ()
        case _: VectorD => x.asInstanceOf [VectorD].max ()
        case _: VectorI => x.asInstanceOf [VectorI].max ()
        case _: VectorL => x.asInstanceOf [VectorL].max ()
        case _: VectorQ => x.asInstanceOf [VectorQ].max ()
        case _: VectorR => x.asInstanceOf [VectorR].max ()
        case _: VectorS => x.asInstanceOf [VectorS].max ()
        case _: VectorT => x.asInstanceOf [VectorT].max ()
        case _  =>  println (s"max: vector type ${x.getClass} is not supported"); null
        } // match
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of vector 'x'.
     *  @param x  the vector whose sum is sought
     */
    def sum (x: Vec): Any =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].sum
        case _: VectorD => x.asInstanceOf [VectorD].sum
        case _: VectorI => x.asInstanceOf [VectorI].sum
        case _: VectorL => x.asInstanceOf [VectorL].sum
        case _: VectorQ => x.asInstanceOf [VectorQ].sum
        case _: VectorR => x.asInstanceOf [VectorR].sum
        case _: VectorS => x.asInstanceOf [VectorS].sum
        case _: VectorT => x.asInstanceOf [VectorT].sum
        case _  =>  println (s"sum: vector type ${x.getClass} is not supported"); null
        } // match
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of vector 'x'.
     *  @param x  the vector whose mean is sought
     */
    def mean (x: Vec): Any =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].mean
        case _: VectorD => x.asInstanceOf [VectorD].mean
        case _: VectorI => x.asInstanceOf [VectorI].mean
        case _: VectorL => x.asInstanceOf [VectorL].mean
        case _: VectorQ => x.asInstanceOf [VectorQ].mean
        case _: VectorR => x.asInstanceOf [VectorR].mean
        case _: VectorS => x.asInstanceOf [VectorS].mean
        case _: VectorT => x.asInstanceOf [VectorT].mean
        case _  =>  println (s"mean: vector type ${x.getClass} is not supported"); null
        } // match
    } // mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the variance of vector 'x'.
     *  @param x  the vector whose variance is sought
     */
    def variance (x: Vec): Any =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].variance
        case _: VectorD => x.asInstanceOf [VectorD].variance
        case _: VectorI => x.asInstanceOf [VectorI].variance
        case _: VectorL => x.asInstanceOf [VectorL].variance
        case _: VectorQ => x.asInstanceOf [VectorQ].variance
        case _: VectorR => x.asInstanceOf [VectorR].variance
        case _: VectorS => x.asInstanceOf [VectorS].variance
        case _: VectorT => x.asInstanceOf [VectorT].variance
        case _  =>  println (s"variance: vector type ${x.getClass} is not supported"); null
        } // match
    } // variance

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the correlation between vectors 'x' and 'y'.
     *  @param x  the first vector
     *  @param y  the second vector
     */
    def corr (x: Vec, y: Vec): Double =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].corr (y.asInstanceOf [VectorC].toDouble)
        case _: VectorD => x.asInstanceOf [VectorD].corr (y.asInstanceOf [VectorD])
        case _: VectorI => x.asInstanceOf [VectorI].corr (y.asInstanceOf [VectorI].toDouble)
        case _: VectorL => x.asInstanceOf [VectorL].corr (y.asInstanceOf [VectorL].toDouble)
        case _: VectorQ => x.asInstanceOf [VectorQ].corr (y.asInstanceOf [VectorQ].toDouble)
        case _: VectorR => x.asInstanceOf [VectorR].corr (y.asInstanceOf [VectorR].toDouble)
        case _: VectorS => println (s"corr: correlation not applicable to vector of StrNum"); Double.NaN
        case _: VectorT => x.asInstanceOf [VectorT].corr (y.asInstanceOf [VectorR].toDouble)
        case _  =>  println (s"corr: vector type ${x.getClass} is not supported"); Double.NaN
        } // match
    } // corr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a double to a value of specific type based on the provided vector.
     *  @param x   the vector whose element type is of interest
     *  @param dd  the double to be converted
     */
    def fromDouble (x: Vec, dd: Any): Any =
    {
        if (dd.isInstanceOf [Double]) {
            val d = dd.asInstanceOf [Double]
            x match {
            case _: VectorC => new Complex (d)
            case _: VectorD => d
            case _: VectorI => d.toInt
            case _: VectorL => d.toLong
            case _: VectorQ => new Rational (d.toLong)
            case _: VectorR => new Real (d)
            case _: VectorS => new StrNum (d.toString)
            case _: VectorT => new TimeNum (Instant.ofEpochSecond (d.toLong))
            case _  => println (s"fromDouble: vector type ${x.getClass} is not supported"); Double.NaN
            } // match
        } else dd                         // was not a double, so do not convert
    } // fromDouble

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the appropriate missing value that is compatible with vector 'x'.
     *  @param x  the vector whose base type is of interest
     */
    def noValue (x: Vec): Any =
    {
        x match {
        case _: VectorC => noComplex
        case _: VectorD => noDouble
        case _: VectorI => noInt
        case _: VectorL => noLong
        case _: VectorQ => noRational
        case _: VectorR => noReal
        case _: VectorS => noStrNum
        case _: VectorT => noTimeNum
        case _  =>  println ("noValue: vector type ${x.getClass} is not supported"); Double.NaN
        } // match
    } // noValue

} // Vec object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vec_Elem` object provides comparison operators for `Vec` elements.
 */
object Vec_Elem
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the two elements and return if the first element is smaller than the second.
     *  @param x  first element to compare
     *  @param y  second element to compare
     */
    def < (x: Any, y: Any): Boolean =
    {
        y match {
        case _: Complex  => x.asInstanceOf [Complex]  < y.asInstanceOf [Complex]
        case _: Double   => x.asInstanceOf [Double]   < y.asInstanceOf [Double]
        case _: Int      => x.asInstanceOf [Int]      < y.asInstanceOf [Int]
        case _: Long     => x.asInstanceOf [Long]     < y.asInstanceOf [Long]
        case _: Rational => x.asInstanceOf [Rational] < y.asInstanceOf [Rational]
        case _: Real     => x.asInstanceOf [Real]     < y.asInstanceOf [Real]
        case _: StrNum   => x.asInstanceOf [StrNum]   < y.asInstanceOf [StrNum]
        case _: TimeNum  => x.asInstanceOf [TimeNum]  < y.asInstanceOf [TimeNum]
        case _  => println (s"<: scalar type ${y.getClass} is not supported"); false
        } // match
    } // <

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the two elements and return if they are nearly equal.
     *  @param x  first element to compare
     *  @param y  second element to compare
     */
    def =~ (x: Any, y: Any): Boolean =
    {
        y match {
        case _: Complex  => x.asInstanceOf [Complex]  =~ y.asInstanceOf [Complex]
        case _: Double   => x.asInstanceOf [Double]   =~ y.asInstanceOf [Double]
        case _: Int      => x.asInstanceOf [Int]      =~ y.asInstanceOf [Int]
        case _: Long     => x.asInstanceOf [Long]     =~ y.asInstanceOf [Long]
        case _: Rational => x.asInstanceOf [Rational] =~ y.asInstanceOf [Rational]
        case _: Real     => x.asInstanceOf [Real]     =~ y.asInstanceOf [Real]
        case _: StrNum   => x.asInstanceOf [StrNum]   =~ y.asInstanceOf [StrNum]
        case _: TimeNum  => x.asInstanceOf [TimeNum]  =~ y.asInstanceOf [TimeNum]
        case _  => println (s"<: scalar type ${y.getClass} is not supported"); false
        } // match
    } // =~
   
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the two elements and return if they are not nearly equal.
     *  @param x   first element to compare
     *  @param y   second element to compare
     */
    def !=~ (x: Any, y: Any): Boolean = ! =~ (x, y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert an element to double.
     *  @param x   the element to be converted to a double
     */
    def toDouble (x : Any): Double =
    {
        x match {
        case _: Complex  => x.asInstanceOf [Complex].toDouble
        case _: Double   => x.asInstanceOf [Double]
        case _: Int      => x.asInstanceOf [Int].toDouble
        case _: Long     => x.asInstanceOf [Long].toDouble
        case _: Rational => x.asInstanceOf [Rational].toDouble
        case _: Real     => x.asInstanceOf [Real].toDouble
        case _: StrNum   => x.asInstanceOf [StrNum].toDouble
        case _: TimeNum  => x.asInstanceOf [TimeNum].toDouble
        case _  => println (s"<: scalar type ${x.getClass} is not supported"); Double.NaN
        } // match
    } // toDouble

} // Vec_Elem object

