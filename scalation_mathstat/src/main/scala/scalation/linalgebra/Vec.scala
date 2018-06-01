
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.reflect.ClassTag

import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO.StrNum
import scalation.stat.{vectorC2StatVector, vectorD2StatVector, vectorI2StatVector,
                       vectorL2StatVector, vectorQ2StatVector, vectorR2StatVector}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vec` trait establishes a common base type for all vectors (e.g., `VectorD`).
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
 *  types of vectors. 
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
        case _  =>  println ("apply: vector type not supported"); 0
        } // match
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'i'th element of vector 'x'.
     *  @param x  the vector to access
     *  @param i  the index position 
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
        case _  =>  println ("apply: vector type not supported"); null
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
        case _  =>  println ("update: vector type not supported")
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
            case _  =>  println ("++ vector type not supported"); null
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
        case _  =>  println (":+ vector type not supported"); null
        } // match
    } // :+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter vector 'x' based on predicate 'p', returning a new vector.
     *  @param x  the vector to filter
     *  @param p  the predicate ('Boolean' function) to apply
     */
    def filter [T: ClassTag: Numeric] (x: Vec, p: T => Boolean): Vec =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].filter (p.asInstanceOf [Complex => Boolean])
        case _: VectorD => x.asInstanceOf [VectorD].filter (p.asInstanceOf [Double => Boolean])
        case _: VectorI => x.asInstanceOf [VectorI].filter (p.asInstanceOf [Int => Boolean])
        case _: VectorL => x.asInstanceOf [VectorL].filter (p.asInstanceOf [Long => Boolean])
        case _: VectorQ => x.asInstanceOf [VectorQ].filter (p.asInstanceOf [Rational => Boolean])
        case _: VectorR => x.asInstanceOf [VectorR].filter (p.asInstanceOf [Real => Boolean])
        case _: VectorS => x.asInstanceOf [VectorS].filter (p.asInstanceOf [StrNum => Boolean])
        case _  =>  println ("filter: vector type not supported"); null
        } // match
    } // filter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter vector 'x' based on predicate 'p', returning the positions in the vector.
     *  @param x  the vector to filter
     *  @param p  the predicate ('Boolean' function) to apply
     */
    def filterPos [T: ClassTag: Numeric] (x: Vec, p: T => Boolean): Seq [Int] =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].filterPos (p.asInstanceOf [Complex => Boolean])
        case _: VectorD => x.asInstanceOf [VectorD].filterPos (p.asInstanceOf [Double => Boolean])
        case _: VectorI => x.asInstanceOf [VectorI].filterPos (p.asInstanceOf [Int => Boolean])
        case _: VectorL => x.asInstanceOf [VectorL].filterPos (p.asInstanceOf [Long => Boolean])
        case _: VectorQ => x.asInstanceOf [VectorQ].filterPos (p.asInstanceOf [Rational => Boolean])
        case _: VectorR => x.asInstanceOf [VectorR].filterPos (p.asInstanceOf [Real => Boolean])
        case _: VectorS => x.asInstanceOf [VectorS].filterPos (p.asInstanceOf [StrNum => Boolean])
        case _  =>  println ("filterPos: vector type not supported"); null
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
        case _  =>  println ("select: vector type not supported"); null
        } // match
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a vector of a different type to `VectorI`.
     *  @param x  the vector to convert
     */
    def toInt (x: Vec) =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].toInt
        case _: VectorD => x.asInstanceOf [VectorD].toInt
        case _: VectorI => x.asInstanceOf [VectorI]
        case _: VectorL => x.asInstanceOf [VectorL].toInt
        case _: VectorQ => x.asInstanceOf [VectorQ].toInt
        case _: VectorR => x.asInstanceOf [VectorR].toInt
        case _: VectorS => x.asInstanceOf [VectorS].toInt
        case _  =>  println ("toInt: vector type not supported"); null
        } // match
    } // toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a vector of a different type to `VectorD`.
     *  @param x  the vector to convert
     */
    def toDouble (x: Vec) =
    {
        x match {
        case _: VectorC => x.asInstanceOf [VectorC].toDouble
        case _: VectorD => x.asInstanceOf [VectorD]
        case _: VectorI => x.asInstanceOf [VectorI].toDouble
        case _: VectorL => x.asInstanceOf [VectorL].toDouble
        case _: VectorQ => x.asInstanceOf [VectorQ].toDouble
        case _: VectorR => x.asInstanceOf [VectorR].toDouble
        case _: VectorS => x.asInstanceOf [VectorS].toDouble
        case _  =>  println ("toDouble: vector type not supported"); null
        } // match
    } // toDouble

    // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert compressed RleVector to dense Vector.
     *  @param x  the vector to convert
     */
    def toDense (x: Vec) =
    {
        x match {
        case _: VectorC | _: RleVectorC => x match { case _: RleVectorC => x.asInstanceOf [RleVectorC].toDense; case _ => x }
        case _: VectorD | _: RleVectorD => x match { case _: RleVectorD => x.asInstanceOf [RleVectorD].toDense; case _ => x }
        case _: VectorI | _: RleVectorI => x match { case _: RleVectorI => x.asInstanceOf [RleVectorI].toDense; case _ => x }
        case _: VectorL | _: RleVectorL => x match { case _: RleVectorL => x.asInstanceOf [RleVectorL].toDense; case _ => x }
        case _: VectorQ | _: RleVectorQ => x match { case _: RleVectorQ => x.asInstanceOf [RleVectorQ].toDense; case _ => x }
        case _: VectorR | _: RleVectorR => x match { case _: RleVectorR => x.asInstanceOf [RleVectorR].toDense; case _ => x }
        case _: VectorS | _: RleVectorS => x match { case _: RleVectorS => x.asInstanceOf [RleVectorS].toDense; case _ => x }
        case _  =>  println ("toDense: vector type not supported"); null
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
        case _  =>  println ("min: vector type not supported"); null
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
        case _  =>  println ("max: vector type not supported"); null
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
        case _  =>  println ("sum: vector type not supported"); null
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
        case _  =>  println ("mean: vector type not supported"); null
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
        case _  =>  println ("variance: vector type not supported"); null
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
        case _  =>  println ("corr: vector type not supported"); Double.NaN
        } // match
    } // corr

} // Vec object

