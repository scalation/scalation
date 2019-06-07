
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/* @author  John Miller
 * @version 1.6
 * @date    Thu Jun  7 21:39:16 EDT 2018
 * @see     LICENSE (MIT style license file).
 *
 * Fast Fourier Transform
 *
 * @see www.cs.au.dk/~gerth/advising/thesis/joergen-fogh.pdf
 * @see en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm
 */

package scalation.calculus

//  U N D E R   D E V E L O P M E N T

import scala.math.Pi

import scalation.linalgebra.{VectoC, VectorC, VectorD}
import scalation.math.{Complex, int_exp, log2}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FFT` object provides a method to transform a signal in the time domain
 *  (e.g., a times series) in its representation in the frequency domain.
 *  The inverse transform is also provided.
 */
object FFT
{
    private val DEBUG = true                                     // debug flag
    private val omega = -2.0 * Pi * Complex._i                   // -2πi

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform a signal in the time domain into its representation in the
     *  frequency domain using the Cooley–Tukey Fast Fourier Transform algorithm.
     *  The values are updated in place.
     *  FIX - algorithm should allow signals of any size
     *  @see www.cs.au.dk/~gerth/advising/thesis/joergen-fogh.pdf, Figure 5.7.
     *  @param x  the signal in the time domain as a vector of Complex numbers
     */
    def fft (x: VectoC): VectoC =
    {
        val n     = x.dim                                        // number of points in signal
        val logn  = log2(n).toInt                                // log base 2 of n

        for (s <- 1 to logn) {
            val step  = 2~^s                                     // 2^(t-q+1)
            val step2 = step / 2                                 // half step
            val root  = Complex.exp (omega / step)               // root of unity omega_n^(2^q)
            if (DEBUG) println (s"fft: iteration $s: step = $step")
            for (k <- 0 until n-step by step) {                  // loop over offsets
                var prod = Complex._1
                for (j <- 0 until step2) {                       // make pass thru signal x
                    val a = x(j + k)
                    val b = x(j + step2 + k)
                    val c = x(j + 1 + k)
                    val d = x(j + 1 + step2 + k)
                    x(j + k)             = a + b
                    x(j + step2 + k)     = prod * (a - b)
                    x(j + 1 + k)         = c + d; prod *= root
                    x(j + 1 + step2 + k) = prod * (c - d)
                    prod *= root
                } // for
            } // for
        } // for
        x
    } // fft

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform a signal in the frequency domain into its representation in the
     *  time domain using an Inverse Fast Fourier Transform algorithm.
     *  The values are updated in place.
     *  FIX - algorithm should allow signals of any size
     *  @see www.dsprelated.com/showarticle/800.php
     *  @param x  the signal in the frequency domain as a vector of Complex numbers
     */
    def ifft (y: VectoC): VectoC =
    {
        val n = y.dim
        val z = new VectorC (n)
        z(0) = y(0)
        for (i <- 1 until n) z(i) = y(n-i)
        fft (z) / n
    } // ifft
 
} // FFT object

import FFT._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FFTTest` is used to test the `FFT` object by starting with a signal in th4e
 *  time domain, converting it to its frequency domain representation and the
 *  converting it back to the time domain.
 *  > runMain scalation.calculus.FFTTest
 */
object FFTTest extends App
{
     import scala.math.sin
     import scalation.plot.Plot

     val (a1, a2) = (1.0, 1.0)                                    // amplitutes
     val (w1, w2) = (2.0, 4.0)                                    // fequencies

     val n = 256
     val t = VectorD.range (0, n) / 20.0
     val x = new VectorC (n)
     for (j <- x.range) x(j) = a1 * sin (w1 * t(j)) + a2 * sin (w2 * t(j))
     new Plot (t, x.toDouble, null, "original signal")

     fft (x)
     new Plot (t, x.toDouble, null, "frequency domain signal")

     val z = ifft (x)
     new Plot (t, x.toDouble, null, "recovered signal")
     
} // FFTTest object

