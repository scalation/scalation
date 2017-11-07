
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Naman Fatehpuria
 *  @version 1.4
 *  @date    Mon Jul 29 14:09:25 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{ceil, min}

import scalation.linalgebra.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NMFactorization` class factors a matrix 'v' into two non negative matrices 
 *  'w' and 'h' such that 'v = wh' approximately.
 *  @see  en.wikipedia.org/wiki/Non-negative_matrix_factorization
 *  @param v      the matrix to be factored 
 *  @param loops  the number of iterations before checking the termination condition  
 *  @param r      factor the m-by-n matrix 'v' in to two factors: an m-by-r and r-by-n matrix  
 */ 
class NMFactorization (v: MatrixD, 
		       loops: Int = 10,
		       var r: Int = 0)
{
    private val m = v.dim1                  // number of rows in matrix v
    private val n = v.dim2     	            // number of columns in matrix v
    private val EPSILON = 1.0E-4            // number close to zero
    private val DEBUG = true                // debug flag 

    if (r == 0) r = ceil (min (v.dim1, v.dim2).toDouble / 2).toInt + 1

    private val w  = new MatrixD (m, r)     // left factor (m-by-r matrix)
    private val h  = new MatrixD (r, n)     // right factor (r-by-n matrix)
    w.set (1.0); h.set (1.0)                // initialize all matrix elements to 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor the original matrix 'v' into left 'w' and right 'h' matrices by
     *  iteratively calling the 'update' method until the difference between
     *  'v' and its approximation 'vv = w * h' is sufficiently small.
     */
    def factor (): Tuple2 [MatrixD, MatrixD] = 
    {
        var vv: MatrixD = null                        // holds product of the factors (vv -> v)

        println ("Original Matrix to Factor v = " + v)
        var converged = false                         // convergence flag
        var i         = 0                             // number of update steps performed
 
        while (! converged) {
            for (k <- 0 until loops) update ()        // perform several update steps
    
            vv = w * h                                // compute new product of factors
            val dist = (vv - v).norm1                 // compute norm of the difference
            if (DEBUG) {
                println ("Result for iteration " + i + ":"); i += loops
                println ("w = " + w + "\nh = " + h)
                println ("vv = " + vv)
                println ("dist = " + dist)
            } // if

            if (dist < EPSILON) converged = true             // quit when distance is small enough
        } // while

        println ("Factors w = " + w + "and     h = " + h)    // print factors
        println ("Product of Factors vv = " + vv)            // print approximation
        (w, h) 	                                             // return factors
    } // factor
        
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a multiplicative update step to create better estimates for
     *  factor matrices w and h.
     */
    private def update ()
    {
	val v_ht = v * h.t
	val h_ht = h * h.t
	for (i <- 0 until m; a <- 0 until r) {	
            w(i, a) *= v_ht(i, a) / (w(i) dot h_ht.col(a))
        } // for

	val wt_v = w.t * v
	val wt_w = w.t * w
	for (b <- 0 until r; j <- 0 until n) {
	    h(b, j) *= wt_v(b, j) / (wt_w(b) dot h.col(j))
	} // for	          
    } // update

} // NMFactorization class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NMFactorizationTest` object to test `NMFactorizationTest` class.
 */
object NMFactorizationTest extends App
{
    val v = new MatrixD((4,6), 1.0, 2.0, 3.0,  4.0,  5.0,  6.0,
                               4.0, 5.0, 6.0,  7.0,  8.0,  6.0,
			       7.0, 8.0, 9.0, 10.0, 11.0, 12.0,
			       7.0, 8.0, 9.0, 10.0, 11.0, 12.0)

    val nmf = new NMFactorization (v)
    val f = nmf.factor ()
    println ("f: " + f)

} // NMFactorizationTest object

