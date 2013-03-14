
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Thu Nov 15 13:06:37 EST 2012
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes Bankers.scala
 *  @run     scala -cp ../../classes:classes game.Bankers
 */

package game

import scalation.linalgebra_gen.Matrices.MatrixI
import scalation.linalgebra_gen.Vectors.VectorI

object Bankers extends App
{ 
    var avail = new VectorI (5, 5)           // units of each resource
    val max   = new MatrixI ((4, 2), 3, 2,   // maximum needs of processes
                                     2, 3,
                                     3, 2,
                                     2, 3)
    val alloc = new MatrixI (4, 2)           // current resource allocation

    def request (p: Int, q: VectorI): Boolean =
    {
        var grant = false
        if (q <= avail) {
           println ("units are available to grant request: " + q + " by " + p)
           avail    -= q
           alloc(p) += q               // tentatively make the allocation
           if (safe (p, q)) {
               println ("the request: " + q + " by " + p + " is safe")
               grant = true
           } else {
               avail    += q
               alloc(p) -= q           // not safe, so take back allocation
           } // if
        } // if
        grant
    } // request

    def safe (p: Int, q: VectorI): Boolean =
    {
        val done = new VectorI (4)     // initially not done, all zero
        var work = avail               // keep track of reclaimed resources
        var cont = true
        while (cont) {
            cont = false
            for (i <- 0 until done.dim) {
                if (done(i) == 0 && max(i) - alloc(i) <= work) {
                    done(i) = 1        // process can finish
                    work += alloc(i)   // could release it allocation
                    cont = true        // resources released => continue
                } // if
            } // for
        } // while
        done.sum == done.dim
    } // safe

    request (0, new VectorI (2, 2))
    request (1, new VectorI (2, 2))
    request (2, new VectorI (1, 0))
    request (3, new VectorI (0, 1))

} // Bankers object

