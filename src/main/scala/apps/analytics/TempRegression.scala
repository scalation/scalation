
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Sep 13 17:52:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package apps.analytics

import scalation.analytics.Regression
import scalation.analytics.RegTechnique._
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.time

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TempRegression` object performs regression on a small dataset of
 *  temperatures from counties in Texas where the variables/factors to consider
 *  are Latitude (x1), Elevation (x2) and Longitude (x3).  The regression equation
 *  is the following:
 *  <p>
 *      y  =  b dot x  =  b0 + b1*x1 + b2*x2 + b3*x3
 *  <p>
 *  A fit is created for both the full model and a reduce model created using
 *  backward elimination.
 *  @see www.stat.ufl.edu/~winner/cases/txtemp.ppt
 *  > run-main apps.analytics.TempRegression
 */
object TempRegression extends App
{
    // 16 data points:        Constant      x1      x2       x3
    //                                     Lat    Elev     Long        County
    val x = new  MatrixD ((16, 4), 1.0, 29.767,   41.0,  95.367,    // Harris
                                   1.0, 32.850,  440.0,  96.850,    // Dallas
                                   1.0, 26.933,   25.0,  97.800,    // Kennedy
                                   1.0, 31.950, 2851.0, 102.183,    // Midland
                                   1.0, 34.800, 3840.0, 102.467,    // Deaf Smith
                                   1.0, 33.450, 1461.0,  99.633,    // Knox
                                   1.0, 28.700,  815.0, 100.483,    // Maverick
                                   1.0, 32.450, 2380.0, 100.533,    // Nolan
                                   1.0, 31.800, 3918.0, 106.400,    // El Paso
                                   1.0, 34.850, 2040.0, 100.217,    // Collington
                                   1.0, 30.867, 3000.0, 102.900,    // Pecos
                                   1.0, 36.350, 3693.0, 102.083,    // Sherman
                                   1.0, 30.300,  597.0,  97.700,    // Travis
                                   1.0, 26.900,  315.0,  99.283,    // Zapata
                                   1.0, 28.450,  459.0,  99.217,    // Lasalle
                                   1.0, 25.900,   19.0,  97.433)    // Cameron

    val y = VectorD (56.0, 48.0, 60.0, 46.0, 38.0, 46.0, 53.0, 46.0,
                     44.0, 41.0, 47.0, 36.0, 52.0, 60.0, 56.0, 62.0)

    var rg: Regression [MatrixD, VectorD] = _
    val z = VectorD (1.0, 30.0, 1000.0, 100.0)

    println ("-------------------------------------------------")
    println ("Fit the parameter vector b using QR Factorization")
    time { rg = new Regression (x, y) }                        // use QR Factorization
    time { rg.train () }
    println ("full model: fit = " + rg.fit)
    println ("predict (" + z + ") = " + rg.predict (z))
    println ("reduced model: fit = " + rg.backElim ())

    println ("-------------------------------------------------")
    println ("Fit the parameter vector b using Cholesky Factorization")
    time { rg = new Regression (x, y, Fac_Cholesky) }          // use Cholesky Factorization
    time { rg.train () }
    println ("full model: fit = " + rg.fit)
    println ("predict (" + z + ") = " + rg.predict (z))
    println ("reduced model: fit = " + rg.backElim ())

    println ("-------------------------------------------------")
    println ("Fit the parameter vector b using Matrix Inversion")
    time { rg = new Regression (x, y, Inverse) }               // use Matrix Inversion
    time { rg.train () }
    println ("full model: fit = " + rg.fit)
    println ("predict (" + z + ") = " + rg.predict (z))
    println ("reduced model: fit = " + rg.backElim ())

} // TempRegression

