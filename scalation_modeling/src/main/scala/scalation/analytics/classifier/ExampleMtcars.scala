
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Oct 28 16:16:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scalation.linalgebra.MatrixD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleMtcars` object provides the Motor Trend Car Road Tests dataset (mtcars)
 *  as a combined 'xy' matrix.
 *  @see https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
 *  @see https://gist.github.com/seankross/a412dfbd88b3db70b74b
 */
object ExampleMtcars
{
    // mtcars dataset: y  = b0 * 1 + b1 * x1
    //                 y  = V/S (e.g., V-6 vs. I-4)
    //                 x1 = Mpg
    // 32 data points:             One    x1    y
    val xy = new MatrixD ((32, 3), 1.0,  21.0,  0,        //  1 - Mazda RX4
                                   1.0,  21.0,  0,        //  2 - Mazda RX4 Wa
                                   1.0,  22.8,  1,        //  3 - Datsun 710
                                   1.0,  21.4,  1,        //  4 - Hornet 4 Drive
                                   1.0,  18.7,  0,        //  5 - Hornet Sportabout
                                   1.0,  18.1,  1,        //  6 - Valiant
                                   1.0,  14.3,  0,        //  7 - Duster 360
                                   1.0,  24.4,  1,        //  8 - Merc 240D
                                   1.0,  22.8,  1,        //  9 - Merc 230
                                   1.0,  19.2,  1,        // 10 - Merc 280
                                   1.0,  17.8,  1,        // 11 - Merc 280C
                                   1.0,  16.4,  0,        // 12 - Merc 450S
                                   1.0,  17.3,  0,        // 13 - Merc 450SL
                                   1.0,  15.2,  0,        // 14 - Merc 450SLC
                                   1.0,  10.4,  0,        // 15 - Cadillac Fleetwood
                                   1.0,  10.4,  0,        // 16 - Lincoln Continental

                                   1.0,  14.7,  0,        // 17 - Chrysler Imperial
                                   1.0,  32.4,  1,        // 18 - Fiat 128
                                   1.0,  30.4,  1,        // 19 - Honda Civic
                                   1.0,  33.9,  1,        // 20 - Toyota Corolla
                                   1.0,  21.5,  1,        // 21 - Toyota Corona
                                   1.0,  15.5,  0,        // 22 - Dodge Challenger
                                   1.0,  15.2,  0,        // 23 - AMC Javelin
                                   1.0,  13.3,  0,        // 24 - Camaro Z28
                                   1.0,  19.2,  0,        // 25 - Pontiac Firebird
                                   1.0,  27.3,  1,        // 26 - Fiat X1-9
                                   1.0,  26.0,  0,        // 27 - Porsche 914-2
                                   1.0,  30.4,  1,        // 28 - Lotus Europa
                                   1.0,  15.8,  0,        // 29 - Ford Pantera L
                                   1.0,  19.7,  0,        // 30 - Ferrari Dino
                                   1.0,  15.0,  0,        // 31 - Maserati Bora
                                   1.0,  21.4,  1)        // 32 - Volvo 142E

} // ExampleMtcars object

