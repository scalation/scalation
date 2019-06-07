
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Dec  9 15:22:42 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/frames/frame.html
 *  @see Reference: The official NBA basketball Encyclopedia, Villard Books
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleBasketBall` class stores a medium-sized example dataset with data
 *  about basketball player that can be used to predict their scoring average.
 */
object ExampleBasketBall
{
    // x0 = height in feet
    // x1 = weight in pounds
    // x2 = percent of successful field goals (out of 100 attempted)
    // x3 = percent of successful free throws (out of 100 attempted)
    // y  = average points scored per game

    //                              x1,    x2,      x3,      x4,      y
    val xy = new MatrixD ((54, 5), 6.8,   225,   0.442,   0.672,    9.2,
                                   6.3,   180,   0.435,   0.797,   11.7,
                                   6.4,   190,   0.456,   0.761,   15.8,
                                   6.2,   180,   0.416,   0.651,    8.6,
                                   6.9,   205,   0.449,   0.900,   23.2,
                                   6.4,   225,   0.431,   0.780,   27.4,
                                   6.3,   185,   0.487,   0.771,    9.3,
                                   6.8,   235,   0.469,   0.750,   16.0,
                                   6.9,   235,   0.435,   0.818,    4.7,
                                   6.7,   210,   0.480,   0.825,   12.5,
                                   6.9,   245,   0.516,   0.632,   20.1,
                                   6.9,   245,   0.493,   0.757,    9.1,
                                   6.3,   185,   0.374,   0.709,    8.1,
                                   6.1,   185,   0.424,   0.782,    8.6,
                                   6.2,   180,   0.441,   0.775,   20.3,
                                   6.8,   220,   0.503,   0.880,   25.0,
                                   6.5,   194,   0.503,   0.833,   19.2,
                                   7.6,   225,   0.425,   0.571,    3.3,
                                   6.3,   210,   0.371,   0.816,   11.2,
                                   7.1,   240,   0.504,   0.714,   10.5,
                                   6.8,   225,   0.400,   0.765,   10.1,
                                   7.3,   263,   0.482,   0.655,    7.2,
                                   6.4,   210,   0.475,   0.244,   13.6,
                                   6.8,   235,   0.428,   0.728,    9.0,
                                   7.2,   230,   0.559,   0.721,   24.6,
                                   6.4,   190,   0.441,   0.757,   12.6,
                                   6.6,   220,   0.492,   0.747,    5.6,
                                   6.8,   210,   0.402,   0.739,    8.7,
                                   6.1,   180,   0.415,   0.713,    7.7,
                                   6.5,   235,   0.492,   0.742,   24.1,
                                   6.4,   185,   0.484,   0.861,   11.7,
                                   6.0,   175,   0.387,   0.721,    7.7,
                                   6.0,   192,   0.436,   0.785,    9.6,
                                   7.3,   263,   0.482,   0.655,    7.2,
                                   6.1,   180,   0.340,   0.821,   12.3,
                                   6.7,   240,   0.516,   0.728,    8.9,
                                   6.4,   210,   0.475,   0.846,   13.6,
                                   5.8,   160,   0.412,   0.813,   11.2,
                                   6.9,   230,   0.411,   0.595,    2.8,
                                   7.0,   245,   0.407,   0.573,    3.2,
                                   7.3,   228,   0.445,   0.726,    9.4,
                                   5.9,   155,   0.291,   0.707,   11.9,
                                   6.2,   200,   0.449,   0.804,   15.4,
                                   6.8,   235,   0.546,   0.784,    7.4,
                                   7.0,   235,   0.480,   0.744,   18.9,
                                   5.9,   105,   0.359,   0.839,    7.9,
                                   6.1,   180,   0.528,   0.790,   12.2,
                                   5.7,   185,   0.352,   0.701,   11.0,
                                   7.1,   245,   0.414,   0.778,    2.8,
                                   5.8,   180,   0.425,   0.872,   11.8,
                                   7.4,   240,   0.599,   0.713,   17.1,
                                   6.8,   225,   0.482,   0.701,   11.6,
                                   6.8,   215,   0.457,   0.734,    5.8,
                                   7.0,   230,   0.435,   0.764,    8.3)

    val (x, y) = PredictorMat.pullResponse (xy)
    val fname  = Array ("height", "weight", "field goal percentage", "free throw percentage")
    val _1     = VectorD.one (x.dim1)                              // column of all ones
    val oxy    = _1 +^: xy                                         // prepend a column of all ones to xy
    val ox     = _1 +^: x                                          // prepend a column of all ones to x
 
} // ExampleBasketBall object

