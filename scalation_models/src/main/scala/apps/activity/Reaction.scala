
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Nov  1 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package apps.activity

import scalation.activity._
import scalation.dynamics.Derivatives.Derivative
import scalation.linalgebra.VectorD
import scalation.random.Sharp
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Reaction` object models a simple biochemical reaction.  A glycan will pick
 *  up a new glycan residue to form another glycan.  The reaction will be catalyzed
 *  by an protein enzyme.
 *  @see scalation.activity.PetriNetTest, scalation.activity.PetriNetRulesTest
 *  for more examples of test code.
 *  > runMain apps.activity.Reaction
 */
object Reaction extends App
{
    //:: Set up the colors for fluids (green for glycans, blue for enzymes)

    val colors = Array [Color] (green, blue)

    //:: Define the places along with their initial markings by color.

    val place = Array [PlaceD] (new PlaceD (100, 250, VectorD (20.0,  0.0)),
                                new PlaceD (200, 350, VectorD ( 0.0, 10.0)),
                                new PlaceD (500, 250, VectorD ( 0.0,  0.0)))

    //:: Define the transitions.

    val transt = Array [Transition] (new Transition (300, 240, Sharp (4), colors))

    //:: Define the overall Petri net.

    val pnet = new PetriNet (colors, place, transt)

    //:: Define the derivative for the ODE that governs inflow into transistions.

    def derv1 (t: Double, y: Double) = .1 * y
    def derv2 (t: Double, y: Double) = 1.0

    //:: For each transition, link to all of the incoming/outgoing places via true/false arcs.
    //:: Also, establish a back link to the containing Petri net.

    transt(0).connect (pnet,
        Array [ArcD] (new ArcD (place(0), transt(0), true,  VectorD (0.0, 0.0), null, Array [Derivative] (derv1, derv2)),
                      new ArcD (place(1), transt(0), true,  VectorD (0.0, 10.0))),
        Array [ArcD] (new ArcD (place(1), transt(0), false, VectorD (0.0, 10.0)),
                      new ArcD (place(2), transt(0), false, VectorD (10.0, 0.0))))

    println (pnet)
    pnet.simulate (2, 20)

} // Reaction object

