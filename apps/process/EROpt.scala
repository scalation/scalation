
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Oct  9 21:24:35 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes EROpt.scala
 *  @run     scala -cp ../../classes:classes process.EROpt
 */

package process

import scalation.process._
import scalation.linalgebra.VectorD
import scalation.linalgebra_gen.Vectors.VectorI
import scalation.minima.IntegerLocalSearch
//import scalation.minima.IntegerNLP
import scalation.random.{Uniform, Variate}
import scalation.util.Monitor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object performs Simulation Optimization (SO) on an Emergency Room (ER) model
 *  to find the numbers of nurses and doctors that mimimize the overall cost function.
 *  Cost is based on the daily pay for a nurse (8 hours * 30 $ per hour), a doctor
 *  (8 hours * 60 $ per hour) and a cost based on customer wait time ($10 per minute
 *  of mean waiting time).
 */
object EROpt extends App
{
    val nStop   = 100                             // simulation stopping rule (100 patients)
    val cost    = VectorD (240., 480., 10.)       // cost coefficient vector
    val PENALTY = 1.E8                            // penalty for infeasibility (e.g., -1 tellers)
    var bm: ERModelOpt = null                     // instance of an ER model

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Computation of the objective function from the simulation results.  This
     *  involves (1) extracting and checking input parameters, (2) executing the
     *  simulation model and (3) performing cost analysis.
     *  @see "SoPT: Ontology for Simulation Optimization for Scientific Experiments"
     *  @param x  the vector of input parameters
     */
    def f (x: VectorI): Double =
    {
        val nurses = x(0)
        if (nurses < 1) return PENALTY * (1 - nurses)     // return based on penalty
        val doctors = x(1)
        if (doctors < 1) return PENALTY * (1 - doctors)   // return based on penalty
        bm = new ERModelOpt ("bank", nStop, Uniform (2000, 4000), nurses, doctors,
                             Uniform (7000, 9000), Uniform (5000, 7000), Uniform (900, 1100))
        val results  = bm.simulate ()
        val waitTime = results(2).mean + results(5).mean
        val response = VectorD (nurses, doctors, waitTime)
        val total = cost dot response                                 // compute overall cost
        println ("---------------------------------------------------------------")
        println ("simulated an ER with " + nurses + " nurses, " + doctors +  " doctors, cost = " + total)
        println ("---------------------------------------------------------------")
        total
    } // f

    val optimizer = new IntegerLocalSearch (f)
//  val optimizer = new IntegerNLP (f, x0.dim)
    val x0 = new VectorI (2); x0.set (1)
    val result = optimizer.solve (x0)
    println ("###############################################################")
    println ("optimal solution x = " + result)
    println ("###############################################################")

} // EROpt


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class defines a simple process-interaction model of an Emergenct Room (ER)
 *  model where service is provided by one or more nurses and one or more doctors.
 *  A patient will first see a nurse and then a doctor.
 *  @param name        the name of the ER model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param nurses      the number of nurses (service units)
 *  @param doctors     the number of doctors (service units)
 *  @param nurseRV     the nurse service time distribution
 *  @param doctorRV    the doctor service time distribution
 *  @param moveRV      the time distribution for motion along transports
 */
class ERModelOpt (name: String, nArrivals: Int, iArrivalRV: Variate, nurses: Int, doctors: Int,
                  nurseRV: Variate, doctorRV: Variate, moveRV: Variate)
      extends Model (name, false)
{
    Monitor.traceOff ()

    val entry     = new Source ("entry", this, Patient, nArrivals, iArrivalRV, (70., 185.))
    val nurseQ    = new WaitQueue ("nurseQ", (200., 190.))
    val nurse     = new Resource ("nurse", nurseQ, nurses, nurseRV, (270., 185.))
    val doctorQ   = new WaitQueue ("doctorQ", (410., 190.))
    val doctor    = new Resource ("doctor", doctorQ, doctors, doctorRV, (480., 190.))
    val door      = new Sink ("door", (620., 185.))
    val toNurseQ  = new Transport ("toNurseQ", moveRV, entry, nurseQ)
    val toDoctorQ = new Transport ("toDoctorQ", moveRV, nurse, doctorQ)
    val toDoor    = new Transport ("toDoor", moveRV, doctor, door)

    addComponents (List (entry, nurseQ, nurse, doctorQ, doctor, door, toNurseQ, toDoctorQ, toDoor))

    case class Patient () extends SimActor ("p", this)
    {
        def act ()
        {
            toNurseQ.move ()
            if (nurse.busy) nurseQ.waitIn ()
            nurse.utilize ()
            nurse.release ()
            toDoctorQ.move ()
            if (doctor.busy) doctorQ.waitIn ()
            doctor.utilize ()
            doctor.release ()
            toDoor.move ()
            door.leave ()
        } // act

    } // Patient

} // ERModelOpt class

