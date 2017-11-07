
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Oct  9 21:24:35 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.process._
import scalation.linalgebra.{VectorD, VectorI}
import scalation.minima.IntegerLocalSearch
//import scalation.minima.IntegerNLP
import scalation.random.{Uniform, Variate}
import scalation.util.Monitor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ERoomOpt` object performs Simulation Optimization 'SO' on an Emergency Room 'ER'
 *  model to find the numbers of nurses and doctors that minimize the overall cost function.
 *  Cost is based on the daily pay for a nurse (8 hours * 30 dollars per hour), a doctor
 *  (8 hours * 60 dollars per hour) and a cost based on customer wait time (10 dollars
 *  per minute of mean waiting time).
 *  > run-main apps.process.ERoomOpt
 */
object ERoomOpt extends App
{
    val nStop   = 100                             // simulation stopping rule (100 patients)
    val cost    = VectorD (240.0, 480.0, 10.0)    // cost coefficient vector
    val PENALTY = 1.0E8                           // penalty for infeasibility (e.g., -1 tellers)
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
        if (nurses < 1) return PENALTY * (1 - nurses)         // return based on penalty
        val doctors = x(1)
        if (doctors < 1) return PENALTY * (1 - doctors)       // return based on penalty
        bm = new ERModelOpt ("eroom", nStop, Uniform (2000, 4000), nurses, doctors,
                             Uniform (7000, 9000), Uniform (5000, 7000), Uniform (900, 1100))
        bm.simulate ()
        val results  = bm.getStatistics
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

} // ERoomOpt object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ERModelOpt` class defines a simple process-interaction model of an Emergency
 *  Room 'ER' model where service is provided by one or more nurses and one or more doctors.
 *  A patient will first see a nurse and then a doctor.
 *  @param name        the name of the 'ER' model
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
      extends Model (name, 1, false)
{
    Monitor.traceOff ()

    val entry     = Source ("entry", this, Patient, 0, nArrivals, iArrivalRV, (70, 285))
    val nurseQ    = WaitQueue ("nurseQ", (200, 290))
    val nurse     = Resource ("nurse", nurseQ, nurses, nurseRV, (270, 285))
    val doctorQ   = WaitQueue ("doctorQ", (410, 290))
    val doctor    = Resource ("doctor", doctorQ, doctors, doctorRV, (480, 290))
    val door      = Sink ("door", (620, 285))
    val toNurseQ  = new Transport ("toNurseQ", entry, nurseQ, moveRV)
    val toDoctorQ = new Transport ("toDoctorQ", nurse, doctorQ, moveRV)
    val toDoor    = new Transport ("toDoor", doctor, door, moveRV)

    addComponent (entry, nurseQ, nurse, doctorQ, doctor, door, toNurseQ, toDoctorQ, toDoor)

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

