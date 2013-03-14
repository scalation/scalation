
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Oct  9 21:24:35 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes ERoom.scala
 *  @run     scala -cp ../../classes:classes process.ERoom
 */

package process

import scalation.process._
import scalation.random.{Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object runs the ERModel.
 */
object ERoom extends App
{
    val nStop   = 100                             // simulation stopping rule (100 patients)
    val nurses  = 3                               // number of nurses
    val doctors = 2                               // number of doctors

    val bm = new ERModel ("bank", nStop, Uniform (2000, 4000), nurses, doctors,
                          Uniform (7000, 9000), Uniform (5000, 7000), Uniform (900, 1100))
    val results  = bm.simulate ()

} // ERoom


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
class ERModel (name: String, nArrivals: Int, iArrivalRV: Variate, nurses: Int, doctors: Int,
                  nurseRV: Variate, doctorRV: Variate, moveRV: Variate)
      extends Model (name)
{
    val entry     = new Source ("entry", this, Patient, nArrivals, iArrivalRV, (70., 185.))
    val nurseQ    = new WaitQueue ("nurseQ", (200., 190.))
    val nurse     = new Resource ("nurse", nurseQ, nurses, nurseRV, (270., 185.))
    val doctorQ   = new WaitQueue ("doctorQ", (410., 190.))
    val doctor    = new Resource ("doctor", doctorQ, doctors, doctorRV, (480., 185.))
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

} // ERModel class

