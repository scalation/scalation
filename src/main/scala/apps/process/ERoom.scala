
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Oct  9 21:24:35 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.process._
import scalation.random.{Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ERoom` object runs the `ERModel`.
 */
object ERoom extends App
{
    val nStop   = 50                              // simulation stopping rule (50 patients)
    val nurses  = 3                               // number of nurses
    val doctors = 2                               // number of doctors

    val bm = new ERModel ("bank", nStop, Uniform (2000, 4000), nurses, doctors,
                          Uniform (7000, 9000), Uniform (5000, 7000), Uniform (900, 1100))
    val results  = bm.simulate ()

} // ERoom object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ERModel` class defines a simple process-interaction model of an Emergenct Room (ER)
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
    val entry     = Source ("entry", this, Patient, 0, nArrivals, iArrivalRV, (70, 340))
    val nurseQ    = WaitQueue ("nurseQ", (230, 340))
    val nurse     = Resource ("nurse", nurseQ, nurses, nurseRV, (250, 335))
    val doctorQ   = WaitQueue ("doctorQ", (440, 340))
    val doctor    = Resource ("doctor", doctorQ, doctors, doctorRV, (460, 335))
    val door      = Sink ("door", (640, 340))
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

} // ERModel class

