
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package apps.database

import scalation.columnar_db.Relation
import scalation.linalgebra.{MatrixD, VectorI, VectorS}
import scalation.maxima.Hungarian
import scalation.random.{Randi0, Random}
import scalation.util.EasyWriter

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClassSchedule` object is an app used for scheduling classes for a 2 year
 *  cycle of four semesters, excluding Summer semesters.  It illustrates the
 *  capabilties of the ScalaTion analytics database.
 *  > runMain apps.relalgebra.ClassSchedule
 */
object ClassSchedule extends App
{
    val course     = Relation ("csci_course")
    val teacher    = Relation ("csci_teacher")
    val preference = Relation ("csci_preference")

    course.show ()
    teacher.show ()
    preference.show ()

//  course.writeCSV ("csci_courses.csv")
//  teacher.writeCSV ("csci_teachers.csv")
//  preference.writeCSV ("csci_preferences.csv")

    val course_cid       = course.toVectorI (0)
    val course_frequency = course.toVectorI (5)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a four semester class schedule.
     */
    def buildSchedule ()
    {
        val coin = Random ()
        val skip = Randi0 (3)
        val ew   = new EasyWriter ("scheduling", "csci_schedule.csv")

        ew.println ("Fall16, Spring17, Fall17, Spring18")
        for (i <- course_cid.indices) {
            val offerings = course_frequency(i)
            for (j <- 0 until course_frequency(i)) {
                if (offerings == 1) {
                    for (k <- 0 until skip.igen) ew.print (", ")
                } else if (offerings == 2) {
                    if (coin.gen < 0.5) ew.print (", ")
                } // if
                ew.print (course_cid(i) + ", ")
                if (j % 4 == 3 && j != offerings-1) ew.println ()
            } // for
            ew.println ()
        } // for
        ew.close ()
    } // buildSchedule

    val course_id        = VectorI.range (0, course.rows)
    val course_instance  = VectorI ((for (i <- course_id; j <- 0 until course_frequency(i)) yield i).toSeq)

    val teacher_id       = VectorI.range (0, teacher.rows)
    val teacher_name     = teacher.toVectorS (0)
    val teacher_load     = teacher.toVectorI (1)
    val teacher_instance = VectorI ((for (i <- teacher_id; j <- 0 until teacher_load(i)) yield i).toSeq)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign teachers to course offerings using the Hungarian bipartite graph
     *  matching algorithm to maximize their perferences.
     */
    def assignTeachers ()
    {
        println ("teacher_instance = " + teacher_instance)
        println ("course_instance = " + course_instance)
        val score = preference.toMap (Seq ("name", "cid"), "level")
        println ("score = " + score)

        val cost = new MatrixD (course_instance.dim, teacher_instance.dim)
        for (c <- course_instance.indices; t <-teacher_instance.indices) {
            cost(c, t) = score.getOrElse (Seq (course_cid(course_instance(c)),
                                               teacher_name(teacher_instance(t))), 0).asInstanceOf [Int].toDouble
        } // for

        println ("cost = " + cost)
        val ap = new Hungarian (cost)
        println ("optimal cost = " + ap.solve ())
    } // assignTeachers

    buildSchedule ()
    assignTeachers ()

} // ClassSchedule object

