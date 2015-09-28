
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Aug 28 15:02:36 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import math.round

import scalation.linalgebra.{MatrixI, VectorI, VectorD}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DAG' class provides a data structure for storing directed acyclic graphs.
 *  @param par  records the parents for each node in the graph
 */
class DAG (val par: Array [Array [Int]])


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesNetwork` class implements a Bayesian Network Classifier.  It
 *  classifies a data vector 'z' by determining which of 'k' classes has the
 *  highest Joint Probability of 'z' and the outcome (i.e., one of the 'k'
 *  classes) of occurring.  The Joint Probability calculation is factored
 *  into multiple calculations of Conditional Probability.  Conditional
 *  dependencies are specified using a Directed Acyclic Graph (DAG).  Nodes
 *  are conditionally dependent on their parents only.  Conditional probability
 *  are recorded in tables.  Training is achieved by ...
 *  @param x      the integer-valued training/test data vectors stored as rows of a matrix
 *  @param y      the training/test classification vector, where y_i = class for row i of the matrix x
 *  @param fn     the names for all factors
 *  @param k      the number of classes
 *  @param cn     the names for all classes
 *  @param dag    the directed acyclic graph specifying conditional dependencies
 *  @param table  the array of tables recording conditional probabilities
 */
class BayesNetwork (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
                    dag: DAG = null, table: Array [Map [Int, Double]] = null)
      extends ClassifierInt (x, y, fn, k, cn)
{
    private val DEBUG = true                 // debug flag

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Joint Probability (JP) of vector 'x'  ('z' concat outcome).
     *  as the product of each of its element's conditional probability.
     *  @param x  the vector of variables
     */
    def jp (x: VectorI): Double =
    {
        var prod = 1.0
        for (i <- 0 until x.dim) prod *= cp (i, x.select (dag.par (i)) ++ x(i))
        prod
    } // jp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Conditional Probability (CP) of 'x_i' given its parents' values.
     *  @param i    the ith variable (whose conditional probability is sought)
     *  @param key  the values of x_i's parents and x_i
     */
    def cp (i: Int, key: VectorI): Double =
    {
        if (DEBUG) println ("cp: find key " + key + " hashCode = " + key.hashCode +
                            " in table " + i + ":\n" + table(i))
        table(i)(key.hashCode)
    } // cp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier, i.e., ...
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int)    // FIX - use these parameters
    {
        // FIX - to be implemented
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an integer-valued data vector 'z', classify it returning the class
     *  number (0, ..., k-1) with the highest relative posterior probability.
     *  @param z  the data vector to classify
     */
    override def classify (z: VectorI): Tuple2 [Int, String] =
    {
        val prob = new VectorD (k)
        val x    = new VectorI (z.dim + 1)
        for (i <- 0 until z.dim) x(i) = z(i)
        for (c <- 0 until k) { x(z.dim) = c; prob(c) = jp (x) }
        println ("prob = " + prob)
        val best = prob.argmax ()           // class with the highest relative posterior probability
        (best, cn(best))                    // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a continuous data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  The vector 'z' id first converted to an integer valued vector by rounding.
     *  @param z  the data vector to classify
     *
    def classify (z: VectorD): Tuple2 [Int, String] =
    {
        val z_int = new VectorI (z.dim)
        for (i <- 0 until z.dim) z_int(i) = round (z(i)).toInt
        classify (z_int)
    } // classify
     */

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        // FIX: to be implemented
    } // reset

} // BayesNetwork class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesNetworkTest` object is used to test the `BayesNetwork` class.
 *  Ex: Classify whether a person has a Back Ache.
 *  @see www.eng.tau.ac.il/~bengal/BN.pdf
 */
object BayesNetworkTest extends App
{
    // Compute P(C, S, W, B, A) = P(C) P(S) P(W|C) P(B|S,C) P(A|B)

    val dag = new DAG (Array (Array (),           // parents for C(0) = {}
                              Array (),           // parents for S(1) = {}
                              Array (0),          // patents for W(2) = {C}
                              Array (0, 1),       // patents for B(3) = {C, S}
                              Array (3)))         // patents for A(4) = {W}

    // Chair                   C
    val cTable = Map (VectorI (0).hashCode -> 0.20,
                      VectorI (1).hashCode -> 0.80)

    // Sport                   S
    val sTable = Map (VectorI (0).hashCode -> 0.98,
                      VectorI (1).hashCode -> 0.02)

    // Worker                  C  W
    val wTable = Map (VectorI (0, 0).hashCode -> 0.99,
                      VectorI (0, 1).hashCode -> 0.01,
                      VectorI (1, 0).hashCode -> 0.10,
                      VectorI (1, 1).hashCode -> 0.90)

    // Back                    C  S  B
    val bTable = Map (VectorI (0, 0, 0).hashCode -> 0.99,
                      VectorI (0, 0, 1).hashCode -> 0.01,
                      VectorI (0, 1, 0).hashCode -> 0.10,
                      VectorI (0, 1, 1).hashCode -> 0.90,
                      VectorI (1, 0, 0).hashCode -> 0.80,
                      VectorI (1, 0, 1).hashCode -> 0.20,
                      VectorI (1, 1, 0).hashCode -> 0.01,
                      VectorI (1, 1, 1).hashCode -> 0.90)

    // Ache                    B  A
    val aTable = Map (VectorI (0, 0).hashCode -> 0.90,
                      VectorI (0, 1).hashCode -> 0.10,
                      VectorI (1, 0).hashCode -> 0.30,
                      VectorI (1, 1).hashCode -> 0.70)

    val table = Array (cTable, sTable, wTable, bTable, aTable)

    val fn = Array ("Chair", "Sport", "Worker", "Back", "Ache")
    val cn = Array ("No", "Yes")
                      
    println ("---------------------------------------------------------------")

    val bn = new BayesNetwork (null, null, fn, 2, cn, dag, table)

    // train the classifier ---------------------------------------------------
    bn.train ()

    // test sample ------------------------------------------------------------
    //                 C  S  W  B  A
    val x1 =  VectorI (1, 1, 0, 0, 1)                        // compute JP for vector x1
    val x2 =  VectorI (1, 1, 0, 0, 0)                        // compute JP for vector x2
    val z  =  VectorI (1, 1, 0, 0)                           // data vector z to classify
    println ("--- JP       " + x1 + " = " + bn.jp (x1) + "\n")
    println ("--- JP       " + x2 + " = " + bn.jp (x2) + "\n")
    println ("--- classify " + z +  " = " + bn.classify (z) + "\n")

} // BayesNetworkTest object

