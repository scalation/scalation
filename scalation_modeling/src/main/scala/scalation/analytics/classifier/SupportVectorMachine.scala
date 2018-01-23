
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Naman Fatehpuria
 *  @version 1.4
 *  @date    Mon Mar  3 14:39:17 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     fbim.fh-regensburg.de/~saj39122/Diplomarbeiten/Miklos/Papers/Keerthi%20improvement%20on%20SMO.pdf
 */

package scalation.analytics.classifier

import scala.collection.mutable.Set
import scala.math.abs
import scala.util.control.Breaks

import scalation.linalgebra.{MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SupportVectorMachine` class is a translation of Pseudo-Code from a
 *  modified SMO (Modification 2) found at the above URL's into Scala and includes
 *  a few simplifications (e.g., currently only works for linear kernels, dense
 *  data and binary classification).
 *  @param x   the training data points stored as rows in a matrix
 *  @param y   the classification of the data points stored in a vector
 *  @param fn  the factor names
 *  @param cn  the class names
 */
class SupportVectorMachine (x: MatrixD, y: VectoI, fn: Array [String] = Array (), cn: Array [String] = Array ("-", "+"))
      extends ClassifierReal (x, y, fn, 2, cn)
{
    type Pair = Tuple2 [Double, Double]
    
    private val DEBUG   = true               // debug flag
    private val EPSILON = 1E-3               // a number close to zero
    private val TOL     = 1E-3               // tolerance level
    private val C       = 0.05               // crossing penalty
    private val alp     = new VectorD (m)    // alpha (Lagrange multipliers)
    private val fCache  = new VectorD (m)    // errors for all non-bound examples
    private val w       = new VectorD (n)    // weights
    
    private var I_0     = Set [Int] ()       // {i: 0 < alp(i) < C}
    private var I_1     = Set [Int] ()       // {i: y(i) =  1, alp(i) = 0}
    private var I_2     = Set [Int] ()       // {i: y(i) = -1, alp(i) = C}
    private var I_3     = Set [Int] ()       // {i: y(i) =  1, alp(i) = C}
    private var I_4     = Set [Int] ()       // {i: y(i) = -1, alp(i) = 0}
    
    private var b       =  0.0               // threshold
    private var b_Low   =  1.0               // lower threshold
    private var b_Up    = -1.0               // upper threshold
    private var i_Low   = -1                 // index for b_Low
    private var i_Up    = -1                 // index for b_Up
    
    private var al1     = 0.0                // old Lagrange multiplier 1
    private var a1      = 0.0                // new Lagrange multiplier 1
    private var al2     = 0.0                // old Lagrange multiplier 2
    private var a2      = 0.0                // new Lagrange multiplier 2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train uses SMO (Sequential Minimum Optimization) algorithm to solves the 
     *  optimization problem for the weight vector 'w' and the threshold 'b' for 
     *  the model '(w dot z) - b'.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int)         // FIX - use these parameters
    {
        // Fill Set I_1 and I_4, as initially alp[i] = 0
        // Initialize i_Up to any index of class +1 
        // Initialize i_Low to any index of class -1
        for (i <- 0 until m) {
            if (y(i) == 1) {
                I_1 += i; i_Up = i
            } else {
                I_4 += i; i_Low = i
            } // if
        } // for
        
        fCache (i_Low) =  1
        fCache (i_Up)  = -1
      
        var nChanged = 0
        var checkAll = true
        
        val loop = new Breaks
        
        while (nChanged > 0 || checkAll) {
            nChanged = 0
            if (checkAll) for (k <- 0 until m if checkExample (k)) nChanged += 1
            else {
                var success = true
                nChanged = 0
                while ( ( b_Up < b_Low - 2 * TOL ) && success ) {
                    success = takeStep (i_Up, i_Low)
                    if (success) nChanged += 1
                } // while
            } // if  
            if (checkAll) checkAll = false
            else if (nChanged == 0 ) checkAll = true
        } // while
        
        b = (b_Low + b_Up) / 2.0          
    } // train
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, determine which class it belongs to.
     *  Classify returns 1 meaning 'z' belongs to the positive class, while 
     *  -1 means it belongs to the negative class.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): (Int, String, Double) =
    {
        if ((w dot z) >= b) (1, "+", -1.0) else (-1, "-", -1.0)   // FIX - need metric
    } // classify
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, determine which class it belongs to.
     *  Classify returns 1 meaning 'z' belongs to the positive class, while 
     *  -1 means it belongs to the negative class.
     *  @param z  the vector to classify
     */
//  def classify (z: VectoI): Tuple2 [Int, String] =
//  {
//      val zd = new VectorD (z.dim)
//      for (j <- 0 until z.dim) zd(j) = z(j).toDouble
//      classify (zd)
//  } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        // FIX: to be implemented
    } // reset
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value using the learned function (assumes linear, dense).
     *  @param k  the row index into the data matrix
     */
    private def func (k: Int): Double = w dot x(k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value using the kernel function (assumes linear, dense).
     *  @param i1  the first row index into the data matrix
     *  @param i2  the second row index into the data matrix
     */
    private def kernel_func (i1: Int, i2: Int): Double = x(i1) dot x(i2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Optimize by replacing old values of Lagrange multipliers 'al1', 'al2' with
     *  new values 'a1' and 'a2.'
     *  @param i1  the index for the first Lagrange multipliers (alpha) 
     *  @param i2  the index for the second Lagrange multipliers (alpha)
     */
    private def takeStep (i1: Int, i2: Int): Boolean =
    {
        if (DEBUG) println ("takeStep (" + i1 + ", " + i2 + ")")
        if (i1 == i2) { println ("takeStep: skip if i1 == i2"); return false }
        
        al1 = alp (i1); al2 = alp (i2)
        var y1 = y (i1); var y2 = y (i2)
        var F1 = fCache (i1); var F2 = fCache (i2) 
        var s  = y1 * y2
        
        val (l, h) = computeLH (y1, y2)
        if (l == h) { println ("takeStep: skip if l == h"); return false }
        
        // compute eta
        val k11 = kernel_func (i1, i1)
        val k12 = kernel_func (i1, i2)
        val k22 = kernel_func (i2, i2)
        val eta = 2.0 * k12 - (k11 + k22)

        if (eta < 0.0) {
            a2 = al2 - y2 * (F1 - F2) / eta
            if (a2 < l) a2 = l else if (a2 > h) a2 = h
        } else {
            val c1 = eta / 2.0
            val c2 = y2 * (F1 - F2) - eta * al2
            val lObj = c1 * l * l + c2 * l
            val hObj = c1 * h * h + c2 * h
            a2 = if (lObj > hObj + EPSILON) l else if (lObj < hObj - EPSILON) h else al2
        } // if
        
        if (abs (a2 - al2) < EPSILON * (a2 + al2 + EPSILON)) {
            println ("takeStep: skip if a2 = " + a2 + " ~=  al2 = " + al2)      // almost no change
            return false
        } // if
        
        a1 = al1 + s * (al2 - a2)
        if (a1 < 0.0) {
            a2 += s * a1; a1 = 0
        } else if (a1 > C) {
            val t = a1 - C; a2 += s * t; a1 = C
        } // if
        
        update (i1, i2, y1, y2)                // weights and fCache
        alp (i1) = a1; alp (i2) = a2           // store a1, a2 in alp array
        
        // Compute update F values for i1 and i2.
        fCache(i1) = F1 + y1 * (a1 - al1) * k11 + y2 * (a2 - al2) * k12
        fCache(i2) = F2 + y1 * (a1 - al1) * k12 + y2 * (a2 - al2) * k22
        
        // Update I_0, I_1, I_2, I_3, I_4 for i1
        if (a1 > 0 && a1 < C)       I_0 += i1 else I_0 -= i1
        if (y(i1) == 1 && a1 == 0)  I_1 += i1 else I_1 -= i1
        if (y(i1) == -1 && a1 == C) I_2 += i1 else I_2 -= i1
        if (y(i1) == 1 && a1 == C)  I_3 += i1 else I_3 -= i1
        if (y(i1) == -1 && a1 == 0) I_4 += i1 else I_4 -= i1
        
        // Update I_0, I_1, I_2, I_3, I_4 for i2
        if (a2 > 0 && a2 < C)       I_0 += i2 else I_0 -= i2
        if (y(i2) == 1 && a2 == 0)  I_1 += i2 else I_1 -= i2
        if (y(i2) == -1 && a2 == C) I_2 += i2 else I_2 -= i2
        if (y(i2) == 1 && a2 == C)  I_3 += i2 else I_3 -= i2
        if (y(i2) == -1 && a2 == 0) I_4 += i2 else I_4 -= i2
        
        // Compute (i_Low, b_Low) and (i_Up, b_Up), using i1, i2 and indices in I_0
        var I = I_0
        I += (i1, i2)
        for (i <- I) {
            if (fCache (i) < b_Up) {
                b_Up = fCache (i)
                i_Up = i
            } // if
            if (fCache (i) > b_Low) {
                b_Low = fCache (i)
                i_Low = i
            } // if
        } // for
        true
    } // takeStep
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute 'l' and 'h'.
     *  @param y1  the first target value
     *  @param y2  the second target value
     */
    private def computeLH (y1: Int, y2: Int): Pair =
    {
        if (y1 == y2) {
            val gamma = al1 + al2
            return if (gamma > C) (gamma - C, C) else (0.0, gamma)
        } else {
            val gamma = al1 - al2
            return if (gamma > 0.0) (0.0, C - gamma) else (-gamma, C )
        } // if
    } // computeLH
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update weights 'w' and error cache 'fCache'.
     *  @param i1  the index for the first Lagrange multipliers (alpha) 
     *  @param i2  the index for the second Lagrange multipliers (alpha)
     *  @param y1  the first target value
     *  @param y2  the second target value
     */
    def update (i1: Int, i2: Int, y1: Int, y2: Int)
    {
        val t1 = y1 * (a1 - al1)
        val t2 = y2 * (a2 - al2)
        
        // Update weight vector to reflect change in a1 and a2
        for (j <- 0 until n) w(j) += x(i1, j) * t1 + x(i2, j) * t2

        // Update fCache(i) for i in I_0 using new Lagrange multipliers (a1 & a2)
        for (i <- I_0 if i != i1 && i != i2) { 
            fCache(i) += t1 * kernel_func (i1, i) + t2 * kernel_func (i2, i)
//          fCache(i) += (y1 * a1 * kernel_func (i1, i)) - y(i) + (y2 * a2 * kernel_func (i2, i)) - y(i)
        } // for
    } // update
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Using the index i2 from training data, update (b_Low, i_Low) or (b_Up, i_Up)
     *  Then check for optimality using current b_Low and b_Up and, if
     *  violated, find an index i1 to do joint optimization with i2.
     *  @param i2 second data point for optimization
     */
    private def checkExample (i2: Int): Boolean =
    {
        var i1 = -1
        var F2 = 0.0
        al2 = alp (i2)
        
        if (I_0 contains i2) F2 = fCache (i2)
        else {
            F2 = func (i2) - y (i2)
            fCache (i2) = F2

            // Update (b_Low, i_Low) or (b_Up, i_Up) using (F2, i2)
            if (((I_1 contains i2) || (I_2 contains i2)) && (F2 < b_Up)) {
                b_Up = F2
                i_Up = i2
            } else if (((I_3 contains i2) || (I_4 contains i2)) && (F2 > b_Low)) {
                b_Low = F2
                i_Low = i2
            } // if
        } // if
        
        // Check optimality using current b_Low and b_Up and, if
        // violated, find an index i1 to do joint optimization with i2.
        var optimality = true
        
        if ((I_0 contains i2) || (I_1 contains i2) || (I_2 contains i2)) {
            if (b_Low - F2 > 2 * TOL) {
                optimality = false
                i1 = i_Low
            } // if
        } // if

        if ((I_0 contains i2) || (I_3 contains i2) || (I_4 contains i2)) {
            if(F2 - b_Up > 2 * TOL) {
                optimality = false
                i1 = i_Up
            } // if
        } // if
        
        if (optimality) return false
        
        // For i2 in I_0 choose the better i1.
        if (I_0 contains i2) {
            if(b_Low - F2 > F2 - b_Up) i1 = i_Low
            else i1 = i_Up
        } // if
        
        takeStep (i1, i2)
    } // checkExample
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert svm to a string showing (w, b).
     */
    override def toString = "(w, b) = " + (w, b)

} // SupportVectorMachine


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SupportVectorMachineTest` is used to test the `SupportVectorMachine` class.
 *  > runMain scalation.analytics.classifier.SupportVectorMachineTest
 */
object SupportVectorMachineTest extends App
{
    // Test 1
    val x = new MatrixD ((4, 2), 1.0, 2.0,            // 4 data points
                                 2.0, 1.0,
                                 2.0, 3.0,
                                 3.0, 2.0)
    val y = VectorI (-1, -1, 1, 1)                    // classification of points
    val z = VectorD (4.0, 3.0)
    
    val svm   = new SupportVectorMachine (x, y)       // create optimizer
    svm.train ()
    println (svm)
    println ("classify (" + z + ") = " + svm.classify (z))

} // SupportVectorMachineTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SupportVectorMachineTest2` is used to test the `SupportVectorMachine` class.
 */
object SupportVectorMachineTest2 extends App
{
    // FIX - put in a larger test example

} // SupportVectorMachineTest2 object

