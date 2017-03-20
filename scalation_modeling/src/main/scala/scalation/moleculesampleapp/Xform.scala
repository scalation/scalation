
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun May 17 15:07:22 EDT 2015
 *  @see     LICENSE (MIT style license file)
 *           Also see Oracle Copyright below
 *
 *  Translated to Scala from MoleculeSampleApp
 *  @see https://docs.oracle.com/javase/8/javafx/graphics-tutorial/sampleapp3d.htm
 *  @see https://docs.oracle.com/javase/8/javafx/graphics-tutorial/sampleapp3d-code.htm
 */

/*
 * Copyright (c) 2013, 2014 Oracle and/or its affiliates.
 * All rights reserved. Use is subject to license terms.
 *
 * This file is available and licensed under the following license:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *  - Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the distribution.
 *  - Neither the name of Oracle nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES LOSS OF USE,
 * DATA, OR PROFITS OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scalation.moleculesampleapp

import javafx.scene.Group
import javafx.scene.transform.{Rotate, Scale, Translate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RotateOrder` object enumerates possible rotation orders, e.g.,
 *  XYZ mean rotate on x-axis, then y-axis and then on z-axis.
 */
object RotateOrder extends Enumeration
{
    val XYZ, XZY, YXZ, YZX, ZXY, ZYX = Value

} // RotateOrder object

import RotateOrder._


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Xform` class supports custom transforms (Translate, Rotate and Scale).
 */
class Xform extends Group
{
    val t  = new Translate () 
    val p  = new Translate () 
    val ip = new Translate () 
    val rx = new Rotate (); rx.setAxis (Rotate.X_AXIS)
    val ry = new Rotate (); ry.setAxis (Rotate.Y_AXIS)
    val rz = new Rotate (); rz.setAxis (Rotate.Z_AXIS)
    val s  = new Scale ()
    var rotateOrder: RotateOrder.Value = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This aux. constructor allows a rotation order to be specified.
     *  @param  _rotateOrder  the rotation order
     */
    def this (_rotateOrder: RotateOrder.Value)
    {
        this ()
        rotateOrder = _rotateOrder
    } // aux. constructor

    if (rotateOrder == null) {
        getTransforms ().addAll (t, rz, ry, rx, s) 
    } else {   
        rotateOrder match {       // choose the order of rotations based on rotateOrder
        case XYZ => getTransforms ().addAll (t, p, rz, ry, rx, s, ip) 
        case XZY => getTransforms ().addAll (t, p, ry, rz, rx, s, ip) 
        case YXZ => getTransforms ().addAll (t, p, rz, rx, ry, s, ip) 
        case YZX => getTransforms ().addAll (t, p, rx, rz, ry, s, ip)  // For Camera
        case ZXY => getTransforms ().addAll (t, p, ry, rx, rz, s, ip) 
        case ZYX => getTransforms ().addAll (t, p, rx, ry, rz, s, ip) 
        case _   =>  println ("Xform.constructor: unrecognized rotation order")
        } // match
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get rotation object.
     */
    def getRx: Rotate = rx
    def getRy: Rotate = ry
    def getRz: Rotate = rz

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set translation.
     */
    def setTranslate (x: Double, y: Double, z: Double) { t.setX (x); t.setY (y); t.setZ (z) }
    def setTranslate (x: Double, y: Double) { t.setX (x); t.setY (y) }
    def setTx (x: Double) { t.setX (x) }
    def setTy (y: Double) { t.setY (y) }
    def setTz (z: Double) { t.setZ (z) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set rotation.
     */
    def setRotate (x: Double, y: Double, z: Double) { rx.setAngle(x); ry.setAngle(y); rz.setAngle(z) }
    def setRx (x: Double) { rx.setAngle (x) }
    def setRy (y: Double) { ry.setAngle (y) }
    def setRz (z: Double) { rz.setAngle (z) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set scaling.
     */
    def setScale (scaleFactor: Double) { s.setX (scaleFactor); s.setY (scaleFactor); s.setZ (scaleFactor) }
    def setScale (x: Double, y: Double, z: Double) { s.setX (x); s.setY (y); s.setZ (z) }
    def setSx (x: Double) { s.setX (x) }
    def setSy (y: Double) { s.setY (y) }
    def setSz (z: Double) { s.setZ (z) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the pivot.
     */
    def setPivot (x: Double, y: Double, z: Double) { p.setX (x);   p.setY (y);   p.setZ (z)
                                                     ip.setX (-x); ip.setY (-y); ip.setZ (-z) } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset all transforms to defaults.
     */
    def reset ()
    {
        t.setX (0.0);      t.setY (0.0);      t.setZ (0.0)
        rx.setAngle (0.0); ry.setAngle (0.0); rz.setAngle (0.0)
        s.setX (1.0);      s.setY (1.0);      s.setZ (1.0)
        p.setX (0.0);      p.setY (0.0);      p.setZ (0.0)
        ip.setX (0.0);     ip.setY (0.0);     ip.setZ (0.0)
    } // reset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset t, s, p (ip) transforms to defaults.
     */
    def resetTSP ()
    {
        t.setX (0.0);  t.setY (0.0);  t.setZ (0.0)
        s.setX (1.0);  s.setY (1.0);  s.setZ (1.0)
        p.setX (0.0);  p.setY (0.0);  p.setZ (0.0)
        ip.setX (0.0); ip.setY (0.0); ip.setZ (0.0)
    } // resetTSP

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert Xform to a string.
     */
    override def toString: String =
    {
        "Xform[t = (" + t.getX() + ", " + t.getY() + ", " + t.getZ() + ")  " +
              "r = (" + rx.getAngle() + ", " + ry.getAngle() + ", " + rz.getAngle() + ")  " +
              "s = (" + s.getX() + ", " + s.getY() + ", " + s.getZ() + ")  " +
              "p = (" + p.getX() + ", " + p.getY() + ", " + p.getZ() + ")  " +
              "ip = (" + ip.getX() + ", " + ip.getY() + ", " + ip.getZ() + ")]"
    } // toString

} // Xform class

