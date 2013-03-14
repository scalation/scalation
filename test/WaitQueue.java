
 
/******************************************************************
 * @(#) WaitQueue.java     1.0
 *
 * Copyright (c) 2011 John Miller
 * All Right Reserved
 *-----------------------------------------------------------------
 * Permission to use, copy, modify and distribute this software and
 * its documentation without fee is hereby granted provided that
 * this copyright notice appears in all copies.
 * WE MAKE NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY
 * OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
 * LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT. WE SHALL NOT BE
 * LIABLE FOR ANY DAMAGES SUFFERED BY ANY USER AS A RESULT OF USING,
 * MODIFYING OR DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 *-----------------------------------------------------------------
 *
 * @version     1.0, 7 November 2011
 * @author      John Miller
 */

import static java.lang.System.*;
import java.awt.*;
import java.awt.geom.*;
import javax.swing.*;

/******************************************************************
 * The WaitQueue class shows a queue with a varying number waiting.
 */
public class WaitQueue extends JFrame
                   implements Runnable
{
    private static final Dimension   dim   = new Dimension (640, 350);
    private static final int         tau   = 500;
    private final Point2D.Double     quPos = new Point2D.Double (300, 200);
    private final Rectangle2D.Double queue = new Rectangle2D.Double ();
    private final Rectangle2D.Double strip = new Rectangle2D.Double ();
    private final Thread             displayer;
    private int                      l_q = 0;

    /**************************************************************
     * The DisplayPanel inner class is used to place shapes in the
     * drawing region.
     */
    public class DisplayPanel extends JPanel
    {
        /**********************************************************
         * Paint the display panel component.
         * @param gr  the graphics context.
         */
        public void paintComponent (Graphics gr)
        {
            super.paintComponent (gr);
            Graphics2D gr2 = (Graphics2D) gr;   // use hi-res

            queue.setFrame (quPos.x, quPos.y, 50, 20);
            gr2.setPaint (Color.red);
            gr2.fill (queue);

            int w = 3 * l_q;
            strip.setFrame (quPos.x + 50 - w, quPos.y + 5, w, 10);
            gr2.setPaint (Color.black);
            gr2.fill (strip);
        } // paintComponent

    } // DisplayPanel inner class

    /**************************************************************
     * Construct an animator frame.
     * @param title  title of frame
     */
    public WaitQueue (String title)
    {
        super (title);
        setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE);

        getContentPane ().add (new DisplayPanel ());
        setLocation (100, 100);
        setSize (dim);
        setVisible (true);

        displayer = new Thread (this);
        displayer.start ();
    } // WaitQueue

    /**************************************************************
     * Run method for the display thread.
     * Repeatedly update the number in the queue (l_q), sleep and repaint.
     */
    public void run ()
    {
        boolean up = true;
        for (int i = 0; i < 100; i++) {
            if (up) {
                l_q++;
                if (l_q == 15) up = false;
            } else {
                l_q--;
                if (l_q == 0) up = true;
            } // if
      
            try {
                displayer.sleep (tau);
            } catch (InterruptedException ex) {
                out.println ("WaitQueue.run: sleep failed");
            } // try

            repaint ();
        } // for
    } // run

    /**************************************************************
     * Main method for invoking the WaitQueue animation.
     * @param  args  command-line arguments
     */
    public static void main (String [] args)
    {
        new WaitQueue ("WaitQueue");
    } // main

} // WaitQueue class

