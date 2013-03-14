
class WaitQueue extends MainFrame with Actor
{
    private val frameSize = new Dimension (700, 600)

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * The canvas Panel is used to place shapes in the drawing region.
     */
    val canvas = new Panel
    {
        background    = bgColor
        preferredSize = frameSize

        /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         * Paint the display panel component.
         * @param g2d  the high-resolution Graphics context
         */
        override def paintComponent (g2d: Graphics2D)
        {
            super.paintComponent (g2d)
            g2d.setPaint (red)
            queue.setFrame (qPos.x, qPos.y



} // WaitQueue
