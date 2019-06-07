
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Vamsi Nadella, John Miller
 *  @version 1.6
 *  @date    Sat May 26 12:25:41 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

//  U N D E R   D E V E L O P M E N T

package scalation.image

import java.awt.image.BufferedImage
import java.awt.{Color, color, FlowLayout}
import java.io.File

import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JFrame, JLabel}

import scala.math.abs

import scalation.linalgebra._
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImageCV` class ...
 *  @param path  ...
 */
class ImageCV (path: String)
{
    private var buffImage   = getBuffImage                     // buffered image
    private var imageWidth  = buffImage.getWidth               // the image width
    private var imageHeight = buffImage.getHeight              // the image height
    private var rgbMatrix   = getRgbMatrix                     // the RGB matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /* Reset the variable derived from 'buffImage'.
     */
    def reset ()
    {
        imageWidth  = buffImage.getWidth
        imageHeight = buffImage.getHeight
        rgbMatrix   = getRgbMatrix
    } // reset 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /* Return the buffered image object on the path.
     */
    def getBuffImage: BufferedImage = ImageIO.read (new File (path))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a matrix of RGB values of size (imageWidth, imageHeight).
     */
    def getRgbMatrix: MatriI =
    {
        val pixels = Array.ofDim [Int] (imageWidth * imageHeight)
        toMatrix (buffImage.getRGB (0, 0, imageWidth, imageHeight, pixels, 0, imageWidth),
                  imageHeight, imageWidth)
    } // getRgbMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return values from red channel from the image.  FIX.
     */
    def getRed: MatriI =
    {
        val red = rgbMatrix.map (arr => arr.map (x => new Color(x).getRed))
        new MatrixI (red)
    } // getRed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return values from green channel from the image.  FIX.
     */
    def getGreen: MatriI =
    {
        val green = rgbMatrix.map (arr => arr.map (x => new Color(x).getGreen))
        new MatrixI (green)
    } // getGreen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return values from blue channel from the image.  FIX.
     */
    def getBlue: MatriI =
    {
        val blue = rgbMatrix.map (arr => arr.map (x => new Color(x).getBlue))
        new MatrixI (blue)
    } // getBlue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return values from alpha channel from the image.  FIX.
     */
    def getAlpha: MatriI =
    {
        val alpha = rgbMatrix.map (arr => arr.map (x => new Color(x).getAlpha))
        new MatrixI (alpha)
    } // getAlpha

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the height of the image.
     */
    def height: Int = imageHeight

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the width of the image.
     */
    def width: Int = imageWidth

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the image into a gray scale image and assign it to 'buffImage'.
     *  @see en.wikipedia.org/wiki/Grayscale
     */
    def toGray ()
    {
        val gray  = (getRed * 213 + getGreen * 715 + getBlue * 72) / 1000
        buffImage = toBuffImage (gray)
        reset ()
    } // toGray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a vector to matrix.
     *  @param vec
     *  @param height
     *  @param width
     */
    def toMatrix (vec: VectorI, height: Int, width: Int): MatriI =
    {
        val mat = new MatrixI (height, width)
        for (i <- 0 until height) mat.set (i, vec(i * width until (i+1) * width))
        mat
    } // toMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert an array to matrix.
     *  @param arr 
     *  @param height
     *  @param width
     */
    def toMatrix (arr: Array [Int], height: Int, width: Int): MatriI =
    {
        val mat = new MatrixI (height, width)
        for (i <- 0 until height) mat(i) = VectorI (arr.slice (i * width, (i + 1) * width))
        mat
    } // toMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a given matrix to buffered image.
     *  @param mat
     */
    def toBuffImage (mat: MatriI): BufferedImage =
    {
        val resBuffImage = new BufferedImage (mat.dim2, mat.dim1, BufferedImage.TYPE_INT_RGB)
        for (i <- mat.range1; j <- mat.range2) {
            val rgb   = mat(i, j)
            val color = new Color (rgb, rgb, rgb)
            resBuffImage.setRGB (j, i, color.getRGB)
        } // for
        resBuffImage
    } // toBuffImage

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a rgbMatrix to buffered image.
     */
    def toBuffImage: BufferedImage =
    {
        val width  = rgbMatrix.dim2
        val height = rgbMatrix.dim1
        val resBuffImage = new BufferedImage (width, height, BufferedImage.TYPE_INT_RGB)
        for (i <- 0 until height; j <- 0 until width) {
            val rgb   = rgbMatrix(i, j)
            val color = new Color (rgb, rgb, rgb)
            resBuffImage.setRGB (j, i, color.getRGB)
        } // for
        resBuffImage
    } // toBuffImage

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Save the image to file for a given path and format.
     *  @param path
     *  @param format
     */
    def toFile (path: String, format: String)
    {
        ImageIO.write (buffImage, format, new File (path))
    } // toFile

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display the image.
     */
    def displayImage ()
    {
        val icon  = new ImageIcon (buffImage)
        val frame = new JFrame
        frame.setLayout (new FlowLayout)
        frame.setSize (imageWidth, imageHeight)
        val lbl = new JLabel
        lbl.setIcon (icon)
        frame.add (lbl)
        frame.setVisible (true)
        frame.setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE)
    } // displayImage

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reduce the noise (denoise) image 'buffImage' using a simple local neighborhood
     *  mean method.
     */
    def deNoise ()
    {
        val bufimg = buffImage
        val (r_mat, g_mat, b_mat) = (getRed, getGreen, getBlue)
        for (i <- 1 until imageHeight-1; j <- 1 until imageWidth-1) {
            val red   = r_mat(i-1 to i+1, j-1 to j+1).sum / 9
            val green = g_mat(i-1 to i+1, j-1 to j+1).sum / 9
            val blue  = b_mat(i-1 to i+1, j-1 to j+1).sum / 9
            bufimg.setRGB (j, i, new Color (red, green, blue).getRGB)
        } // for
        buffImage = bufimg
        reset ()
    } // deNoise

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform one way of De-Noising the image by taking the mean of the kernal
     *  image values and assigns it one pixel
     *  FIX - under development
     */
    def smoothingMean ()
    {
        val bufimg = buffImage
        val (r_mat, g_mat, b_mat) = (getRed, getGreen, getBlue)
        for (i <- 0 until imageHeight-2; j <- 0 until imageWidth-2) {
            var r_smooth, g_smooth, b_smooth = 0
            for (k <- i until i+3; l <- j until j+3) {
                r_smooth += r_mat(k, l) / 9
                g_smooth += g_mat(k, l) / 9
                b_smooth += b_mat(k, l) / 9
//              println (r_smooth, g_smooth, b_smooth)
            } // for
            bufimg.setRGB (j, i, new Color (r_smooth, g_smooth, b_smooth).getRGB)
        } // for
        buffImage = bufimg
        reset ()
    } // smoothingMean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform De-Noising by enumerates the pixel value based on the median of the
     *  kernal for that pixel.
     *  @param ksize
     */
    def smoothingMedian (ksize: Int)
    {
        val bufimg = buffImage
        val (r_mat, g_mat, b_mat) = (getRed, getGreen, getBlue)
        for (i <- 0 until imageHeight-ksize-1; j <- 0 until imageWidth-ksize-1) {
            val r_smooth = new VectorI (ksize * ksize)
            val g_smooth = new VectorI (ksize * ksize)
            val b_smooth = new VectorI (ksize * ksize)
            var m = 0
            for (k <- i until i+ksize; l <- j until j+ksize) {
                r_smooth(m) = r_mat(k, l)
                g_smooth(m) = g_mat(k, l)
                b_smooth(m) = b_mat(k, l)
                m +=1
            } // for
            r_smooth.sort (); g_smooth.sort (); b_smooth.sort ()
            bufimg.setRGB (j, i, new Color (r_smooth(r_smooth.dim/2), g_smooth(g_smooth.dim/2),
                                            b_smooth(b_smooth.dim/2)).getRGB)
        } // for
        buffImage = bufimg
        reset ()
    } // smoothingMedian

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a color image to a black and white image using the iterative threshold
     *  mechanism to obtain a b/w matrix.
     */
    def toBW ()
    {
        val bufimg = new BufferedImage (imageWidth, imageHeight, BufferedImage.TYPE_BYTE_BINARY)
        val red    = iterTreshold (getRed)
        val green  = iterTreshold (getGreen)
        val blue   = iterTreshold (getBlue)
        for (i <- red.range1; j <- red.range2) {
            bufimg.setRGB (j, i, new Color (red(i,j), green(i, j), blue(i, j)).getRGB)
        } // for
        buffImage = bufimg
        reset ()
    } // toBW

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'iterThreshold' method is used as a part of converting the image into b/w.
     *  It chooses an intital threshold value as average of the max and min values
     *  of the matrix and a new threshold value is calculated for each iteration.
     *  The process stops when the difference between the old and new threshold values
     *  are less than 5.
     *  @param mat
     */
    private def iterTreshold (mat: MatriI): MatriI =
    {
        val max = mat.max ()
        val min = mat.min ()
        var t1  = (max + min) / 2
        var t2  = 0
  
        while (abs (t1 - t2) > 1) {
            if (t2 == 0) t2=t1
            t1 = t2
            val a1 = mat.clean (t2.toDouble, relative = false)
            val a2 = mat - a1
            val a1avg = a1.sum / countNonZeros (a1)
            val a2avg = a2.sum / countNonZeros (a2)
            t2 = (a1avg + a2avg) / 2
            println (s"(t1, t2) = ($t1, $t2)")
        } // while

        val res = mat
        for (i <- mat.range1; j <- mat.range2) {
            if (mat(i, j) > t2) res(i, j) = 255 else res(i, j) = 0
        } // for
        res
    } // iterThreshold
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of elements in the matrix that are not zeros.
     *  @param mat
     */
    def countNonZeros (mat: MatriI): Int =
    {
        var count = 0
        for (i <- mat.range1) count += mat(i).countZero
        mat.dim1 * mat.dim2 - count
    } // countNonZeros

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'deNoising' method calls 'deNoise' repeatedly to reduce the noise in the image.
     */
    def deNoising () { deNoise (); deNoise (); deNoise (); deNoise (); }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    //  FIX: Needs to be developed further.
    def laplaceFilter: BufferedImage =
    {
        val lmat = new MatrixI (rgbMatrix.dim1, rgbMatrix.dim2)
        for (i <- 1 until rgbMatrix.dim1-1; j <- 1 until rgbMatrix.dim2-1) {
            val ns5 = rgbMatrix(i-1 to i+1, j-1 to j+1).sum / 9
            lmat(i, j) = 9 * (ns5 - rgbMatrix(i, j))
        } // for
        toBuffImage (rgbMatrix - lmat)
    } // laplaceFilter

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Absolute subtraction of two matrices.
     *  @param mat1
     *  @param mat2
     */
    def subAbs (mat1: MatriI, mat2: MatriI): MatriI =
    {
        val res = new MatrixI (mat1.dim1, mat1.dim2)
        for (i <- mat1.range1; j <- mat2.range2) res(i, j) = abs (mat1(i, j) - mat2 (i, j))
        res
    } // subAbs

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the values pixel by pixel.  Pixels having exactly similar RGB values
     *  are considered similar.
     *  @param mat1
     */
    def similarity (mat: MatriI): Double =
    {
        if (rgbMatrix.dim1 != mat.dim1 && rgbMatrix.dim2 != mat.dim2) {
            println ("the matrices are of different dimensions, cannot be compared")
            0.0
        } else {
            val prod  = rgbMatrix.dim1 * rgbMatrix.dim2
            val nosim = countNonZeros (subAbs (rgbMatrix, mat))
            (prod - nosim) * 100.0 / prod
        } // if
    } // similarity

} // ImageCV class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ImageCVTest` object is used to test `ImageCV` class.
 *  > runMain scalation.image.ImageCVTest
 */
object ImageCVTest extends App
{
    // sample images:
    // val file = BASE_DIR + "sample.jpg"
    // val file = BASE_DIR + "balloons_noisy.png"
    // val file = BASE_DIR + "noisy_voc_worst_002.png"
    // val file = BASE_DIR + "kodim23-noise-std51.png"

    val file = BASE_DIR + "balloons_noisy.png"
    val noisy_img = new ImageCV (file)
    noisy_img.displayImage ()                                       // display the noisy image

    banner ("Noise Reduction")
    val rgb = noisy_img.getRgbMatrix                                // get the RGB matrix of the image
    noisy_img.smoothingMedian (5)                                   // apply de-nosing by median method
    println ("Image similarity " + noisy_img.similarity (rgb))      // prints the similarity of two image
    noisy_img.displayImage ()                                       // display the de-noised image

    banner ("Grayscale Conversion")
    val gray = noisy_img.toGray ()                                  // convert to grayscale
    noisy_img.displayImage ()                                       // display the converted image

} // ImageCVTest object

