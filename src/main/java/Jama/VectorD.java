
package Jama;

import java.text.NumberFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;
import java.text.FieldPosition;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.StreamTokenizer;

/**
 *  Jama = Java Vector class.  -- Added by John Miller
 *  <P>
 *  The Java VectorD Class provides the fundamental operations of numerical
 *  linear algebra.  Various constructors create vectors from one dimensional
 *  arrays of double precision floating point numbers.  Various "gets" and
 *  "sets" provide access to subvectors and vector elements.  Several methods 
 *  implement basic vector arithmetic, including vector addition and
 *  multiplication, vector norms, and element-by-element array operations.
 *  Methods for reading and printing vectors are also included.  All the
 *  operations in this version of the VectorD Class involve real vectors.
 *  Complex vectors may be handled in a future version.
 *  @author The MathWorks, Inc. and the National Institute of Standards and Technology.
 *  @version 5 August 1998
*/
public class VectorD implements Cloneable, java.io.Serializable {

/* ------------------------
   Class variables
 * ------------------------ */

   private static final long serialVersionUID = 1;

   /** Array for internal storage of elements.
    *  @serial internal array storage.
    */
   private double[] a;

   /** Dimension.
    *  @serial dimension.
    */
   private int n;

/* ------------------------
   Constructors
 * ------------------------ */

   /** Construct an n-vector of zeros. 
    *  @param n    Number of elements
    */
   public VectorD (int n)
   {
      this.n = n;
      a = new double[n];
   } // constructor

   /** Construct a constant n-vector.
    *  @param m    Number of rows.
    *  @param n    Number of colums.
    *  @param s    Fill the vector with this scalar value.
    */
   public VectorD (int n, double s)
   {
      this.n = n;
      a = new double[n];
      for (int j = 0; j < n; j++) a[j] = s;
   } // constructor

   /** Construct a vector from a 1-D array.
    *  @param a    One-dimensional array of doubles.
    *  @exception  IllegalArgumentException All rows must have the same length
    *  @see        #constructWithCopy
    */
   public VectorD (double[] a)
   {
      n = a.length;
      this.a = a;
   } // constructor

/* ------------------------
   Public Methods
 * ------------------------ */

   /** Construct a vector from a copy of a 1-D array.
    *  @param a    One-dimensional array of doubles.
    */
   public static VectorD constructWithCopy (double[] a)
   {
      int n = a.length;
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = a[j];
      return x;
   } // constructWithCopy

   /** Make a deep copy of a vector
    */
   public VectorD copy ()
   {
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = a[j];
      return x;
   } // copy

   /** Clone the VectorD object.
    */
   public Object clone ()
   {
      return this.copy();
   } // clone

   /** Access the internal one-dimensional array.
    *  @return     Pointer to the two-dimensional array of vector elements.
    */
   public double[] getArray ()
   {
      return a;
   } // getArray

   /** Copy the internal one-dimensional array.
    *  @return     Two-dimensional array copy of vector elements.
    */
   public double[] getArrayCopy ()
   {
      double[] c = new double[n];
      for (int j = 0; j < n; j++) c[j] = a[j];
      return c;
   }

   /** Return dimension.
    */
   public int getDimension ()
   {
      return n;
   } // getDimension

   /** Get a single element.
    *  @param j    index.
    *  @return     a(j)
    *  @exception  ArrayIndexOutOfBoundsException
   */
   public double get (int j)
   {
      return a[j];
   } // get

   /** Get a subvector.
    *  @param j0   Initial index
    *  @param j1   Final index
    *  @return     a(j0:j1)
    *  @exception  ArrayIndexOutOfBoundsException Subvector indices
    */
   public VectorD getVectorD (int j0, int j1)
   {
      VectorD x = new VectorD (j1-j0+1);
      double[] b = x.getArray();
      for (int j = j0; j <= j1; j++) b[j-j0] = a[j];
      return x;
   } // getVectorD

   /** Get a subvector.
    *  @param c    Array of indices.
    *  @return     a(c(:))
    *  @exception  ArrayIndexOutOfBoundsException Subvector indices
    */
   public VectorD getVectorD (int[] c)
   {
      VectorD x = new VectorD (c.length);
      double[] b = x.getArray();
      for (int j = 0; j < c.length; j++) b[j] = a[c[j]];
      return x;
   } // getVectorD

   /** Set a single element.
    *  @param j    Column index.
    *  @param s    a(j).
    *  @exception  ArrayIndexOutOfBoundsException
   */
   public void set (int j, double s)
   {
      a[j] = s;
   } // set

   /** Set a subvector.
    *  @param j0   Initial column index
    *  @param j1   Final column index
    *  @param x    a(j0:j1)
    *  @exception  ArrayIndexOutOfBoundsException Subvector indices
    */
   public void setVectorD (int j0, int j1, VectorD x)
   {
      for (int j = j0; j <= j1; j++) a[j] = x.get(j-j0);
   } // setVectorD

   /** Set a subvector.
    *  @param c    Array of indices.
    *  @param x    a(c(:))
    *  @exception  ArrayIndexOutOfBoundsException Subvector indices
    */
   public void setVectorD (int[] c, VectorD x)
   {
      for (int j = 0; j < c.length; j++) a[c[j]] = x.get(j);
   } // setVectorD

   /** One norm.
    *  @return    sum of absolute elements
    */
   public double norm1 ()
   {
      double s = 0.0;
      for (int j = 0; j < n; j++) s += Math.abs (a[j]);
      return s;
   } // norm1

   /** Two norm.
    *  @return    sqrt of sum of elements squared
    */
   public double norm2 ()
   {
      double s = 0.0;
      for (int j = 0; j < n; j++) s += a[j] * a[j];
      return Math.sqrt (s);
   } // norm2

   /** Infinity norm.
    *  @return    maximum row sum.
    */
   public double normInf ()
   {
      double f = a[0];
      for (int j = 0; j < n; j++) f = Math.max (f, Math.abs (a[j]));
      return f;
   } // normInf

   /** Unary minus.
    *  @return    -a
    */
   public VectorD uminus ()
   {
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = -a[j];
      return x;
   } // uminus

   /** c = a + b
    *  @param b    another vector
    *  @return     a + b
    */
   public VectorD plus (VectorD b)
   {
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = a[j] + b.a[j];
      return x;
   } // plus

   /** a = a + b
    *  @param b    another vector
    *  @return     a + b
    */
   public VectorD plusEquals (VectorD b)
   {
      for (int j = 0; j < n; j++) a[j] = a[j] + b.a[j];
      return this;
   } // plusEquals

   /** c = a - b
    *  @param b    another vector
    *  @return     a - b
    */
   public VectorD minus (VectorD b)
   {
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = a[j] - b.a[j];
      return x;
   } // minus

   /** a = a - b
    *  @param b    another vector
    *  @return     a - b
    */
   public VectorD minusEquals (VectorD b)
   {
      for (int j = 0; j < n; j++) a[j] = a[j] - b.a[j];
      return this;
   } // minusEquals

   /** Element-by-element multiplication, c = a.*b
    *  @param b    another vector
    *  @return     a.*b
    */
   public VectorD times (VectorD b)
   {
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = a[j] * b.a[j];
      return x;
   } // times

   /** Element-by-element multiplication in place, a = a.*b
    *  @param b    another vector
    *  @return     a.*b
    */
   public VectorD timesEquals (VectorD b)
   {
      for (int j = 0; j < n; j++) a[j] = a[j] * b.a[j];
      return this;
   } // timesEquals

   /** Element-by-element right division, c = a./b
    *  @param b    another vector
    *  @return     a./b
    */
   public VectorD divide (VectorD b)
   {
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = a[j] / b.a[j];
      return x;
   } // divide

   /** Element-by-element right division in place, a = a./b
    *  @param b    another vector
    *  @return     a./b
    */
   public VectorD divideEquals (VectorD b)
   {
      for (int j = 0; j < n; j++) a[j] = a[j] / b.a[j];
      return this;
   } // divideEquals

   /** Multiply a vector by a scalar, c = s * a
    *  @param s    scalar
    *  @return     s*a
    */
   public VectorD times (double s)
   {
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = s * a[j];
      return x;
   } // times

   /** Multiply a vector by a scalar in place, a = s * a
    *  @param s    scalar
    *  @return     replace A by s*a
    */
   public VectorD timesEquals (double s)
   {
      for (int j = 0; j < n; j++) a[j] = s * a[j];
      return this;
   } // timesEquals

   /** Linear algebraic vector multiplication, a * b
    *  @param b    another vector
    *  @return     VectorD product, a * b
    *  @exception  IllegalArgumentException VectorD inner dimensions must agree.
    */
   public double dot (VectorD b)
   {
      if (b.n != n) {
         throw new IllegalArgumentException ("VectorD dimensions must agree.");
      } // if

      double s = 0;
      for (int j = 0; j < n; j++) s += a[j] * b.a[j];
      return s;
   } // dot

   /** Generate vector with random elements
    *  @param n    Number of elements.
    *  @return     An n-vector with uniformly distributed random elements.
    */
   public static VectorD random (int n)
   {
      VectorD x = new VectorD (n);
      double[] c = x.getArray();
      for (int j = 0; j < n; j++) c[j] = Math.random ();
      return x;
   } // random

   /** Print the vector to stdout.   Line the elements up in columns
    *  with a Fortran-like 'Fw.d' style format.
    *  @param w    Column width.
    *  @param d    Number of digits after the decimal.
    */
   public void print (int w, int d)
   {
      print (new PrintWriter(System.out, true), w, d);
   } // print

   /** Print the vector to the output stream.   Line the elements up in
    *  columns with a Fortran-like 'Fw.d' style format.
    *  @param output Output stream.
    *  @param w      Column width.
    *  @param d      Number of digits after the decimal.
    */
   public void print (PrintWriter output, int w, int d)
   {
      DecimalFormat format = new DecimalFormat();
      format.setDecimalFormatSymbols (new DecimalFormatSymbols (Locale.US));
      format.setMinimumIntegerDigits (1);
      format.setMaximumFractionDigits (d);
      format.setMinimumFractionDigits (d);
      format.setGroupingUsed (false);
      print (output, format, w+2);
   } // print

   /** Print the vector to stdout.  Line the elements up in columns.
    *  Use the format object, and right justify within columns of width
    *  characters.
    *  Note that is the vector is to be read back in, you probably will want
    *  to use a NumberFormat that is set to US Locale.
    *  @param format A  Formatting object for individual elements.
    *  @param width     Field width for each column.
    *  @see java.text.DecimalFormat#setDecimalFormatSymbols
    */
   public void print (NumberFormat format, int width)
   {
      print (new PrintWriter (System.out,true), format, width);
   } // print

   // DecimalFormat is a little disappointing coming from Fortran or C's printf.
   // Since it doesn't pad on the left, the elements will come out different
   // widths.  Consequently, we'll pass the desired column width in as an
   // argument and do the extra padding ourselves.

   /** Print the vector to the output stream.  Line the elements up in columns.
    *  Use the format object, and right justify within columns of width
    *  characters.
    *  Note that is the vector is to be read back in, you probably will want
    *  to use a NumberFormat that is set to US Locale.
    *  @param output the output stream.
    *  @param format A formatting object to format the vector elements 
    *  @param width  Column width.
    *  @see java.text.DecimalFormat#setDecimalFormatSymbols
    */
   public void print (PrintWriter out, NumberFormat format, int width)
   {
      for (int j = 0; j < n; j++) {
         String s = format.format (a[j]);                   // format the number
         int padding = Math.max (1, width-s.length ());     // At _least_ 1 space
         for (int k = 0; k < padding; k++) out.print (' ');
         out.print (s);
      } // for
      out.println ();               // end with newline
   } // print

   /** Read a vector from a stream.  The format is the same the print method,
    *  so printed matrices can be read back in (provided they were printed using
    *  US Locale).  Elements are separated by
    *  whitespace, all the elements for each row appear on a single line,
    *  the last row is followed by a blank line.
    *  @param input the input stream.
    */
   public static VectorD read (BufferedReader input) throws java.io.IOException
   {
      StreamTokenizer tokenizer = new StreamTokenizer(input);

      // Although StreamTokenizer will parse numbers, it doesn't recognize
      // scientific notation (E or D); however, Double.valueOf does.
      // The strategy here is to disable StreamTokenizer's number parsing.
      // We'll only get whitespace delimited words, EOL's and EOF's.
      // These words should all be numbers, for Double.valueOf to parse.

      tokenizer.resetSyntax ();
      tokenizer.wordChars (0, 255);
      tokenizer.whitespaceChars (0, ' ');
      tokenizer.eolIsSignificant (true);
      java.util.Vector<Double> vD = new java.util.Vector<Double>();

      // Ignore initial empty lines
      while (tokenizer.nextToken () == StreamTokenizer.TT_EOL);
      if (tokenizer.ttype == StreamTokenizer.TT_EOF) {
         throw new java.io.IOException ("Unexpected EOF on vector read.");
      } // if

      do {
         vD.addElement (Double.valueOf (tokenizer.sval));      // Read & store 1st row.
      } while (tokenizer.nextToken() == StreamTokenizer.TT_WORD);

      int n = vD.size();               // Now we've got the number of elements
      double [] c = new double [n];
      for (int j = 0; j < n; j++) {    // extract the elements of the 1st row.
         c[j] = vD.elementAt(j).doubleValue();
      } // for
      return new VectorD (c);
   } // read

} // VectorD class

