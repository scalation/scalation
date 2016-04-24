
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Sep  9 20:37:38 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import java.io.File

import scala.collection.mutable.Set

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RunSpellCheck` object is used to check the spelling of the a given package.
 *  The package directory (relative path) is entered as a command-line argument.
 *  > run-main scalation.util.RunSpellCheck <package directory>
 */
object RunSpellCheck extends App
{
    val packdir = if (args.length > 0) args(0) else "scalation/util"
    val sp = SpellCheck
    sp.checkDir (SRC_DIR + "main/scala" + SEP + packdir)

} // RunSpellCheck object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SpellCheck` object is used to check the spelling in source code comments.
 */
object SpellCheck extends Error
{
    private val DICTIONARY    = "/usr/share/dict/american-english"
    private val CUSTOM        = Set (
         // date-time
            "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
            "sun", "mon", "tue", "wed", "thu", "fri", "sat", "edt", "est",
         // math
            "accumulators", "anova", "ancova", "bezier", "cdf", "cmrg", "combinatorics", "congruential", "cosecant",
            "cotangent", "covariance", "covariances", "denonimator", "dimensionality", "div", "eratosthenes",
            "equi", "exp", "gauss", "gcd", "icdf", "kurtosis", "lcg", "lhs", "max", "min", "modulo", "mrg",
            "nlp", "nonnegative", "pdf", "pearson", "pf", "pmf", "pow", "quantile", "quantiles", "radians",
            "rhs", "rms", "rv", "secant", "significand", "signum", "skewness", "sqrt", "taylor", "trinomial",
            "tuple", "tuples", "unary", "variates",
         // probability distributions
            "bernoulli", "chisquare", "erlang", "gaussian", "hypergeometric", "lorentz", "multinomial",
            "nhpp", "pareto", "poisson", "weibull",
         // first, second, third, fourth
            "st", "nd", "rd", "th",
         // scala
            "covariant", "datatype", "def", "hashcode", "java", "nan", "nullary", "recoded", "regex", "sbt",
            "scala", "scalation", "subclass", "subclasses", "subtypes", "superclass", "superclasses", "val", "var",
         // files
            "csv", "dataset", "datastore", "dir", "eol", "gb", "html", "json", "kb", "mb", "unicode", "url",
            "utf", "src",
         // other
            "accessor", "analytics", "apache", "bi", "columnar", "etc", "fanout", "ieee", "linux", "mac",
            "meta", "mit", "multi", "olap", "precompute", "precomputed", "preorder", "reposition", "rescaled",
            "rescaling", "resizable", "resize", "representable", "reproducibility", "subinterval", "traversal",
            "traversable", "uncomment")

    private val dictionary    = buildDictionary          // dictionary - correctly spelled word

    private val START_COMMENT = """/**"""                // comment start pattern
    private val END_COMMENT   = """*/"""                 // comment end pattern
    private val LINE_COMMENT  = """//"""                 // line comment pattern

    private val AT_TAG        = Set ("""@date""",        // ignore @-tags
                                     """@define""",
                                     """@return""",
                                     """@since""",
                                     """@tparam""",
                                     """@version""")

    println ("dictionary size = " + dictionary.size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a dictionary (set of correctly spelled words)
     */
    private def buildDictionary (): Set [String] =
    {
        val dict = Set [String] ()
        val lines = getFromURL_File (DICTIONARY)       
        for (ln <- lines) dict += ln
        dict union CUSTOM
    } // buildDictionary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the spelling of words in the given line.
     *  @param line    the line to check
     *  @param lineNo  the line number of the line
     */
    def checkLine (line: String, lineNo: Int)
    {
        var skip = 0
        if (line.indexOf ("@author") < 0 && line.indexOf ("@see") < 0 &&
            line.indexOf ("run-main") < 0 && line.indexOf ("@builder") < 0) {
            val misspelled = Set [String] ()
            for (word <- line.split ("[0-9\\*\"\\^,\\[\\]{}%!~#./()-<>=|:; ]")) {
                if (word.indexOf ("`") < 0 && word.indexOf ("'") < 0 ) {
                    if (word == "@param") skip = 1
                    if (skip == 0) {
                        for (wrd <- word.split ("_")) {
                            val wd = wrd.toLowerCase
                            if (wd != "" && ! (AT_TAG contains wd) && ! (dictionary contains wd)) misspelled += word
                        } // if
                    } // if
                    skip = if (skip == 1) 2 else 0 
                } // if
            } // for
            if (misspelled.nonEmpty) println (s"line $lineNo:  \t $misspelled")
        } // if
    } // checkLine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the spelling of comments in the given file.  Toggle commented out
     *  lines below to check // ... EOL or not.
     *  @param file_name  the file name
     *  @param checkAll   whether to check the whole file or just the comments
     */
    def checkFile (file_name: String, checkAll: Boolean = false)
    {
        println ("check spelling in " + file_name)
        var inComment  = false                           // whether inside a /* ... */ comment
        var inLComment = false                           // whether inside a // ... EOL comment

        val lines = getFromURL_File (file_name)
        var count = 1
        for (ln <- lines) {
            if (ln.indexOf (START_COMMENT) >= 0 ) inComment = true
            if (inComment && ln.indexOf (END_COMMENT) >= 0) inComment = false
            if (! inComment) inLComment = ln.indexOf (LINE_COMMENT) >= 0      // check // ... EOL comments
            inLComment = false                                                // do not check them
            if (checkAll || inComment) checkLine (ln, count)
            count += 1
        } // for
    } // checkFile

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the spelling of comments in files within the given directory.
     *  @param dir_name   the directory name
     *  @param checkAll   whether to check whole files or just the comments
     */
    def checkDir (dir_name: String, checkAll: Boolean = false)
    {
        val dir = new File (dir_name)
        if (dir.isDirectory ()) {
            for (fi <- dir.listFiles ()) {
                val fname = fi.getName
                if (fname endsWith ".scala") checkFile (dir_name + SEP + fname, checkAll)
            } // of
        } else {
            flaw ("checkDirectory", dir_name + " is not a directory")
        } // if
    } // checkDir

} // SpellCheck object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SpellCheckTest` object is used to test the `SpellCheck` object.
 *  run-main scalation.util.SpellCheckTest
 */
object SpellCheckTest extends App
{
    
    val sp = SpellCheck
    println ("Test line:")
    sp.checkLine ("is this spellled correcty", 1)
    sp.checkFile (SRC_DIR + "main/scala/scalation/util/SpellCheck.scala")
    sp.checkDir (SRC_DIR + "main/scala/scalation/util")

} // SpellCheckTest object

