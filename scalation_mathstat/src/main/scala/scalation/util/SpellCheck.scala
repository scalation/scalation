
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat Apr 23 23:40:31 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package util

import java.io.File

import scala.collection.mutable.Set

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RunSpellCheck` object is used to check the spelling of the a given package.
 *  The package directory (relative path) is entered as a command-line argument.
 *  Ex:  'scalation/math'
 *  > run-main scalation.util.RunSpellCheck <package directory>
 */
object RunSpellCheck extends App
{
    val packdir = if (args.length > 0) args(0) else "scalation" + ⁄ + "util"
    val sp = SpellCheck
    sp.checkDir (SRC_DIR + "main" + ⁄ + "scala" + ⁄ + packdir)

} // RunSpellCheck object

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SpellCheck` object is used to check the spelling in source code comments.
 *  If not using Linux, may need to replace path for 'DICTIONARY'.
 *  Note, okay to use '/' since the dictionary location is system-dependent anyway.
 */
object SpellCheck extends Error
{
    /** Dictionary of word provides by the Operating System
     */
    private val DICTIONARY = "/usr/share/dict/words"
//  private val DICTIONARY = "/usr/share/dict/american-english"

    /** Custom words to be added to the dictionary.
     */
    private val CUSTOM = Set (
         // date-time
            "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
            "sun", "mon", "tue", "wed", "thu", "fri", "sat", "edt", "est",
         // math
            "accumulators", "acyclic", "adjacency", "anova", "ancova", "atan", "bayes", "bayesian",
            "bezier", "bfgs", "biconnected", "bidiagonal", "bidiagonalize", "bidiagonalization",
            "bijection", "bijections", "bfs", "cdf", "centroid", "centroids", "cholesky", "collinearity",
            "cmrg", "combinatorics", "congruential", "cos", "cosecant", "cotangent", "covariance",
            "covariances", "cuboidic", "cumulate", "dag", "denonimator", "dfs", "dimensionality", "div",
            "eigenvalue", "eigenvalues", "eigenvector", "eigenvectors", "eratosthenes", "equi", "euclidean",
            "exp", "gauss", "gcd", "hessenburg", "hessian", "hypermatrix", "hypermatrices", "hungarian",
            "icdf", "infeasible", "integrators", "isomorphism", "isomorphically", "jacobian", "jordan",
            "kurtosis", "lagrange", "laplacian", "lcg", "lhs", "linearized", "logit", "lp", "manhattan",
            "markov", "markovian", "max", "min", "mle", "modulo", "mrg", "nlp", "nonnegative", "nullspace",
            "optimality", "orthogonalization", "orthonormalization", "pde", "pdf", "pearson", "pf",
            "pmf", "pow", "quantile", "quantiles", "queueing", "petri", "radians", "rhs", "rms", "rv",
            "schmidt", "secant", "significand", "sigmoid", "signum", "skewness", "sqrt", "stoichiometry",
            "strassen", "subgraph", "subgraphs", "subparts", "subtree", "submatrix", "subtrees", "taylor",
            "thomas", "treewidth", "triangulated", "tridiagonal", "tridiagonalize", "tridiagonalization",
            "trinomial", "tuple", "tuples", "unary", "unconstrained", "unreduced", "variates", "vif",
         // probability distributions
            "bernoulli", "bivariate", "chisquare", "erlang", "gaussian", "hypergeometric", "lorentz",
            "multinomial", "nhpp", "pareto", "poisson", "univariate", "weibull",
         // first, second, third, fourth
            "st", "nd", "rd", "th",
         // scala
            "coroutine", "coroutines", "covariant", "datatype", "def", "fortran", "hashcode", "java",
            "nan", "nullary", "parameterized", "recoded", "regex", "sbt", "scala", "scalation",
            "subclass", "subclasses", "subtype", "subtypes", "superclass", "superclasses", "val", "var",
         // files
            "csv", "dataset", "datastore", "dir", "eol", "gb", "html", "json", "kb", "mb", "unicode",
            "url", "utf", "src",
         // other
            "accessor", "accuracies", "advection", "analytics", "apache", "attractor", "bi", "classifier",
            "classifiers", "columnar", "dalton", "daltons", "durations", "dequeue", "dequeued", "diagostics",
            "enqueue", "enqueued", "etc", "fanout", "glycan", "glycans", "ieee", "incrementally",
            "iteratively", "lexicographical", "linux", "mac", "matcher", "meta", "mit", "multi", "olap",
            "pairings", "perceptron", "pre", "precompute", "precomputed", "predictors", "preorder", "prepend",
            "prepended", "prepending", "recompute", "recurse", "reposition", "rescaled", "rescaling",
            "resizable", "resize", "representable", "reproducibility", "spectrometer", "stateful", "subinterval",
            "timestamp", "traversal", "traversable", "uncomment", "undirected", "unpaired", "unvisited")

    /** Dictionary of correctly spelled word
     */
    private val dictionary    = buildDictionary

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
                if (fname endsWith ".scala") checkFile (dir_name + ⁄ + fname, checkAll)
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
    sp.checkFile (SRC_DIR + "main" + ⁄ + "scala" + ⁄ + "scalation" + ⁄ + "util" + ⁄ + "SpellCheck.scala")
    sp.checkDir  (SRC_DIR + "main" + ⁄ + "scala" + ⁄ + "scalation" + ⁄ + "util")

} // SpellCheckTest object

