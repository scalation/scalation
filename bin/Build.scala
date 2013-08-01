
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Srikalyan Swayampakula, Michael E. Cotterell
 *  @version 1.0
 *  @date    Sun May  5 13:49:22 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

import java.io.{BufferedWriter, File, FileWriter}

import collection.mutable.{ArrayBuffer, ListBuffer}
import tools.nsc.{Global, Settings}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This Build App object is used to build the ScalaTion Scala-based Analytics,
 *  Optimization and Simulation system.
 *
 *  To build the complete ScalaTion system, type the following command:
 *
 *  $ scala Build
 *
 *  Build options may also be used to build parts of the ScalaTion system:
 *        -a to compile an apps_package or apps_package/file
 *        -s to compile a  src_package  or src_package/file
 *        -h for help 
 *
 *  $ scala Build [ -a apps_package[.file] | -s src_package[.file] | -h ]
 *        $ scala Build -a optimization
 *        $ scala Build -a optimization.ServiceNetwork
 *        $ scala Build -s analytics
 *        $ scala Build -s analytics.Regression
 *        $ scala Build -h
 *
 *  Instead of the universal dot ".", may also use the OS specific file/path separator ('/' or '\')
 *
 *  If necessary, the Build App itself can be re-compiled.
 *
 *  $ scalac Build.scala
 */
object Build extends App
{
    private val sep       = File.separator            // file separator ('/' for UNIX, '\' for Windows)
    private val BASE_DIR  = ".."
    private val SRC_DIR   = BASE_DIR + sep + "src"
    private val CLASS_DIR = BASE_DIR + sep + "classes"
    private val DOC_DIR   = BASE_DIR + sep + "doc"
    private val APPS_DIR  = BASE_DIR + sep + "apps"

    /** The source packages (in src) making up scalation system
     */
    private val src_packages = Array ("util",
                                      "math",
                                      "linalgebra",
                                      "linalgebra_gen",
                                      "calculus",
                                      "random",
                                      "stat",
                                      "scala2d",
                                      "plot",
                                      "model",
                                      "animation",
                                      "minima",
                                      "maxima",
                                      "analytics",
                                      "graphalytics",
                                      "metamodel",
                                      "queueingnet",
                                      "dynamics",
                                      "dynamics_pde",
                                      "activity",
                                      "event",
                                      "process",
                                      "state")

    /** The apps packages (in apps) for simulation, optimizations and analytics
     */
    private val apps_packages = Array ("activity",
                                       "dynamics",
                                       "event",
                                       "process",
                                       "state",
                                       "montecarlo",
                                       "analytics",
                                       "optimization",
                                       "simopt",
                                       "game")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extend the src package name to the full path.
     *  @param pkg  the package name to extend
     */
    def extend_s (pkg: String): String = SRC_DIR + sep + "scalation" + sep + pkg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extend the apps package name to the full path.
     *  @param pkg  the package name to extend
     */
    def extend_a (pkg: String): String = APPS_DIR + sep + pkg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compile the source file identified by "package.file".  Also takes
     *  "package[/|\]file.scala".  If no separator (sep) is found, compile all
     *  source files (.scala) in the given src package.
     *  @param qname  the qualified name
     */
    def compile_s (qname: String)
    {
        val qpath = qname.replaceAll("\\.", sep)
        val idx = qpath.indexOf (sep)
        if (idx > 0) {
            val pkg = qpath.substring (0, idx)
            if (src_packages contains pkg) {
                val c = new Comp (CLASS_DIR, CLASS_DIR)
                val n = c.compFile (extend_s (qpath))
                println ("compile_s: compiled src file " + qpath + ": " + n + " files")
            } else {
                println ("compile_s: file " + qpath + ": package not found")
            } // if
        } else if (src_packages contains qname) {
            val c = new Comp (CLASS_DIR, CLASS_DIR)
            val n = c.compPackage (extend_s (qpath))
            println ("compile_s: compiled src package " + qpath + ": " + n + " files")
        } else {
            println ("compile_s: package " + qpath + ": not found")
        } // if
    } // compile_s

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compile the source files (.scala) for all the src packages in scalation.
     */
    def compileAll_s ()
    {
        val c = new Comp (CLASS_DIR, CLASS_DIR)
        for (pkg <- src_packages) {
            val n = c.compPackage (extend_s (pkg))
            println ("compileAll_s: compiled package " + pkg + ": " + n + " files")
        } // for
    } // compileAll_s

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compile the apps file identified by "package.file".  Also takes
     *  "package[/|\] file.scala".  If no separator (sep) is found, compile all
     *  apps files (.scala) in the given apps package.
     *  @param qname  the qualified name
     */
    def compile_a (qname: String)
    {
        val qpath = qname.replaceAll("\\.", sep)
        val idx = qpath.indexOf (sep)
        if (idx > 0) {
            val pkg = qpath.substring (0, idx)
            if (apps_packages contains pkg) {
                val c = new Comp (CLASS_DIR, extend_a (pkg) + sep + "classes")
                val n = c.compFile (extend_a (qpath))
                println ("compile_a: compiled apps file " + qpath + ": " + n + " files")
            } else {
                println ("compile_a: file " + qpath + ": package not found")
            } // if
        } else if (apps_packages contains qpath) {
            val c = new Comp (CLASS_DIR, extend_a (qpath) + sep + "classes")
            val n = c.compPackage (extend_a (qpath))
            println ("compile_a: compiled apps package " + qpath + ": " + n + " files")
        } else {
            println ("compile_a: package " + qpath + ": not found")
        } // if
    } // compile_a

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compile the apps files (.scala) for all the apps packages in scalation.
     */
    def compileAll_a ()
    {
        for (pkg <- apps_packages) {
            val c = new Comp (CLASS_DIR, extend_a (pkg) + sep + "classes")
            val n = c.compPackage (extend_a (pkg))
            println ("compileAll_a: compiled apps " + pkg + ": " + n + " files")
        } // for
    } // compileAll_a

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print how to generate scaladoc files for all the scalation packages.
     *  FIX: run scaladoc programmatically
     *  @see http://www.scala-lang.org/docu/files/tools/scaladoc.html
     */
    def doc
    {
        val _J    = " -J-mx1024m"
        val _cp   = " -classpath " + CLASS_DIR
        val files = " " + SRC_DIR + sep + "scalation" + sep + "*" + sep + "*.scala"
        println ("\n***To generate scaladoc documentation execute the following two commands:")
        println ("$ " + "cd ../doc")
        println ("$ " + "scaladoc" + _J + _cp + files)
    } // doc

    val arg = args                                                      // get command-line arguments

    if (arg.length == 0) {                                              // build everything
        compileAll_s ()                                                 // -build all src files
        compileAll_a ()                                                 // -build all apps files
        GenIndexHtml                                                    // -generate index.html files
        doc                                                             // -print instruction for scaladoc

    } else if (arg.length == 1) {                                       // print help
        println ("$ scala Build [ -a apps_package[/file] | -s src_package[/file] | -h ]")

    } else if (arg.length == 2) {                                       // build selective
        arg(0) match {
            case "-a" => compile_a (arg(1))                             // build apps files
            case "-s" => compile_s (arg(1))                             // build src files
            case _ =>    println ("Build: " + arg(0) + " option not supported")
        } // match

    } else {
        println ("Build: incorrect number of command-line arguments given")
    } // if

} // Build object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to compile Scala files (.scala) programmatically.
 *  @param _cp  for -classpath <path>    Specify where to find user class files
 *  @param _d   for -d <directory|jar>   destination for generated class files
 */
class Comp (_cp: String = "classes", _d: String = "classes")
{
    private val _J: String  = null         // advanced Java option, e.g., "-J-mx1014m"
    private val s   = new Settings         // compiler settings
    private val sep = File.separator       // file separator ('/' for UNIX, '\' for Windows)
    private val ext = ".scala"             // filename extension for source files

    if (_cp != null) s.classpath.append (_cp)
    if (_d  != null) s.outputDirs.setSingleOutput (_d)
    if (_J  != null) s.jvmargs.tryToSet (List (_J))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the file for the given package and file name.  Works whether or not
     *  the filename extension (ext) is present.
     *  @param qname  the package/file qualified name
     */
    def getFile (qname: String): List [String] =
    {
        val fList = ListBuffer [String] ()
        fList += (if (qname.endsWith (ext)) qname else qname + ext)
        fList.toList
    } // getFile

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compile the file for the given package and file name, returning the
     *  number of files compiled.
     *  @param qname  the package/file qualified name
     */
    def compFile (qname: String): Int =
    {
        val gEnv   = new Global (s)          // global environment for compiler
        val runner = new gEnv.Run            // object to run Scala compiler
        val file   = getFile (qname)         // list of file names (only 1)
        runner.compile (file)                // compile all files in list
        file.size                            // number of source files compiled
    } // compPackage
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get all the files for the given package.
     *  @param pname  the package name
     */
    def getFiles (pname: String): List [String] =
    {
        val fList = ListBuffer [String] ()
        val dir   = new File (pname)
        for (file <- dir.listFiles) {
           val fname = file.getName
           if (fname.endsWith (".scala")) fList += (pname + sep + fname)
        } // for
        fList.toList
    } // getFiles

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compile all the files for the given package, returning the number of
     *  files compiled.
     *  @param pname  the package name
     */
    def compPackage (pname: String): Int =
    {
        val gEnv   = new Global (s)          // global environment for compiler
        val runner = new gEnv.Run            // object to run Scala compiler
        val files  = getFiles (pname)        // list of file names
        runner.compile (files)               // compile all files in list
        files.size                           // number of source files compiled
    } // compPackage
    
} // Comp class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to create "index.html" files in source code directories
 *  to enable Web browsing of source code.
 */
object GenIndexHtml
{
    private val SKIP       = "old"                     // do not process files in this directory
    private val sep        = File.separator            // file separator ('/' for UNIX, '\' for Windows)
    private val BASE_DIR   = ".."
    private val SRC_DIR    = BASE_DIR + sep + "src"
    private val currentDir = SRC_DIR + sep + "scalation"

    println ("Generate index.html files starting from currentDir = " + currentDir)
    recCreate (new File (currentDir))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively create index.html files for each directory.
     *  @param f  the file/directory to examine
     */
    def recCreate (f: File)
    {
        recDeleteIndex (f)
        val dirs = new ArrayBuffer [File] ()

        try {
            val iFile = new File (f.getAbsolutePath () + "/index.html")      // the index.html file to write
            val fos   = new BufferedWriter (new FileWriter (iFile))
            fos.write ("<html>\n<body>\n<h1> Source files in " + f.getName () + " Package </h1><p>\n<ul>\n")

            for (fi <- f.listFiles ()) {
                val fName = fi.getName ()
                if (! fi.isDirectory ()) {
                    fos.write ("<li> <a href = './" + fName + "'> " + fName + " </a> </li>\n")
                } else if (fName != SKIP) {
                    dirs += fi
                } // if
            } // for

            for (fi <- dirs) {
                val fName = fi.getName ()
                if (fName != SKIP) {
                    fos.write ("<li> <a href = './" + fName + "'> " + fName + " </a> </li>\n")
                } // if
            } // for

            fos.write ("</ul>\n</body>\n<html>")
            fos.close ()

            for (fi <- dirs if fi.isDirectory ()) recCreate (fi)     // recurse into each directory
        } catch { case _ => }
    } // recCreate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively delete index.html files for each directory (clean up step).
     *  @param f  the file/directory to examine
     */
    def recDeleteIndex (f: File)
    {
        if (! f.isDirectory ()) {
            if (f.getName () == "index.html") f.delete ()
        } else {
            val files = f.listFiles ()
            if (files != null) {
                for (fi <- files) try recDeleteIndex (fi) catch { case _ => }
            } // if
        } // if
    } // recDeleteIndex

} // GenIndexHtml object

