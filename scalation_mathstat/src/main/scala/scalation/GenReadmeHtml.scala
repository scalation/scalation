
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 1.4
 *  @date    Sat Nov  4 12:18:00 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import java.io.{File, PrintWriter}
import java.util.Scanner

import scala.io.Source

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GenReadmeHtml` object is used to create the html tables for the api and
 *  src directories for convenient Web browsing.
 *  > runmain scalation.GenReadmeHtml
 */
object GenReadmeHtml extends App
{
    val SKIP         = "old"                                                       // do not process files in this directory
    val moduleList   = Array("scalation_mathstat", "scalation_database", "scalation_modeling", "scalation_models")
    val moduleNames  = Array("MATH AND STAT", "DATABASE", "MODELING", "APPLICATIONS")
    val scalationVer = BASE.substring (BASE.lastIndexOf ('_') + 1)                 // ScalaTion version  (i.e., 1.4)
    val scalaVerFull = scala.util.Properties.versionNumberString                   // full scala version (i.e., 2.12.4)
    val scalaVer     = scalaVerFull.substring (0, scalaVerFull.lastIndexOf ('.'))  // main scala version (i.e., 2.12)
    val javaVer      = System.getProperty("java.version").split('.')(1)            // current java version (i.e., 8)

    val fApi   = new File (BASE + ⁄ + "api.html")
    val fSrc   = new File (BASE + ⁄ + "src.html")
    val outApi = new PrintWriter (fApi)                                            // api table writer
    val outSrc = new PrintWriter (fSrc)                                            // src table writer

    // write table headers
    outApi.println (s"""<A name = "scaladoc">\n\n<p><hr><p>\n<h3> Source Packages (doc) </h3>\n<p>\n\n<blockquote>\n<table border = 3>\n<tr>\n<td> <b>Package</b>\n<td> <b>Description</b>\n<tr>""")
    outSrc.println (s"""<A name = "source-code">\n\n<p>\n<h3> Source Packages (src) </h3>\n<p>\n\n<blockquote>\n<table border = 3>\n<tr>\n<td> <b>Package</b>\n<td> <b>Description</b>\n<tr>""")

    // filling in table rows for api and src
    for (i <- 0 until moduleList.length) {
        val topPath = if (i < moduleList.length-1) "scalation" else "apps"          // apps (models) should be the last module
        outApi.println (s"""<tr>\n<td> <a href = "${moduleList(i)}/target/scala-$scalaVer/api/index.html">${moduleNames(i)}</a>\n<tr>""")
        outSrc.println (s"""<tr>\n<td> <a href = "${moduleList(i)}/src/main/scala/$topPath">${moduleNames(i)}</a>\n<tr>""")
        recWrite (new File(BASE + ⁄ + moduleList(i) + s"${⁄}src${⁄}main${⁄}scala${⁄}$topPath"), moduleList(i), topPath, topPath, 0)
    } // for

    // filling in table rows for testing packages (src only)
    val topPath = "testing"
    for (i <- 0 until moduleList.length) {      // for each module
        val testDir = new File (BASE + ⁄ + moduleList(i) + s"${⁄}src${⁄}test${⁄}scala${⁄}$topPath")
        if (testDir.exists()) {
            outSrc.println(s"""<tr>\n<td> <a href = "${moduleList(i)}/src/test/scala/$topPath">${moduleNames(i)} UNIT TESTS</a>\n<tr>""")
            recWrite (testDir, moduleList(i), topPath, topPath, 0, true)
        } // if
    } // for

    outApi.println ("</table>\n</blockquote>")
    outSrc.println ("</table>\n</blockquote>")

    outApi.close
    outSrc.close

    val moduleListS = new StringBuilder
    for (i <- 0 until moduleList.length) {
        moduleListS.append (moduleList(i))
        if      (i <  moduleList.length - 2) moduleListS.append (", ")
        else if (i == moduleList.length - 2) moduleListS.append (" and ")
    } // for

    val prefix = raw"""
<html>
<head>
<LINK href="style.css" rel="stylesheet" type="text/css">
<title> ScalaTion </title>
</head>
<body>

<center>
<h1> SCALAble SimulaTION - ScalaTion </h1>
<p>
<a href = "#papers">Papers</a> | <a href = "#scaladoc">Scaladoc</a> | <a href = "#source-code"> Source Code</a>
</center>

<p>
<b> Welcome to ScalaTion, the Scala-based system for Simulation, Optimization and Analytics. </b>

<p>
This system, coded in Scala, supports multi-paradigm simulation modeling including
'tableau', 'event', 'process', 'dynamics', 'dynamics_pde', 'activity' and 'state' oriented models.

<p>
<a href = "http://www.scala-lang.org">Scala</a> is a modern object-oriented, functional programming language
that is well-suited for developing simulation engines.
It is in the Java family of languages and can call Java code.
The inclusion of advanced and functional programming capabilities,
makes the code much more concise than Java.
ScalaTion also supports optimization and analytics as they nowadays go hand-in-hand with simulation modeling.
Some of the algorithms come in both sequential and parallel versions.

<p>
The <b>ScalaTion $scalationVer</b> version defines multiple .par subpackages that contain parallel versions of sequential algorithms.
Future directions include completing the 'scala3d' and 'physics' packages.

<p>
As of version $scalationVer, ScalaTion consists of ${moduleList.length} subprojects:
$moduleListS

<p>
Please read the <a href = LICENSE.html> LICENSE </a> file (an MIT style license).

<p><hr><p>
<h3> Installation Instructions </h3>

Requires:
<a href = "http://www.scala-lang.org/downloads">Scala $scalaVer.x</a> and
<a href = "http://www.oracle.com/technetwork/java/javase/downloads">Java $javaVer</a>
<br>
Recommended:
<a href = "http://www.scala-sbt.org/">sbt</a> &nbsp;
(see <a href = "http://www.scala-sbt.org/0.13/docs/Setup.html">Setup.html</a>)

<h4>1. Download one of the following two files</h4>

<a href = "../scalation_$scalationVer.tar.gz"> scalation_$scalationVer.tar.gz </a> <br> <b>OR</b> <br>
<a href = "../scalation_$scalationVer.zip"> scalation_${scalationVer}.zip </a>

<p>
<h4>2. Untar or unzip the file</h4>

<pre><code>
$$ tar xvfz scalation_$scalationVer.tar.gz <br> <b>OR</b> <br>
$$ unzip scalation_$scalationVer.zip
</code></pre>

<h4>3. To compile code or run apps, enter sbt and type compile, runMain or exit </h4>

<pre><code>
$$ cd scalation_$scalationVer <br>
$$ cd scalation_models <br>
$$ sbt <br>
> compile <br>
> runMain apps.process.Bank <br>
> runMain scalation.analytics.RegressionTest <br>
> exit
</code></pre>

<p>
ScalaTion $scalationVer is set up to use the Simple Build Tool <a href = "http://www.scala-sbt.org">sbt</a>.

<p>
ScalaTion $scalationVer uses the following build specification files
<a href = "scalation_mathstat/build.sbt">build.sbt</a>,
<a href = "scalation_modeling/build.sbt">build.sbt</a> and
<a href = "scalation_mmodels/build.sbt">build.sbt</a>.
Source packages are in the <b>src/main/scala</b> directory,
class files are in the <b>target/scala-${scalaVer}/classes</b> directory, and
documentation files are in the <b>target/scala-${scalaVer}/api</b> directory.
Data file input, output or analytics are stored in subdirectories of <b>data</b>,
while database files are stored in <b>store</b>.

<p><hr><p>

To develop using an IDE:
<ol>
<li>
Download <a href = "http://typesafe.com/stack/downloads/scala-ide">Scala IDE for Eclipse</a>
<li>
Extract the downloaded .zip/.tar.gz file
<li>
Start eclipse by running the eclipse command or clicking eclipse application from the file manager
<li>
Create and run HelloWorld.scala, see <a href = "https://www.assembla.com/spaces/scala-ide/wiki/tutorial">tutorial</a>
</ol>

<p>
For more information about the source code, see
<a href = "src/README_src.hmtl">src/README_src.hmtl</a>.

<A name = "papers">

<p><hr><p>
<h3> Papers/Documentation </h3>
<p>

<ol>
<li>
John A. Miller, Jun Han and Maria Hybinette,
<a href = "http://informs-sim.org/wsc10papers/067.pdf">
"Using Domain Specific Languages for Modeling and Simulation: ScalaTion as a Case Study,"</a>
Proceedings of the 2010 ACM/IEEE Winter Simulation Conference (WSC'10),
Baltimore, Maryland (December 2010) pp. 741-752.

<p>
<li>
Michael E. Cotterell, John A. Miller, Tom Horton,
<a href = "http://arxiv.org/abs/1112.1751">
"Unicode in Domain-Specific Programming Languages for Modeling & Simulation:
ScalaTion as a Case Study,"</a>
Arxiv preprint arXiv:1112.175
(December 2011) pp. 1-10.

<p>
<li>
Michael E. Cotterell, John A. Miller, Jun Han and Tom Horton,
<a href = "../scalation_papers/alasim/alasim_extended_abstract.pdf">
"Extending ScalaTion, a Domain-Specific Language for Modeling & Simulation, for Simulation Optimization,"</a>
Proceedings of the AlaSim International Modeling and Simulation Conference & Exhibition (AlaSim'12),
Huntsville, Alabama (May 2012) pp. 1-1.

<p>
<li>
Yung Long Li,
<a href = "../home/theses/li_thesis/thesis/TR_Yung_Long_Li.pdf">
"Evaluation of Parallel Implementations of Dense and Sparse
Matrices for the ScalaTion Library," </a>
Technical Report,
University of Georgia (December 2012) pp. 1-60.

<p>
<li>
John A. Miller, Michael E. Cotterell and Stephen J. Buckley,
<a href = "http://informs-sim.org/wsc13papers/includes/files/104.pdf">
"Supporting a Modeling Continuum in ScalaTion: From Predictive Analytics to Simulation Modeling,"</a>
Proceedings of the 2013 ACM/IEEE Winter Simulation Conference (WSC'13),
Washington, DC (December 2013) pp. 1191-1202.

<p>
<li>
Matthew Saltz, Ayushi Jain, Abhishek Kothari, Arash Fard, John A. Miller, and Lakshmish Ramaswamy,
<a href = "http://www.thecloudcomputing.org/2014/AdvanceProgram-ICWS-SCC-CLOUD-MS-BigDataCongress-SERVICES-2014.pdf">
"DualIso: An Algorithm for Subgraph Pattern Matching on Very Large Labeled Graphs,"</a>
<I> Proceedings of the 3rd IEEE International Congress on Big Data </I>
(<a href = "http://www.ieeebigdata.org/2014">BigData'14</a>),
Anchorage, Alaska (June-July 2014) pp. 498-505.
<br>
Online <a href = "../home/theses/jain_thesis/bigdata_2014/BigDataCong2014_DualIso_Supplement.pdf">supplement</a>

</ol>
"""

    val postfix = raw"""
<p>
<h3> Code Release Process </h3>
<p>

<pre><code>
$$ cd scalation_$scalationVer <br>
$$ cd scalation_mathstat <br>
$$ sbt <br>
> compile <br>
> package <br>
> test <br>
> runMain scalation.util.RunSpellCheck 'package-directory' <br>
> runMain scalation.GenIndexHtml <br>
> clean <br>
> doc <br>
> exit
</code></pre>

Similarly for scalation_modeling and scalation_models.
Currently the .jar files need to be copied (copy_jars.sh) into the lib directory of dependent subprojects.

<p>
</body>
</html>
"""

    val readmeOut = new PrintWriter(BASE + ⁄ + "README.html")

    readmeOut.println (prefix)
    readmeOut.println (Source.fromFile (fApi).mkString)
    readmeOut.println (Source.fromFile (fSrc).mkString)
    readmeOut.println (postfix)

    readmeOut.close

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively write table rows for each package.
     *  @param f            the directory to examine
     *  @param module       the module name (i.e. scalation_mathstat)
     *  @param parPath      the path of the parent directories (i.e. scalation/util)
     *  @param parName      the name of the parent directories (i.e. scalation.util)
     *  @param depth        the depth of the project structure
     *  @param isTestPac    flag for whether dealing with unit testing packages or not
     */
    def recWrite (f: File, module: String, parPath : String, parName: String, depth: Int, isTestPac: Boolean = false)
    {
        val dirs     = f.listFiles                                              // list files in the current directory
        val mainTest = if (isTestPac) "test" else "main"
        for (fi <- dirs if (fi.isDirectory && !fi.getName.contains (SKIP))) {   // process each subpackage in the current package/directory
            val pacName      = fi.getName
            val hyphens      = getHyphens (depth)
            val descriptions = getDescriptions (new File (fi.getAbsolutePath + ⁄ + "package.scala"))
            if (!isTestPac) outApi.println (s"""<tr>\n<td> ${hyphens}<a href = "$module/target/scala-$scalaVer/api/$parPath/$pacName/index.html"> $parName.$pacName </a>\n<td> $descriptions""")
            outSrc.println (s"""<tr>\n<td> ${hyphens}<a href = "$module/src/${mainTest}/scala/$parPath/$pacName/"> $parName.$pacName </a>\n<td> $descriptions""")
            recWrite (fi, module, s"$parPath/$pacName", s"$parName.$pacName", depth+1, isTestPac)
        } // for
    } // recWrite

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the number of hyphens for the sub-packages.
     *  @param depth    the depth of the project structure
     */
    def getHyphens (depth: Int): String =
    {
        val sb = new StringBuilder
        for (i <- 0 until depth) sb.append ("- ")
        sb.toString
    } // getHyphens

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the Descriptions of a package from its package.scala comments.
     *  @param f    the package.scala file
     */
    def getDescriptions (f: File): String =
    {
        if (f.exists) {
            val scan       = new Scanner (f)
            var foundStart = false              // flag for fining the starting point of search, which is the package line
            var contSearch = true               // flag for continuing with the search (has not encountered */)
            var sb         = new StringBuilder
            while (contSearch && scan.hasNextLine) {
                var line = scan.nextLine
                if (foundStart) {
                    if (line.startsWith ("/**")) {
                        line = line.substring (3).trim
                        sb.append (line)
                        sb += ' '
                    } else if (line.startsWith (" */")) {
                        contSearch = false
                    } else if (line.startsWith (" *")) {
                        line = line.substring (2).trim
                        sb.append (line)
                        sb += ' '
                    } // if
                } else if (line.contains ("package")) foundStart = true
            } // while
            sb.toString
        } else s"There is currently no `package.scala` for ${f.getParentFile.getName}"
    } // getDescriptions

} // GenReadmeHtml object

