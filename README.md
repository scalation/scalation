##Build Status
[![Build Status](https://travis-ci.org/mvnural/scalation.svg)](https://travis-ci.org/mvnural/scalation)

#SCALAble SimulaTION - ScalaTion

Welcome to **ScalaTion**, the Scala-based system for Simulation, Optimization and Analytics.

This system, coded in Scala, supports multi-paradigm simulation modeling including
'tableau', 'event', 'process', 'dynamics', 'dynamics_pde', 'activity' and 'state' oriented models.

Scala is a modern object-oriented, functional programming language that is well-suited for developing simulation engines. It is in the Java family of languages and can call Java code. The inclusion of advanced and functional programming capabilities, makes the code much more concise than Java.

ScalaTion also supports optimization and analytics as they nowadays go hand-in-hand with simulation modeling.
Some of the algorithms come in both sequential and parallel versions.

The **ScalaTion 1.2** version defines multiple .par subpackages that contain parallel versions of sequential algorithms.
Future directions include completing the 'scala3d' and 'physics' packages.

## License
Please read the <a href = LICENSE.html> LICENSE </a> file (an MIT style license).

## Installation Instructions

Requires:
<a href = "http://www.scala-lang.org/downloads">Scala 2.12.x</a> and
<a href = "http://www.oracle.com/technetwork/java/javase/downloads">Java 8</a>

Recommended:
<a href = "http://www.scala-sbt.org/">sbt</a> 
(see <a href = "http://www.scala-sbt.org/0.13/tutorial/Setup.html">Setup.html</a>)

### Issue the following command to checkout from github (requires git)
`git clone https://github.com/scalation/scalation.git scalation_1.2`

### (Alternative Method) Download one of the following two files from ScalaTion website (http://cobweb.cs.uga.edu/~jam/scalation_1.2/README.html)

<a href = "http://cobweb.cs.uga.edu/~jam/scalation_1.2.tar.gz"> scalation_1.2.tar.gz </a> <br> <b>OR</b> <br>
<a href = "http://cobweb.cs.uga.edu/~jam/scalation_1.2.zip"> scalation_1.2.zip </a>

<p>
<h4>Untar or unzip the file</h4>

<pre><code>
$ tar xvfz scalation_1.2.tar.gz <br> <b>OR</b> <br>
$ unzip scalation_1.2.zip
</code></pre>



<!--
<p>
<h4>3. Export the SCALATION_CLASSES environment variable to point at class files</h4>

<pre><code>
export SCALATION_CLASSES=$HOME/scalation_1.2/target/scala-2.12.0-M2/classes
</code></pre>

<p>
Adjust the prefix to correspond to where you installed scalation.
Ideally, put the export in one of your dot files (e.g., .profile or .bashrc).
-->

## Usage

To compile code or run apps, enter sbt and type compile, run-main or exit

$ cd scalation_1.2 <br>
$ sbt <br>
> compile <br>
> run-main apps.process.Bank <br>
> run-main scalation.analytics.RegressionTest <br>
> exit



ScalaTion 1.2 is set up to use the Simple Build Tool <a href = "http://www.scala-sbt.org">sbt</a>.


ScalaTion 1.2 uses the following build specification file
<a href = "build.sbt">build.sbt</a>.
Source packages are in the <b>src/main/scala</b> directory,
class files are in the <b>target/scala-2.12.0-M2/classes</b> directory, and
documentation files are in the <b>target/scala-2.12.0-M2/api</b> directory.
Data file input, output or analytics are stored in subdirectories of <b>data</b>,
while database files are stored in <b>store</b>.

### To develop using an IDE:
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


For more information about the source code, see 
<a href = "src/README_src.hmtl">src/README_src.hmtl</a>.

## Papers/Documentation ##


<ol>
<li>
John A. Miller, Jun Han and Maria Hybinette,
<a href = "http://informs-sim.org/wsc10papers/067.pdf">
"Using Domain Specific Languages for Modeling and Simulation: ScalaTion as a Case Study,"</a>
Proceedings of the 2010 ACM/IEEE Winter Simulation Conference (WSC'10),
Baltimore, Maryland (December 2010) pp. 741-752.


<li>
Michael E. Cotterell, John A. Miller, Tom Horton,
<a href = "http://arxiv.org/abs/1112.1751">
"Unicode in Domain-Specific Programming Languages for Modeling & Simulation:
ScalaTion as a Case Study,"</a>
Arxiv preprint arXiv:1112.175
(December 2011) pp. 1-10.


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


