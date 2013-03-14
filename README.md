# ScalaTion

Welcome to ScalaTion, the Scala-based library for Analytics, Simulation and Optimization.

This system coded in <a href = "http://www.scala-lang.org"> Scala </a> supports multi-paradigm
simulation modeling including 'dynamics', 'activity', 'event', 'process' and 'state' oriented models.
Scala is a modern object-oriented, functional programming language
(see <a href = "feature_matrix.html"> feature matrix </a>)
that is well-suited for developing simulation engines.
It also supports optimization and analytics.

Future directions include adding 'scala3d' and 'physics' packages.

Requires: Scala 2.9 and Java 6 or 7

## Installation Instructions </h3>

1. Download one of the following two file:

<p>
<a href = "../scalation.tar.gz"> scalation.tar.gz </a>
<br>
<a href = "../scalation.zip"> scalation.zip </a>

2. Unzip or untar the file:

    > tar xvfz scalation.tar.gz
    > unzip scalation.zip

3. If necessary re-build the scalation system (it's pre-built):

    > cd scalation
    > scalac Build.scala
    > scala Build

4. Run examples (e.g., for process oriented):

    > cd examples/process
    > scalac -cp ../../classes -d classes Bank.scala
    > scala -cp ../../classes:classes Bank

Please read the <a href = LICENSE.html> LICENSE </a> file (an MIT style license).

## Documentation/Papers

John A. Miller, Jun Han and Maria Hybinette, [Using Domain Specific Languages for Modeling and Simulation: ScalaTion as a Case Study](http://informs-sim.org/wsc10papers/067.pdf), Proceedings of the 2010 ACM/IEEE Winter Simulation Conference (WSC'10), Baltimore, Maryland (December 2010) pp. 741-752.

Michael E. Cotterell, John A. Miller, Tom Horton, [Unicode in Domain-Specific Programming Languages for Modeling & Simulation: ScalaTion as a Case Study](http://arxiv.org/abs/1112.1751), Arxiv preprint arXiv:1112.175 (December 2011) pp. 1-10.

