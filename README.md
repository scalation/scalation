# SCALAble SimulaTION - ScalaTion

**Welcome to ScalaTion, the Scala-based system for Simulation, Optimization and Analytics.**

This system, coded in Scala, supports multi-paradigm simulation modeling including
'tableau', 'event', 'process', 'dynamics', 'dynamics_pde', 'activity' and 'state'
oriented models.

<a href = "http://www.scala-lang.org">Scala</a> is a modern object-oriented, functional
programming language that is well-suited for developing simulation engines.
It is in the Java family of languages and can call Java code.
The inclusion of advanced and functional programming capabilities,
makes the code much more concise than Java.
ScalaTion also supports optimization and analytics as they nowadays go hand-in-hand
with simulation modeling.
Some of the algorithms come in both sequential and parallel versions.

The **ScalaTion 1.1.1** version defines multiple <code>.par</code> subpackages that
contain parallel versions of sequential algorithms.
Future directions include completing the '<code>scala3d</code>' and
'<code>physics</code>' packages.

## Installation Instructions </h3>

1. Clone the repository:

```
$ git clone git://github.com/scalation/scalation.git scalation
```

2. Build the scalation system:

```
$ sbt compile
```

4. Run examples (e.g., for process oriented):

```
$ sbt run process.Bank
```

Please read the <code>LICENSE</code> file (an MIT style license).

For more information, see the
<a href="http://cobweb.cs.uga.edu/~jam/scalation_1.1.1/README.html">ScalaTion Project</a>
page.

