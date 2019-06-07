
Structure of the `mathematics` module

The `mathematics` module is divided into lower and upper parts, where compilation
of a part is only dependent parts below it.

1) Lower Part
files: GenIndexHtml.scala, GenReadmeHtml.scala, package.scala, ReplaceOne.scala, Replace.scala, UpVersion.scala
packages: util, math

2) Upper Part
packages: linalgebra

With sbt may compile parts one by one by using "chmod 000".
> chmod 000 linalgebra
> chmod 700 linalgebra

