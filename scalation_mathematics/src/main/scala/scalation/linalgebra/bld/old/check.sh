
#######################################################################
# check.sh - script to check differences between existing and generated
# source files:
# vector traits          > run-main scalation.linalgebra.bld.BldVecto
# vector classes         > run-main scalation.linalgebra.bld.BldVector
# matrix traits          > run-main scalation.linalgebra.bld.BldMatri
# matrix classes         > run-main scalation.linalgebra.bld.BldMatrix
# sparse matrix classes  > run-main scalation.linalgebra.bld.BldSparseMatrix
# symtri matrix classes  > run-main scalation.linalgebra.bld.BldSymTriMatrix
# bid matrix classes     > run-main scalation.linalgebra.bld.BldBidMatrix

#######################################################################
# check vector traits

echo diff VectoI.scalaa ../VectoI.scala
diff VectoI.scalaa ../VectoI.scala

echo diff VectoL.scalaa ../VectoL.scala
diff VectoL.scalaa ../VectoL.scala

echo diff VectoD.scalaa ../VectoD.scala
diff VectoD.scalaa ../VectoD.scala

echo diff VectoC.scalaa ../VectoC.scala
diff VectoC.scalaa ../VectoC.scala

echo diff VectoR.scalaa ../VectoR.scala
diff VectoR.scalaa ../VectoR.scala

echo diff VectoQ.scalaa ../VectoQ.scala
diff VectoQ.scalaa ../VectoQ.scala

#######################################################################
# check vector classes

echo diff VectorI.scalaa ../VectorI.scala
diff VectorI.scalaa ../VectorI.scala

echo diff VectorL.scalaa ../VectorL.scala
diff VectorL.scalaa ../VectorL.scala

echo diff VectorD.scalaa ../VectorD.scala
diff VectorD.scalaa ../VectorD.scala

echo diff VectorC.scalaa ../VectorC.scala
diff VectorC.scalaa ../VectorC.scala

echo diff VectorR.scalaa ../VectorR.scala
diff VectorR.scalaa ../VectorR.scala

echo diff VectorQ.scalaa ../VectorQ.scala
diff VectorQ.scalaa ../VectorQ.scala

#######################################################################
# check matrix traits

echo diff MatriI.scalaa ../MatriI.scala
diff MatriI.scalaa ../MatriI.scala

echo diff MatriL.scalaa ../MatriL.scala
diff MatriL.scalaa ../MatriL.scala

echo diff MatriD.scalaa ../MatriD.scala
diff MatriD.scalaa ../MatriD.scala

echo diff MatriC.scalaa ../MatriC.scala
diff MatriC.scalaa ../MatriC.scala

echo diff MatriR.scalaa ../MatriR.scala
diff MatriR.scalaa ../MatriR.scala

echo diff MatriQ.scalaa ../MatriQ.scala
diff MatriQ.scalaa ../MatriQ.scala

#######################################################################
# check matrix classes

echo diff MatrixI.scalaa ../MatrixI.scala
diff MatrixI.scalaa ../MatrixI.scala

echo diff MatrixL.scalaa ../MatrixL.scala
diff MatrixL.scalaa ../MatrixL.scala

echo diff MatrixD.scalaa ../MatrixD.scala
diff MatrixD.scalaa ../MatrixD.scala

echo diff MatrixC.scalaa ../MatrixC.scala
diff MatrixC.scalaa ../MatrixC.scala

echo diff MatrixR.scalaa ../MatrixR.scala
diff MatrixR.scalaa ../MatrixR.scala

echo diff MatrixQ.scalaa ../MatrixQ.scala
diff MatrixQ.scalaa ../MatrixQ.scala

#######################################################################
# check sparse matrix classes

echo diff SparseMatrixI.scalaa ../SparseMatrixI.scala
diff SparseMatrixI.scalaa ../SparseMatrixI.scala

echo diff SparseMatrixL.scalaa ../SparseMatrixL.scala
diff SparseMatrixL.scalaa ../SparseMatrixL.scala

echo diff SparseMatrixD.scalaa ../SparseMatrixD.scala
diff SparseMatrixD.scalaa ../SparseMatrixD.scala

echo diff SparseMatrixC.scalaa ../SparseMatrixC.scala
diff SparseMatrixC.scalaa ../SparseMatrixC.scala

echo diff SparseMatrixR.scalaa ../SparseMatrixR.scala
diff SparseMatrixR.scalaa ../SparseMatrixR.scala

echo diff SparseMatrixQ.scalaa ../SparseMatrixQ.scala
diff SparseMatrixQ.scalaa ../SparseMatrixQ.scala

#######################################################################
# check symtri matrix classes

echo diff SymTriMatrixI.scalaa ../SymTriMatrixI.scala
diff SymTriMatrixI.scalaa ../SymTriMatrixI.scala

echo diff SymTriMatrixL.scalaa ../SymTriMatrixL.scala
diff SymTriMatrixL.scalaa ../SymTriMatrixL.scala

echo diff SymTriMatrixD.scalaa ../SymTriMatrixD.scala
diff SymTriMatrixD.scalaa ../SymTriMatrixD.scala

echo diff SymTriMatrixC.scalaa ../SymTriMatrixC.scala
diff SymTriMatrixC.scalaa ../SymTriMatrixC.scala

echo diff SymTriMatrixR.scalaa ../SymTriMatrixR.scala
diff SymTriMatrixR.scalaa ../SymTriMatrixR.scala

echo diff SymTriMatrixQ.scalaa ../SymTriMatrixQ.scala
diff SymTriMatrixQ.scalaa ../SymTriMatrixQ.scala

#######################################################################
# check bid matrix classes

echo diff BidMatrixI.scalaa ../BidMatrixI.scala
diff BidMatrixI.scalaa ../BidMatrixI.scala

echo diff BidMatrixL.scalaa ../BidMatrixL.scala
diff BidMatrixL.scalaa ../BidMatrixL.scala

echo diff BidMatrixD.scalaa ../BidMatrixD.scala
diff BidMatrixD.scalaa ../BidMatrixD.scala

echo diff BidMatrixC.scalaa ../BidMatrixC.scala
diff BidMatrixC.scalaa ../BidMatrixC.scala

echo diff BidMatrixR.scalaa ../BidMatrixR.scala
diff BidMatrixR.scalaa ../BidMatrixR.scala

echo diff BidMatrixQ.scalaa ../BidMatrixQ.scala
diff BidMatrixQ.scalaa ../BidMatrixQ.scala

