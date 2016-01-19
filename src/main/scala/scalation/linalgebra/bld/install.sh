
#######################################################################
# install.sh - script to install generated source files:
# vector classes         > run-main scalation.linalgebra.bld.BldVector
# matrix traits          > run-main scalation.linalgebra.bld.BldMatri
# matrix classes         > run-main scalation.linalgebra.bld.BldMatrix
# sparse matrix classes  > run-main scalation.linalgebra.bld.BldSparseMatrix
# symtri matrix classes  > run-main scalation.linalgebra.bld.BldSymTriMatrix
# bid matrix classes     > run-main scalation.linalgebra.bld.BldBidMatrix

# Note: do not install "S" versions for specialized matrices (last 3)

#######################################################################
# install vector classes

echo cp VectorI.scalaa ../VectorI.scala
cp VectorI.scalaa ../VectorI.scala

echo cp VectorL.scalaa ../VectorL.scala
cp VectorL.scalaa ../VectorL.scala

echo cp VectorD.scalaa ../VectorD.scala
cp VectorD.scalaa ../VectorD.scala

echo cp VectorC.scalaa ../VectorC.scala
cp VectorC.scalaa ../VectorC.scala

echo cp VectorR.scalaa ../VectorR.scala
cp VectorR.scalaa ../VectorR.scala

echo cp VectorQ.scalaa ../VectorQ.scala
cp VectorQ.scalaa ../VectorQ.scala

echo cp VectorS.scalaa ../VectorS.scala
cp VectorS.scalaa ../VectorS.scala

#######################################################################
# install matrix traits

echo cp MatriI.scalaa ../MatriI.scala
cp MatriI.scalaa ../MatriI.scala

echo cp MatriL.scalaa ../MatriL.scala
cp MatriL.scalaa ../MatriL.scala

echo cp MatriD.scalaa ../MatriD.scala
cp MatriD.scalaa ../MatriD.scala

echo cp MatriC.scalaa ../MatriC.scala
cp MatriC.scalaa ../MatriC.scala

echo cp MatriR.scalaa ../MatriR.scala
cp MatriR.scalaa ../MatriR.scala

echo cp MatriQ.scalaa ../MatriQ.scala
cp MatriQ.scalaa ../MatriQ.scala

echo cp MatriS.scalaa ../MatriS.scala
cp MatriS.scalaa ../MatriS.scala

#######################################################################
# install matrix classes

echo cp MatrixI.scalaa ../MatrixI.scala
cp MatrixI.scalaa ../MatrixI.scala

echo cp MatrixL.scalaa ../MatrixL.scala
cp MatrixL.scalaa ../MatrixL.scala

echo cp MatrixD.scalaa ../MatrixD.scala
cp MatrixD.scalaa ../MatrixD.scala

echo cp MatrixC.scalaa ../MatrixC.scala
cp MatrixC.scalaa ../MatrixC.scala

echo cp MatrixR.scalaa ../MatrixR.scala
cp MatrixR.scalaa ../MatrixR.scala

echo cp MatrixQ.scalaa ../MatrixQ.scala
cp MatrixQ.scalaa ../MatrixQ.scala

# Note: no MatrixS.scala for specialized matrices

#######################################################################
# install sparse matrix classes

echo cp SparseMatrixI.scalaa ../SparseMatrixI.scala
cp SparseMatrixI.scalaa ../SparseMatrixI.scala

echo cp SparseMatrixL.scalaa ../SparseMatrixL.scala
cp SparseMatrixL.scalaa ../SparseMatrixL.scala

echo cp SparseMatrixD.scalaa ../SparseMatrixD.scala
cp SparseMatrixD.scalaa ../SparseMatrixD.scala

echo cp SparseMatrixC.scalaa ../SparseMatrixC.scala
cp SparseMatrixC.scalaa ../SparseMatrixC.scala

echo cp SparseMatrixR.scalaa ../SparseMatrixR.scala
cp SparseMatrixR.scalaa ../SparseMatrixR.scala

echo cp SparseMatrixQ.scalaa ../SparseMatrixQ.scala
cp SparseMatrixQ.scalaa ../SparseMatrixQ.scala

# Note: no SparseMatrixS.scala for specialized matrices

#######################################################################
# install symtri matrix classes

echo cp SymTriMatrixI.scalaa ../SymTriMatrixI.scala
cp SymTriMatrixI.scalaa ../SymTriMatrixI.scala

echo cp SymTriMatrixL.scalaa ../SymTriMatrixL.scala
cp SymTriMatrixL.scalaa ../SymTriMatrixL.scala

echo cp SymTriMatrixD.scalaa ../SymTriMatrixD.scala
cp SymTriMatrixD.scalaa ../SymTriMatrixD.scala

echo cp SymTriMatrixC.scalaa ../SymTriMatrixC.scala
cp SymTriMatrixC.scalaa ../SymTriMatrixC.scala

echo cp SymTriMatrixR.scalaa ../SymTriMatrixR.scala
cp SymTriMatrixR.scalaa ../SymTriMatrixR.scala

echo cp SymTriMatrixQ.scalaa ../SymTriMatrixQ.scala
cp SymTriMatrixQ.scalaa ../SymTriMatrixQ.scala

# Note: no SymTriMatrixS.scala for specialized matrices

#######################################################################
# install bid matrix classes

echo cp BidMatrixI.scalaa ../BidMatrixI.scala
cp BidMatrixI.scalaa ../BidMatrixI.scala

echo cp BidMatrixL.scalaa ../BidMatrixL.scala
cp BidMatrixL.scalaa ../BidMatrixL.scala

echo cp BidMatrixD.scalaa ../BidMatrixD.scala
cp BidMatrixD.scalaa ../BidMatrixD.scala

echo cp BidMatrixC.scalaa ../BidMatrixC.scala
cp BidMatrixC.scalaa ../BidMatrixC.scala

echo cp BidMatrixR.scalaa ../BidMatrixR.scala
cp BidMatrixR.scalaa ../BidMatrixR.scala

echo cp BidMatrixQ.scalaa ../BidMatrixQ.scala
cp BidMatrixQ.scalaa ../BidMatrixQ.scala

# Note: no BidMatrixS.scala for specialized matrices

