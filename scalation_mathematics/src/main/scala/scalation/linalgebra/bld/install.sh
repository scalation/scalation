
#######################################################################
# install.sh - script to install generated source files:
#----------------------------------------------------------------------
# vector traits          > run-main scalation.linalgebra.bld.BldVecto
# vector classes         > run-main scalation.linalgebra.bld.BldVector
# sparse vector classes  > run-main scalation.linalgebra.bld.BldSparseVector
# RLE vector classes     > run-main scalation.linalgebra.bld.BldRleVector
#----------------------------------------------------------------------
# matrix traits          > run-main scalation.linalgebra.bld.BldMatri
# matrix classes         > run-main scalation.linalgebra.bld.BldMatrix
# sparse matrix classes  > run-main scalation.linalgebra.bld.BldSparseMatrix
# symtri matrix classes  > run-main scalation.linalgebra.bld.BldSymTriMatrix
# bid matrix classes     > run-main scalation.linalgebra.bld.BldBidMatrix
# RLE matrix classes     > run-main scalation.linalgebra.bld.BldRleMatrix
#----------------------------------------------------------------------
# Note: do not install "S" versions for matrix classes (last 5)
#----------------------------------------------------------------------
# May also > run-main scalation.linalgebra.bld.BldAll
#----------------------------------------------------------------------

#######################################################################
# vectors
#######################################################################
# install vector traits

echo cp VectoI.scalaa ../VectoI.scala
cp VectoI.scalaa ../VectoI.scala

echo cp VectoL.scalaa ../VectoL.scala
cp VectoL.scalaa ../VectoL.scala

echo cp VectoD.scalaa ../VectoD.scala
cp VectoD.scalaa ../VectoD.scala

echo cp VectoC.scalaa ../VectoC.scala
cp VectoC.scalaa ../VectoC.scala

echo cp VectoR.scalaa ../VectoR.scala
cp VectoR.scalaa ../VectoR.scala

echo cp VectoQ.scalaa ../VectoQ.scala
cp VectoQ.scalaa ../VectoQ.scala

echo cp VectoS.scalaa ../VectoS.scala
cp VectoS.scalaa ../VectoS.scala

echo cp VectoT.scalaa ../VectoT.scala
cp VectoT.scalaa ../VectoT.scala

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

echo cp VectorT.scalaa ../VectorT.scala
cp VectorT.scalaa ../VectorT.scala

#######################################################################
# install sparse vector classes

echo cp SparseVectorI.scalaa ../SparseVectorI.scala
cp SparseVectorI.scalaa ../SparseVectorI.scala

echo cp SparseVectorL.scalaa ../SparseVectorL.scala
cp SparseVectorL.scalaa ../SparseVectorL.scala

echo cp SparseVectorD.scalaa ../SparseVectorD.scala
cp SparseVectorD.scalaa ../SparseVectorD.scala

echo cp SparseVectorC.scalaa ../SparseVectorC.scala
cp SparseVectorC.scalaa ../SparseVectorC.scala

echo cp SparseVectorR.scalaa ../SparseVectorR.scala
cp SparseVectorR.scalaa ../SparseVectorR.scala

echo cp SparseVectorQ.scalaa ../SparseVectorQ.scala
cp SparseVectorQ.scalaa ../SparseVectorQ.scala

echo cp SparseVectorS.scalaa ../SparseVectorS.scala
cp SparseVectorS.scalaa ../SparseVectorS.scala

echo cp SparseVectorT.scalaa ../SparseVectorT.scala
cp SparseVectorT.scalaa ../SparseVectorT.scala

#######################################################################
# install compressed (RLE) vector classes

echo cp RleVectorI.scalaa ../RleVectorI.scala
cp RleVectorI.scalaa ../RleVectorI.scala

echo cp RleVectorL.scalaa ../RleVectorL.scala
cp RleVectorL.scalaa ../RleVectorL.scala

echo cp RleVectorD.scalaa ../RleVectorD.scala
cp RleVectorD.scalaa ../RleVectorD.scala

echo cp RleVectorC.scalaa ../RleVectorC.scala
cp RleVectorC.scalaa ../RleVectorC.scala

echo cp RleVectorR.scalaa ../RleVectorR.scala
cp RleVectorR.scalaa ../RleVectorR.scala

echo cp RleVectorQ.scalaa ../RleVectorQ.scala
cp RleVectorQ.scalaa ../RleVectorQ.scala

echo cp RleVectorS.scalaa ../RleVectorS.scala
cp RleVectorS.scalaa ../RleVectorS.scala

echo cp RleVectorT.scalaa ../RleVectorT.scala
cp RleVectorT.scalaa ../RleVectorT.scala

#######################################################################
# matrices
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

echo cp MatriT.scalaa ../MatriT.scala
cp MatriT.scalaa ../MatriT.scala

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

# Note: no MatrixS.scala for StrNum matrices
# Note: no MatrixT.scala for TimeNum matrices

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

# Note: no SparseMatrixS.scala for StrNum matrices
# Note: no SparseMatrixT.scala for TimeNum matrices

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

# Note: no SymTriMatrixS.scala for StrNum matrices
# Note: no SymTriMatrixT.scala for TimeNum matrices

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

# Note: no BidMatrixS.scala for StrNum matrices
# Note: no BidMatrixT.scala for TimeNum matrices

#######################################################################
# install compressed (RLE) matrix classes

echo cp RleMatrixI.scalaa ../RleMatrixI.scala
cp RleMatrixI.scalaa ../RleMatrixI.scala

echo cp RleMatrixL.scalaa ../RleMatrixL.scala
cp RleMatrixL.scalaa ../RleMatrixL.scala

echo cp RleMatrixD.scalaa ../RleMatrixD.scala
cp RleMatrixD.scalaa ../RleMatrixD.scala

echo cp RleMatrixC.scalaa ../RleMatrixC.scala
cp RleMatrixC.scalaa ../RleMatrixC.scala

echo cp RleMatrixR.scalaa ../RleMatrixR.scala
cp RleMatrixR.scalaa ../RleMatrixR.scala

echo cp RleMatrixQ.scalaa ../RleMatrixQ.scala
cp RleMatrixQ.scalaa ../RleMatrixQ.scala

# Note: no RleMatrixS.scala for StrNum matrices
# Note: no RleMatrixY.scala for TimeNum matrices

