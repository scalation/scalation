
####################################################################################################
# build_modeling.sh - build modeling and models subprojects and copy jar files from subprojects to lib directories
# assumes scalation_mathematics; scalation_statistics; scalation_database have already been built

#####################################################################################################
# subproject dependencies:
#     scalation_mathematics dependes on:
#     scalation_statistics  dependes on: scalation_mathematics
#     scalation_database    dependes on: scalation_mathematics; scalation_statistics
#     scalation_modeling    dependes on: scalation_mathematics; scalation_statistics; scalation_database
#     scalation_models      dependes on: scalation_mathematics; scalation_statistics; scalation_database; scalation_modeling


#####################################################################################################
# build the scalation_modeling subproject and copy its jar file to the dependent subproject

echo ================================= scalation_modeling ===========================================
echo // "cd scalation_modeling; sbt compile package; cd .."
cd scalation_modeling; sbt compile package; cd ..
echo // "cp scalation_modeling/target/scala-2.12/scalation_modeling_2.12-1.6.jar scalation_models/lib"
cp scalation_modeling/target/scala-2.12/scalation_modeling_2.12-1.6.jar scalation_models/lib

#####################################################################################################
# build the scalation_models subproject

echo ================================== scalation_models ============================================
echo // "cd scalation_models; sbt compile; cd .."
cd scalation_models; sbt compile; cd ..

