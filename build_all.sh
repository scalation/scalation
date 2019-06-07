
####################################################################################################
# build_all.sh - build all subprojects and copy jar files from subprojects to lib directories

#####################################################################################################
# subproject dependencies:
#     scalation_mathematics dependes on:
#     scalation_statistics  dependes on: scalation_mathematics
#     scalation_database    dependes on: scalation_mathematics; scalation_statistics
#     scalation_modeling    dependes on: scalation_mathematics; scalation_statistics; scalation_database
#     scalation_models      dependes on: scalation_mathematics; scalation_statistics; scalation_database; scalation_modeling

#####################################################################################################
# build the scalation_mathematics subproject and copy its jar file to both dependent subprojects

echo ================================ scalation_mathematics =========================================
echo // "cd scalation_mathematics; sbt compile package; cd .."
cd scalation_mathematics; sbt compile package; cd ..
echo // "cp scalation_mathematics/target/scala-2.12/scalation_mathematics_2.12-1.6.jar lib"
cp scalation_mathematics/target/scala-2.12/scalation_mathematics_2.12-1.6.jar lib

#####################################################################################################
# build the scalation_statistics subproject and copy its jar file to both dependent subprojects

echo ================================ scalation_statistics ==========================================
echo // "cd scalation_statistics; sbt compile package; cd .."
cd scalation_statistics; sbt compile package; cd ..
echo // "cp scalation_statistics/target/scala-2.12/scalation_statistics_2.12-1.6.jar lib"
cp scalation_statistics/target/scala-2.12/scalation_statistics_2.12-1.6.jar lib

#####################################################################################################
# build the scalation_database subproject and copy its jar file to the dependent subproject

echo ================================= scalation_database ===========================================
echo // "cd scalation_database; sbt compile package; cd .."
cd scalation_database; sbt compile package; cd ..
echo // "cp scalation_database/target/scala-2.12/scalation_database_2.12-1.6.jar lib"
cp scalation_database/target/scala-2.12/scalation_database_2.12-1.6.jar lib

#####################################################################################################
# build the scalation_modeling subproject and copy its jar file to the dependent subproject

echo ================================= scalation_modeling ===========================================
echo // "cd scalation_modeling; sbt compile package; cd .."
cd scalation_modeling; sbt compile package; cd ..
echo // "cp scalation_modeling/target/scala-2.12/scalation_modeling_2.12-1.6.jar lib"
cp scalation_modeling/target/scala-2.12/scalation_modeling_2.12-1.6.jar lib

#####################################################################################################
# build the scalation_models subproject

echo ================================== scalation_models ============================================
echo // "cd scalation_models; sbt compile; cd .."
cd scalation_models; sbt compile; cd ..

