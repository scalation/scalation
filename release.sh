
####################################################################################################
# releae.sh - build all subprojects for release and copy jar files from subprojects to lib directories
# only run GenIndexHtml for base module (scalation_mathematics)

#####################################################################################################
# subproject dependencies:
#     scalation_mathematics dependes on:
#     scalation_statistics  dependes on: scalation_mathematics
#     scalation_database    dependes on: scalation_mathematics; scalation_statistics
#     scalation_modeling    dependes on: scalation_mathematics; scalation_statistics; scalation_database
#     scalation_models      dependes on: scalation_mathematics; scalation_statistics; scalation_database; scalation_modeling

#####################################################################################################
# build the `scalation_mathematics` subproject and copy its jar file to all dependent subprojects

echo ================================= scalation_mathematics ===========================================
echo // "cd scalation_mathematics; sbt compile package; sbt runMain scalation.GenIndexHtml"
cd scalation_mathematics; sbt compile package; sbt "runMain scalation.GenIndexHtml"
echo // "cp target/scala-2.12/scalation_mathematics_2.12-1.6.jar ../lib"
cp target/scala-2.12/scalation_mathematics_2.12-1.6.jar ../lib
echo // "sbt clean; sbt doc; cd .."
sbt clean; sbt doc; cd ..

#####################################################################################################
# build the `scalation_statistics` subproject and copy its jar file to all dependent subprojects

echo ================================= scalation_statistics ===========================================
echo // "cd scalation_statistics; sbt compile package"
cd scalation_statistics; sbt compile package
echo // "cp target/scala-2.12/scalation_statistics_2.12-1.6.jar ../lib"
cp target/scala-2.12/scalation_statistics_2.12-1.6.jar ../lib
echo // "sbt clean; sbt doc; cd .."
sbt clean; sbt doc; cd ..

#####################################################################################################
# build the `scalation_database` subproject and copy its jar file to all dependent subprojects

echo ================================= scalation_database ===========================================
echo // "cd scalation_database; sbt compile package"
cd scalation_database; sbt compile package
echo // "cp target/scala-2.12/scalation_database_2.12-1.6.jar ../lib"
cp target/scala-2.12/scalation_database_2.12-1.6.jar ../lib
echo // "sbt clean; sbt doc; cd .."
sbt clean; sbt doc; cd ..

#####################################################################################################
# build the `scalation_modeling` subproject and copy its jar file to the dependent subproject

echo ================================= scalation_modeling ===========================================
echo // "cd scalation_modeling; sbt compile package"
cd scalation_modeling; sbt compile package
echo // "cp target/scala-2.12/scalation_modeling_2.12-1.6.jar ../lib"
cp target/scala-2.12/scalation_modeling_2.12-1.6.jar ../lib
echo // "sbt clean; sbt doc; cd .."
sbt clean; sbt doc; cd ..

#####################################################################################################
# build the `scalation_models` subproject

echo ================================== scalation_models ============================================
echo // "cd scalation_models; sbt compile"
cd scalation_models; sbt compile
echo // "sbt clean; sbt doc; cd .."
sbt clean; sbt doc; cd ..

