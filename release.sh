
####################################################################################################
# releae.sh - build all subprojects for release and copy jar files from subprojects to lib directories

#####################################################################################################
# subproject dependencies:
#     scalation_mathstat dependes on:
#     scalation_modeling dependes on: scalation_mathstat
#     scalation_models   dependes on: scalation_mathstat; scalation_modeling

#####################################################################################################
# build the scalation_mathstat subproject and copy its jar file to both dependent subprojects

echo ================================= scalation_mathstat ===========================================
echo // "cd scalation_mathstat; sbt compile package; sbt run-main scalation.GenIndexHtml"
echo // "cp target/scala-2.12/scalation_mathstat_2.12-1.3.jar ../scalation_modeling/lib"
echo // "cp target/scala-2.12/scalation_mathstat_2.12-1.3.jar ../scalation_models/lib"
echo // "sbt clean doc; cd .."

cd scalation_mathstat; sbt compile package; sbt "run-main scalation.GenIndexHtml"
cp target/scala-2.12/scalation_mathstat_2.12-1.3.jar ../scalation_modeling/lib
cp target/scala-2.12/scalation_mathstat_2.12-1.3.jar ../scalation_models/lib
sbt clean doc; cd ..

#####################################################################################################
# build the scalation_modeling subproject and copy its jar file to the dependent subproject

echo ================================= scalation_modeling ===========================================
echo // "cd scalation_modeling; sbt compile package; sbt run-main scalation.GenIndexHtml"
echo // "cp target/scala-2.12/scalation_modeling_2.12-1.3.jar ../scalation_models/lib"
echo // "sbt clean doc; cd .."

cd scalation_modeling; sbt compile package; sbt "run-main scalation.GenIndexHtml"
cp target/scala-2.12/scalation_modeling_2.12-1.3.jar ../scalation_models/lib
sbt clean doc; cd ..

#####################################################################################################
# build the scalation_models subproject

echo ================================== scalation_models ============================================
echo // "cd scalation_models; sbt compile; sbt run-main scalation.GenIndexHtml"
echo // "sbt clean doc; cd .."

cd scalation_models; sbt compile; sbt "run-main scalation.GenIndexHtml"
sbt clean doc; cd ..

