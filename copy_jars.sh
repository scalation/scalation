
####################################################################################################
# copy_jars.sh - copy jar files from subprojects to lib directories
#                assumes all subprojects have been compiled and packaged, else use build_all.sh

#####################################################################################################
# subproject dependencies:
#     scalation_mathstat dependes on:
#     scalation_modeling dependes on: scalation_mathstat
#     scalation_models   dependes on: scalation_mathstat; scalation_modeling

#####################################################################################################
# copy the scalation_mathstat subproject's jar file to both dependent subprojects

echo // "cp scalation_mathstat/target/scala-2.12/scalation_mathstat_2.12-1.3.jar scalation_modeling/lib"
echo // "cp scalation_mathstat/target/scala-2.12/scalation_mathstat_2.12-1.3.jar scalation_models/lib"

cp scalation_mathstat/target/scala-2.12/scalation_mathstat_2.12-1.3.jar scalation_modeling/lib
cp scalation_mathstat/target/scala-2.12/scalation_mathstat_2.12-1.3.jar scalation_models/lib

#####################################################################################################
# copy the scalation_modeling subproject's jar file to the dependent subproject

echo // "cp scalation_modeling/target/scala-2.12/scalation_modeling_2.12-1.3.jar scalation_models/lib"

cp scalation_modeling/target/scala-2.12/scalation_modeling_2.12-1.3.jar scalation_models/lib

