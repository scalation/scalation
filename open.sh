
####################################################################################################
# open.sh - open source and doc file for web access

#####################################################################################################
# subproject dependencies:
#     scalation_mathematics dependes on:
#     scalation_statistics  dependes on: scalation_mathematics
#     scalation_database    dependes on: scalation_mathematics; scalation_statistics
#     scalation_modeling    dependes on: scalation_mathematics; scalation_statistics; scalation_database
#     scalation_models      dependes on: scalation_mathematics; scalation_statistics; scalation_database; scalation_modeling

echo // "chmod 444 *.*"
echo // "chmod -R 755 lib"
echo // "cd scalation_mathematics; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_statistics; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_database; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_modeling; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_models; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."

chmod 444 *.*
chmod -R 755 lib
cd scalation_mathematics; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_statistics; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_database; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_modeling; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_models; chmod 755 .; chmod 444 build.sbt; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..

