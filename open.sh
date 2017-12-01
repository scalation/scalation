
####################################################################################################
# open.sh - open source and doc file for web access

#####################################################################################################
# subproject dependencies:
#     scalation_mathstat dependes on:
#     scalation_modeling dependes on: scalation_mathstat
#     scalation_models   dependes on: scalation_mathstat; scalation_modeling

echo // "chmod 444 *.*"
echo // "cd scalation_mathstat; chmod 755 .; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_database; chmod 755 .; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_modeling; chmod 755 .; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_models; chmod 755 .; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."

chmod 444 *.*
cd scalation_mathstat; chmod 755 .; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_database; chmod 755 .; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_modeling; chmod 755 .; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_models; chmod 755 .; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..

