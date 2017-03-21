
####################################################################################################
# open.sh - open source and doc file for web access

#####################################################################################################
# subproject dependencies:
#     scalation_mathstat dependes on:
#     scalation_modeling dependes on: scalation_mathstat
#     scalation_models   dependes on: scalation_mathstat; scalation_modeling

echo // "cd scalation_mathstat; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_modeling; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."
echo // "cd scalation_models; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd .."

cd scalation_mathstat; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_modeling; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..
cd scalation_models; chmod -R 755 src; chmod 755 target; chmod 755 target/scala-2.12; chmod -R 755 target/scala-2.12/api; cd ..

