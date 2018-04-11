#! /bin/bash -v

xvfb-run --server-args="-screen 0 1024x768x24" /usr/local/lib/unity3d/Editor/Unity -batchmode -projectPath "$(pwd)/Lumiere" -force-opengl -runTests -testPlatform playmode -testResults "$HOME/build_logs/playmode-$CI_JOB_ID.xml"
/usr/local/lib/unity3d/Editor/Unity -nographics -batchmode -projectPath "$(pwd)/Lumiere" -runTests -testPlatform editmode -testResults "$HOME/build_logs/editor-$CI_JOB_ID.xml"
cd unity-testresult-parser
stack setup
stack build
TESTFILES=$(echo $HOME/build_logs/{playmode,editor}-$CI_JOB_ID.xml)
stack exec unity-testresult-parser -- --color=yes --summary $TESTFILES
exit $?
