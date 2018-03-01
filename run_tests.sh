#! /bin/bash -v

xvfb-run --server-args="-screen 0 1024x768x24" /usr/local/lib/unity3d/Editor/Unity -batchmode -projectPath "$(pwd)/Lumiere" -force-opengl -runTests -testPlatform playmode -testResults "$HOME/build_logs/playmode-$CI_JOB_ID.xml"
/usr/local/lib/unity3d/Editor/Unity -nographics -batchmode -projectPath "$(pwd)/Lumiere" -runTests -testPlatform editmode -testResults "$HOME/build_logs/editor-$CI_JOB_ID.xml"
runghc "$(pwd)/parse.hs" $HOME/build_logs/{playmode,editor}-$CI_JOB_ID.xml
exit $?
