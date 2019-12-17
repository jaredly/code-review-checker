#!/bin/bash
set -ex
echo Hi
rm -rf CodeReviewChecker.app
./macapp.sh CodeReviewChecker icon.png
DEST=CodeReviewChecker.app/Contents/MacOS
BASE=`esy echo '#{self.target_dir}'`
cp $BASE/install/default/bin/CodeReviewCheckerProd $DEST/CodeReviewChecker
# cp -r ../assets   CodeReviewChecker.app/Contents/MacOS/
# git rev-parse HEAD > CodeReviewChecker.app/Contents/MacOS/assets/git-head
zip -r CodeReviewChecker.zip CodeReviewChecker.app
