#!/bin/bash

set +e

OUTPUT=~/screenshotbot
RECORDER=$OUTPUT/recorder
mkdir $OUTPUT 2>/dev/null || true
cp sdk $RECORDER
chmod a+x $RECORDER

if [ -e sdk.lwheap ] ; then
    cp sdk.lwheap $RECORDER.lwheap
fi

echo "Installed ~/screenshotbot/recorder"
