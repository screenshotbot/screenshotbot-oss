#!/bin/sh

set +e

OUTPUT=~/screenshotbot
RECORDER=$OUTPUT/recorder
mkdir $OUTPUT 2>/dev/null || true
rm -f $RECORDER
cp sdk $RECORDER
chmod a+x $RECORDER


if [ -e sdk.lwheap ] ; then
    rm -f $RECORDER.lwheap
    cp sdk.lwheap $RECORDER.lwheap
fi

echo "Installed ~/screenshotbot/recorder"
