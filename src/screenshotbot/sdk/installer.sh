#!/bin/sh

set +e

if [ "$SCREENSHOTBOT_DIR" = "" ] ; then
    SCREENSHOTBOT_DIR=~/screenshotbot
fi

RECORDER=$SCREENSHOTBOT_DIR/recorder
mkdir $SCREENSHOTBOT_DIR 2>/dev/null || true
rm -f $RECORDER
cp sdk $RECORDER
chmod a+x $RECORDER


if [ -e sdk.lwheap ] ; then
    rm -f $RECORDER.lwheap
    cp sdk.lwheap $RECORDER.lwheap
fi

echo "Installed $RECORDER"
