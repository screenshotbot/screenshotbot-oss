#!/bin/bash
## Generate a directory of n screenshots
##  two arguments: directory and n

set -e
set -x

dir=$1
num=$2

mkdir -p $dir

MAGICK=""

if [ `git rev-parse --abbrev-ref HEAD` = master ] ; then
    sleep 0

fi

if magick --help ; then
    MAGICK=magick
fi


for i in `seq 0 $num`; do
    $MAGICK convert -size 360x360 xc:white -font "FreeMono" -pointsize 12 -fill black -stroke black -draw "text 15,15 \"$(date) $i\""  -strip $dir/image$i.png
done

