#!/bin/bash

set -e
set -x

apt-get update 
apt-get install -y wget sbcl build-essential 
# apt-get install -y valgrind
# apt-get build-dep -y sbcl
wget  https://github.com/sbcl/sbcl/archive/refs/tags/sbcl-2.5.4.tar.gz 
tar xvzf sbcl-2.5.4.tar.gz
cd sbcl-sbcl-2.5.4/ && sh make.sh && INSTALL_ROOT=/usr/local sh install.sh
