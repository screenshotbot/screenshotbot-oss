FROM debian:stable AS builder

# Update copybara config if you change this line

RUN mkdir -p /opt/software
WORKDIR /opt/software
ADD https://github.com/Clozure/ccl/releases/download/v1.12/ccl-1.12-linuxx86.tar.gz ccl.tar.gz
RUN tar xvzf ccl.tar.gz
RUN apt-get update && apt-get install -y imagemagick html2text openjdk-11-jdk-headless gcc make
WORKDIR /app
COPY . .

# For non-oss version
RUN if ! [ -e launch.lisp ] ; then cp src/screenshotbot/oss/launch.lisp ./ ; fi
RUN test -f scripts/prepare-image.lisp

CMD /opt/software/ccl/lx86cl64 -l launch.lisp -- --object-store /data/
