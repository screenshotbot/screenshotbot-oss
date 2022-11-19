FROM debian:stable AS magick_base


RUN apt-get update && apt-get install -y libyaml-dev git-core libpng-dev zlib1g-dev libpng16-16  zlib1g gcc makeself exiftool build-essential logrotate imagemagick libmagickwand-dev


FROM magick_base AS builder

# Update copybara config if you change this line

RUN mkdir -p /opt/software
WORKDIR /opt/software
ADD https://github.com/Clozure/ccl/releases/download/v1.12/ccl-1.12-linuxx86.tar.gz ccl.tar.gz
RUN tar xvzf ccl.tar.gz
RUN apt-get update && apt-get install -y openjdk-11-jdk-headless gcc make
WORKDIR /app

ENTRYPOINT ["/opt/software/ccl/lx86cl64", "-l", "launch.lisp", "--"]
CMD ["--object-store", "/data/", "--start-slynk", "--slynk-port", "4005", "--slynk-loopback-interface", "0.0.0.0"]
