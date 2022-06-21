FROM debian:stable AS magick_base


RUN apt-get update && apt-get install -y libyaml-dev git-core libpng-dev zlib1g-dev libpng16-16  zlib1g g++ libtool     libpng16-16 libpng-dev libjpeg62-turbo libjpeg62-turbo-dev libgomp1 ghostscript libxml2-dev libxml2-utils libtiff-dev libfontconfig1-dev libfreetype6-dev fonts-dejavu libwebp6 libwebp-dev makeself exiftool build-essential

ADD https://imagemagick.org/archive/ImageMagick.tar.gz ImageMagick.tar.gz
RUN tar xvzf ImageMagick.tar.gz

WORKDIR ImageMagick-7.1.0-28
RUN ls
RUN ./configure --with-quantum-depth=8 --with-png=yes --without-magick-plus-plus --disable-hdri --with-webp=yes
RUN make -j 8
RUN make install
RUN ldconfig /usr/local/lib

FROM magick_base AS builder

# Update copybara config if you change this line

RUN mkdir -p /opt/software
WORKDIR /opt/software
ADD https://github.com/Clozure/ccl/releases/download/v1.12/ccl-1.12-linuxx86.tar.gz ccl.tar.gz
RUN tar xvzf ccl.tar.gz
RUN apt-get update && apt-get install -y html2text openjdk-11-jdk-headless gcc make
WORKDIR /app

CMD /opt/software/ccl/lx86cl64 -l launch.lisp -- --object-store /data/ --start-slynk --slynk-port 4005 --slynk-loopback-interface 0.0.0.0
