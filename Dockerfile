
FROM debian:stable AS builder

# Update copybara config if you change this line
ENV PROJECT_ROOT=../../../

RUN mkdir -p /opt/software
WORKDIR /opt/software
ADD https://github.com/Clozure/ccl/releases/download/v1.12/ccl-1.12-linuxx86.tar.gz ccl.tar.gz
RUN tar xvzf ccl.tar.gz
RUN apt-get update && apt-get install -y imagemagick html2text openjdk-11-jdk-headless gcc
WORKDIR /app
COPY ${PROJECT_ROOT} .
RUN if ! [ -e launch.lisp ] ; then cp src/screenshotbot/oss/launch.lisp ./ ; fi
RUN /opt/software/ccl/lx86cl64 -l launch.lisp -- compile

FROM debian:stable
RUN mkdir -p /opt/software
WORKDIR /opt/software
ADD https://github.com/Clozure/ccl/releases/download/v1.12/ccl-1.12-linuxx86.tar.gz ccl.tar.gz
RUN tar xvzf ccl.tar.gz
RUN apt-get update && apt-get install -y imagemagick html2text openjdk-11-jre-headless
RUN mkdir -p /opt/software
COPY --from=builder /app /app
WORKDIR /app
CMD /opt/software/ccl/lx86cl64 -l launch.lisp -- --object-store /data/
