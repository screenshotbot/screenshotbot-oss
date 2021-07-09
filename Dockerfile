FROM debian:stable
RUN apt-get update && apt-get install -y imagemagick html2text wget openjdk-11-jdk
RUN mkdir -p /opt/software
WORKDIR /opt/software
RUN wget https://github.com/Clozure/ccl/releases/download/v1.12/ccl-1.12-linuxx86.tar.gz
RUN tar xvzf ccl-*.tar.gz
RUN apt-get install -y gcc g++
WORKDIR /app
COPY . .
RUN if ! [ -e launch.lisp ] ; then cp src/screenshotbot/oss/launch.lisp ./ ; fi
RUN cd /app && /opt/software/ccl/lx86cl64 -l launch.lisp -- compile
CMD ["/opt/software/ccl/lx86cl64", "-l", "launch.lisp"]
