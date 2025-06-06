FROM debian:trixie AS magick_base


RUN apt-get update && apt-get install -y libyaml-dev git-core libpng-dev zlib1g-dev libpng16-16  zlib1g gcc makeself exiftool build-essential logrotate  sbcl pkg-config imagemagick libmagickwand-dev

RUN apt-get update && apt-get install -y openjdk-21-jdk-headless

FROM magick_base AS builder

# Update copybara config if you change this line

WORKDIR /app

ENTRYPOINT ["sbcl", "--script", "launch.lisp"]
CMD ["--object-store", "/data/", "--start-slynk", "--slynk-port", "4005", "--slynk-loopback-interface", "0.0.0.0"]
