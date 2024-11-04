FROM debian:bookworm AS magick_base


RUN apt-get update && apt-get install -y libyaml-dev git-core libpng-dev zlib1g-dev libpng16-16  zlib1g gcc makeself exiftool build-essential logrotate imagemagick libmagickwand-dev sbcl pkg-config

ADD https://screenshotbot-assets.s3.us-east-1.amazonaws.com/imagemagick7_7.1.1-39_amd64.deb /root/imagemagick7_7.1.1-39_amd64.deb

RUN apt-get install -y /root/imagemagick7_7.1.1-39_amd64.deb

RUN apt-get update && apt-get install -y openjdk-17-jre-headless

FROM magick_base AS builder

# Update copybara config if you change this line

WORKDIR /app

ENTRYPOINT ["sbcl", "--script", "launch.lisp"]
CMD ["--object-store", "/data/", "--start-slynk", "--slynk-port", "4005", "--slynk-loopback-interface", "0.0.0.0"]
