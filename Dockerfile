FROM debian:bookworm 


RUN apt-get update && apt-get install -y libyaml-dev git-core libpng-dev zlib1g-dev libpng16-16  zlib1g gcc makeself exiftool build-essential logrotate

RUN apt-get update && apt-get install -y sbcl pkg-config

ADD https://screenshotbot-assets.s3.us-east-1.amazonaws.com/imagemagick7_7.1.1-39_amd64.deb /root/imagemagick7_7.1.1-39_amd64.deb

# RUN apt-get update && apt-get install -y /root/imagemagick7_7.1.1-39_amd64.deb

RUN apt-get update && apt-get install -y imagemagick libmagickwand-dev
RUN apt-get update && apt-get install -y openjdk-17-jdk-headless


RUN which jar

# Update copybara config if you change this line

WORKDIR /app

CMD ["make", "test-sb"]
