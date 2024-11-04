#!/bin/sh

apt-get update
apt-get update && apt-get install -y libyaml-dev git-core libpng-dev zlib1g-dev libpng16-16  zlib1g gcc makeself exiftool build-essential logrotate imagemagick libmagickwand-dev sbcl

apt-get install -y openjdk-17-jre-headless

adduser --disabled-password --gecos "" screenshotbot

cd /home/screenshotbot

SB="sudo -u screenshotbot"

$SB git clone https://github.com/screenshotbot/screenshotbot-oss 

cat >/etc/systemd/system/screenshotbot.service <<EOF

[Unit]
Description=Screenshotbot
After=network.target
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=5
User=screenshotbot
NotifyAccess=all
WorkingDirectory=/home/screenshotbot/screenshotbot-oss
ExecStart=/usr/bin/env sbcl --script launch.lisp

[Install]
WantedBy=multi-user.target


EOF

systemctl daemon-reload
systemctl enable screenshotbot
systemctl start screenshotbot
