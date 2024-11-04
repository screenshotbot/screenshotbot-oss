#!/bin/sh

apt-get update
apt-get update && apt-get install -y libyaml-dev git-core libpng-dev zlib1g-dev libpng16-16  zlib1g gcc makeself exiftool build-essential logrotate imagemagick libmagickwand-dev sbcl nginx certbot python3-certbot-nginx

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

IP=`curl -4 ident.me`

echo "The IP address of this machine is: $IP"
echo "Use this to create a domain name that points to $IP"
printf  "Type the domain name here: "

read DOMAIN

echo "We'll use domain '$DOMAIN' to set up HTTPS via Let's Encrypt."

cat >/etc/nginx/sites-available/screenshotbot.conf <<EOF
upstream screenshotbot {
    server 127.0.0.1:4091;
}


server {
  server_name $DOMAIN;

  proxy_set_header Host \$host:\$server_port;
  proxy_set_header X-Real-IP \$remote_addr;
  proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;

  # When using behind a ALB, this is the easiest way to ensure that
  # proto is set correctly.
  proxy_set_header X-Forwarded-Proto \$scheme;
  proxy_next_upstream error timeout http_502 http_503;

  location /wsapp/ {
      proxy_pass http://screenshotbot;
      proxy_http_version 1.1;
      proxy_set_header Upgrade \$http_upgrade;
      proxy_set_header Connection "Upgrade";
      proxy_set_header Host \$host;
  }

  location / {
      proxy_pass http://screenshotbot;
      proxy_redirect http://\$proxy_host:\$proxy_port /;
      proxy_next_upstream error timeout http_502 http_503;
  }

  gzip on;
  gzip_vary on;
  gzip_proxied any;
  gzip_min_length 256;
  gzip_comp_level 6;
  gzip_buffers 16 8k;
  gzip_http_version 1.1;
  gzip_types text/plain text/html text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;


  listen 80;
  listen [::]:80;  
}
EOF

ln -s /etc/nginx/sites-available/screenshotbot.conf /etc/nginx/sites-enabled/screenshotbot.conf

systemctl restart nginx

echo
echo
echo "We'll now run Certbot to set up HTTPS. Please follow the prompts."
echo "If certbot fails, you don't have to re-run this script again. You can just run"
echo "   certbot run --domains $DOMAIN"
echo
echo

certbot run --domains $DOMAIN

echo
echo
echo "And we're done. You should soon be able to access https://$DOMAIN (it may take a few minutes for the the compilation to complete. You can use `journalctl -u screenshotbot -f` to track the progress.)"
