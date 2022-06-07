FROM ubuntu/squid

RUN echo "staging" > /etc/squid/allowed_sites
RUN echo "replay" >> /etc/squid/allowed_sites

# static IP for staging
RUN echo 172.29.1.15 >> /etc/squid/allowed_sites

# second static IP for staging. It's unclear which network is used to
# connect from staging to the squid proxy since both are two networks.
RUN echo 172.20.0.6 >> /etc/squid/allowed_sites

# static IP for replay
RUN echo 172.29.1.14 >> /etc/squid/allowed_sites

# production over OpenVPN
RUN echo 10.9.8.1 >> /etc/squid/allowed_sites

RUN echo 'http_port 3128' > /etc/squid/squid.conf
RUN echo 'cache allow all' >> /etc/squid/squid.conf
RUN echo 'cache_dir ufs /var/spool/squid 5000000 16 256' >> /etc/squid/squid.conf

RUN echo 'acl whitelist dstdomain "/etc/squid/allowed_sites"' >> /etc/squid/squid.conf
RUN echo 'http_access deny !whitelist' >> /etc/squid/squid.conf
