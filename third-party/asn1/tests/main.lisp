(defpackage #:asn1/tests/main
  (:use #:cl
        #:rove
        #:asn1
        #:cl-base64))
(in-package #:asn1/tests/main)

(defvar *rsa-pub*
  "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAum9xmq7qBsjYU3gNFB6z
2DyQypeGvwR3MqbA5x4sevYjeqRunFRq+oo6CyEjzC/zR8xh7NvLFwXImSmyYadU
d+jstH1Kn5MJtBfCwlGSAXRfn6QV8wr+oweWvyDNUgCkgM+6X7Q7wyH8pib9J2WA
R6QcY3GRD+P+c/ZNwlgDSBVWzSUE2Sw1GBXadgEDdTMq/DnGmGmsMIdgCMxJ+szA
Av+dWJhuUPlp5zoFhyxayyJMCAND3llFpmv85bIKfQb8EDkQjtFLOEbU0KIY4pPj
KL01P4pDiqFFo6PWOJUHO5vyeLDWWCl1itOKeGxHvyxNQG/0BvQquxpjNjHZYCk0
cwIDAQAB")
(defvar *rsa-priv*
  "MIIEogIBAAKCAQEAum9xmq7qBsjYU3gNFB6z2DyQypeGvwR3MqbA5x4sevYjeqRu
nFRq+oo6CyEjzC/zR8xh7NvLFwXImSmyYadUd+jstH1Kn5MJtBfCwlGSAXRfn6QV
8wr+oweWvyDNUgCkgM+6X7Q7wyH8pib9J2WAR6QcY3GRD+P+c/ZNwlgDSBVWzSUE
2Sw1GBXadgEDdTMq/DnGmGmsMIdgCMxJ+szAAv+dWJhuUPlp5zoFhyxayyJMCAND
3llFpmv85bIKfQb8EDkQjtFLOEbU0KIY4pPjKL01P4pDiqFFo6PWOJUHO5vyeLDW
WCl1itOKeGxHvyxNQG/0BvQquxpjNjHZYCk0cwIDAQABAoIBABkE+a7ziE6Ox5E0
DDVGBYagYiH+AcRCuihe/oZFo1yBCbPcu0dZgN3MjQuPT/mH+dMJ155sxK17Rjdf
xCOczBYneRSjt88AcY3snmNrhPeTAX4wDA4IzLFeRFmz8jnuAiWTOwS68EY4mmpF
0zVlRrjWikTCKeCDDVPMmxTYsOAMWl03csr4tyJu9t83fCnDoIc5tH1WwiB+nnSl
2wOz3vpDf/CL7ZPqBcW2lINE0OD7sb5mlhBfU66Fli9fFlfO+a0YUfPCBqY8gOso
fjqOvZcqdLlj0f66Xj+86uWlHp9hLGgJ6zgIIsca+yPVrOKlP+sYwDs65WQyhkkd
Hvzp+uECgYEA5PVso7D06+cMSTG4QB8tQFBo2b8VXjepV3BLuGukmbw7z2YQQxTV
0NH2hTTvQ9CY6yy6uhwAwfAP35mRMEzlgzMHhhAc29gvFhWpGAJBz0sC/+O9/v8m
5VREiLyT76Xk3kz99GbWwNwFIdXkyelSfvxiBO7vsDSPw2zwYyNpj1ECgYEA0HRS
4a7whcn3C9U6mxK7mMvT2B+HfbN0FXuIADWgKziEdfRIPsy6NYOg/OBcWMvjy0VI
d2TBAje8D6WvG9QN/DHeAFHSX/DW8oW5IWNGolIY/EiKw8lhBjgrNgHl5eJGD5DH
ipe4U0+4Imdq26OqZwL61HYH5SwqUpqONxorfoMCgYBv+QL3jwxI7ocYqMM2QMkN
ogWVMBlQKaKcy6OMfsBSGzeY945OcDsdVAHfJYM6RCL1KLvtVtKcBj6NGPpjh8fb
ATLVwr2KWtC0WUWII1px+XpvEL8TnU81ap/Vy3wCALzMZxTv2Pd//FpaMNQiVwRs
bBu30+7O2vXQGk/5/BCc8QKBgBAEoEnPU5Q0TNOP8wzvh5LaNtEouxShsY3lDDJX
7JLlqOgXeWW5/aUXFEvaQb5hDIQWMtdZ2qr89WqOZMJSrTBv9Is5vly4+Qtx0yQJ
qOfYPytDt8YLt3Tu5AMmajAcDx4rFepEdlmQiqm6IK/4B6QayoOA/mJR3n6yebMq
Q6VZAoGALlPZ7+FNkAFnXK0m9phXPfQm2H6HyEf8X1gbhimkfh5R/v3BD92CQ35N
RQ7o1mhljwtlH6qDNqfHtuh30m/Hm5mokK/9knUnTza3wmNq6ZA+K1o2LRnlGb+u
u2FIFS1oyxICmSOLsS0x+SRPSJA5vTgxPoM4TYVUESWWl4TNMWg=
")

(deftest decode-encode-tests
  (testing "Public key"
    (let ((pub (base64:base64-string-to-usb8-array *rsa-pub*)))
      (ok (equalp (asn1:encode (asn1:decode pub)) pub))))
  (testing "Private key"
    (let ((priv (base64:base64-string-to-usb8-array *rsa-priv*)))
      (ok (equalp (asn1:decode (asn1:encode (asn1:decode priv)))
                  (asn1:decode priv))))))
