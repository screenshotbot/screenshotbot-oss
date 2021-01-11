# pem

PEM parser.

## Usage

### Parsing a PEM file

```common-lisp
(ql:quickload :pem)

(pem:parse-file #P"rsa-pub.pem")
;=> (("PUBLIC KEY"
;     . "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAum9xmq7qBsjYU3gNFB6z
;   2DyQypeGvwR3MqbA5x4sevYjeqRunFRq+oo6CyEjzC/zR8xh7NvLFwXImSmyYadU
;   d+jstH1Kn5MJtBfCwlGSAXRfn6QV8wr+oweWvyDNUgCkgM+6X7Q7wyH8pib9J2WA
;   R6QcY3GRD+P+c/ZNwlgDSBVWzSUE2Sw1GBXadgEDdTMq/DnGmGmsMIdgCMxJ+szA
;   Av+dWJhuUPlp5zoFhyxayyJMCAND3llFpmv85bIKfQb8EDkQjtFLOEbU0KIY4pPj
;   KL01P4pDiqFFo6PWOJUHO5vyeLDWWCl1itOKeGxHvyxNQG/0BvQquxpjNjHZYCk0
;   cwIDAQAB"))
```

### Reading an RSA public/private keys

```common-lisp
(pem:read-from-file #P"rsa-pub.pem")
;=> #<IRONCLAD::RSA-PUBLIC-KEY {1004FD26B3}>

(pem:read-from-file #P"rsa-priv.pem")
;=> #<IRONCLAD::RSA-PRIVATE-KEY {10050CDB03}>
```
