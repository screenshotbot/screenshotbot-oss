# asn1

ASN.1 encoder/decoder.

## Usage

### Decoding from a Base64 string

```common-lisp
(ql:quickload '(:asn1 :cl-base64))

(defvar *public-key*
  (base64:base64-string-to-usb8-array
   "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAum9xmq7qBsjYU3gNFB6z
2DyQypeGvwR3MqbA5x4sevYjeqRunFRq+oo6CyEjzC/zR8xh7NvLFwXImSmyYadU
d+jstH1Kn5MJtBfCwlGSAXRfn6QV8wr+oweWvyDNUgCkgM+6X7Q7wyH8pib9J2WA
R6QcY3GRD+P+c/ZNwlgDSBVWzSUE2Sw1GBXadgEDdTMq/DnGmGmsMIdgCMxJ+szA
Av+dWJhuUPlp5zoFhyxayyJMCAND3llFpmv85bIKfQb8EDkQjtFLOEbU0KIY4pPj
KL01P4pDiqFFo6PWOJUHO5vyeLDWWCl1itOKeGxHvyxNQG/0BvQquxpjNjHZYCk0
cwIDAQAB"))

(asn1:decode *public-key*)
;=> ((:SEQUENCE (:SEQUENCE (:OBJECT-IDENTIFIER . #(1 2 840 113549 1 1 1)) (:NULL))
;     (:BIT-STRING
;      . #(48 130 1 10 2 130 1 1 0 186 111 113 154 174 234 6 200 216 83 120 13 20
;          30 179 216 60 144 202 151 134 191 4 119 50 166 192 231 30 44 122 246 35
;          122 164 110 156 84 106 250 138 58 11 33 35 204 47 243 71 204 97 236 219
;          203 23 5 200 153 41 178 97 167 84 119 232 236 180 125 74 159 147 9 180
;          23 194 194 81 146 1 116 95 159 164 21 243 10 254 163 7 150 191 32 205 82
;          0 164 128 207 186 95 180 59 195 33 252 166 38 253 39 101 128 71 164 28
;          99 113 145 15 227 254 115 246 77 194 88 3 72 21 86 205 37 4 217 44 53 24
;          21 218 118 1 3 117 51 42 252 57 198 152 105 172 48 135 96 8 204 73 250
;          204 192 2 255 157 88 152 110 80 249 105 231 58 5 135 44 90 203 34 76 8 3
;          67 222 89 69 166 107 252 229 178 10 125 6 252 16 57 16 142 209 75 56 70
;          212 208 162 24 226 147 227 40 189 53 63 138 67 138 161 69 163 163 214 56
;          149 7 59 155 242 120 176 214 88 41 117 138 211 138 120 108 71 191 44 77
;          64 111 244 6 244 42 187 26 99 54 49 217 96 41 52 115 2 3 1 0 1))))
```

### Using with Optima & Ironclad

```common-lisp
(ql:quickload '(:optima :ironclad))

(optima:match (asn1:decode *public-key*)
  ((asn1:rsa-public-key-info n e)
   (ironclad:make-public-key :rsa :n n :e e)))
;=> #<IRONCLAD::RSA-PUBLIC-KEY {1004C2BA63}>
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2017 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
