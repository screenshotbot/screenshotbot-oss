(defpackage :util/tests/test-ssl
  (:use #:cl
        #:fiveam)
  (:import-from #:util/ssl
                #:make-ssl-context)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-ssl)

(defvar *test-cert*
  "-----BEGIN CERTIFICATE-----
MIIF7zCCA9egAwIBAgIUA2MBbH5NagtI0Rf+UH/Nb4mJp3YwDQYJKoZIhvcNAQEL
BQAwgYYxCzAJBgNVBAYTAlhYMRIwEAYDVQQIDAlTdGF0ZU5hbWUxETAPBgNVBAcM
CENpdHlOYW1lMRQwEgYDVQQKDAtDb21wYW55TmFtZTEbMBkGA1UECwwSQ29tcGFu
eVNlY3Rpb25OYW1lMR0wGwYDVQQDDBRDb21tb25OYW1lT3JIb3N0bmFtZTAeFw0y
MzEyMjAyMjI1NThaFw0zMzEyMTcyMjI1NThaMIGGMQswCQYDVQQGEwJYWDESMBAG
A1UECAwJU3RhdGVOYW1lMREwDwYDVQQHDAhDaXR5TmFtZTEUMBIGA1UECgwLQ29t
cGFueU5hbWUxGzAZBgNVBAsMEkNvbXBhbnlTZWN0aW9uTmFtZTEdMBsGA1UEAwwU
Q29tbW9uTmFtZU9ySG9zdG5hbWUwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIK
AoICAQCxe6RSCLJnIxgEhT7Soh709AxY+1fyyWQjdE+D65Pu+RB9O3EZuXw3dr5s
TMPOT8r3rA/rQ4fMdC7d96p6124LruH0Smyy10udzbaPltqQeN8YIsCoTX/YdiwQ
/c4oTb+bZKAg6+5h6kDO3u8lpGAA9PF2yRzUnnNxoteKjE1RYiuKl4MR+C8VyidJ
l/oTsX01TLT/A7yP8ZMJ3tfYmR2aPcb2N8MESsWLtnqNzaEeWfYyu//nfHgE+fNe
NUjXqaWVjs3rG8L0Wxfnt1T6eCewk7Z3LkFNgja0/BcN929OhdipMq2RciDjOgPk
zI73eXJuZd9ngNketHzswQr0ppLdXdYCHKzboM6faWVfh8dYcdR/QF6tkPPXAmGU
FydJjFsyd0S6IcoTkhZVOF3M7+BsOyq7236KFe8BhZxEuhpMCkOCTyJqZCFveZ+i
LAW4fSmx6FN4vavroUSOxzOqpBnRx3b+HwmI1SWmV10yfY1Dql5SWteyVaaIjAmr
Zvd2QtFbHS03rKBz/WcTulWZfTFBTdvrZfIn2lUsHxxPe3dKuWqvobi3MJYCDHa+
rhAXPe6c7YE0vMAZgqv7TVoQD/I+U4I2H8z/is2B1/lAz1UOp7v8NdD0D/ZT/C+8
R+Nutc5u3RFLsuZcNc1FsdvOS7EQlBkEeZq0raTJCTFLCqKgVwIDAQABo1MwUTAd
BgNVHQ4EFgQUAxWa8BUAU0jwJNtyDRx9SvbW1mswHwYDVR0jBBgwFoAUAxWa8BUA
U0jwJNtyDRx9SvbW1mswDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOC
AgEADRsoRHmyr1/H8thS2Bfi2gtnhbe6IT0DS9tKuFVVoNG50/907qgX+hTRzV3G
4wf9XkB3PFS/tQVQwKzLcbioSnNdVNVozJs+yA8c/DAdtUSZ7hrrA/JOKjlGdSQH
pPE7zT2KAZQl6OJ8WxiQMAR/EAG7dGHgX5qfK+DBDfhudRkSAhCW5YTacVdY0ZpK
So7xrrf2Km8lSkZSXbZA+OehgyeTtAqOvP0Lm7AV10S5BcaAPylefBc/+nJAxbnC
Qh5qYB7x1g2o9AdqJ4M4UDub75mjw7rP7GHmpxyEYgwplV+4zUK6ruS+ryM6kDZH
HJdrj7HZq25koXnO+v2CsXlCDYAHtyupST23plrASG/HV44RBqzXz44JUyJVhAqe
sNMmXcuqEEFIdagL6ZuNziuSZ5QW2Kbvabca4TZ92mzaFkapEHYlAn8v4+r9CcMb
VnYMz0g+P6RemIocaacITMT3d5oPoXO4roM0p79Fr9zSvIumVaf5ZeHqaO6P59Ao
6041AD6QEFjkq09aoi2lLv6Y0eeL0o8ha853npA7aewAxUxn0dE64ceHBz5J71nt
X7F8H51KKqNN0Wm2CQVQIb+SGvy+7g0quDkl5lxlMqGw66dyuaQRzOnMnbiPo51B
j6i56GUaQDbtWA0JgaRVtcWRM+w2W4jjfecc/E3EnYlW0m8=
-----END CERTIFICATE-----")

(defvar *test-key*
  "-----BEGIN PRIVATE KEY-----
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
jFLQEGjR6Nr63A4PRTlAVsCZTxb5EMiS
-----END PRIVATE KEY-----")

(util/fiveam:def-suite)

(test make-ssl-context
  (finishes
   (make-ssl-context
    :certificate *test-cert*))
  (is
   (eql
    (make-ssl-context
     :certificate *test-cert*)
    (make-ssl-context
     :certificate *test-cert*))))

(def-fixture state ()
  (let ((port (util/random-port:random-port)))
   (uiop:with-temporary-file (:pathname key :stream s)
     (write-string *test-key* s)
     (finish-output s)
     (uiop:with-temporary-file (:pathname cert :stream s)
       (write-string *test-cert* s)
       (finish-output s)
       (&body)))))

(test happy-path-attaching-stream
  (with-fixture state ()
    (let ((server-context (comm:create-ssl-server-context
                           :key-file key
                           :cert-file cert))
          (client-context (make-ssl-context :certificate *test-cert*)))
     (let ((process (comm:start-up-server
                     :function (lambda (socket)
                                 (let ((stream
                                         (comm:create-ssl-socket-stream
                                          socket server-context)))
                                   (format stream "hello world~%")
                                   (finish-output stream)))
                     :service port)))
       (unwind-protect
            (let ((stream (comm:open-tcp-stream "localhost" port
                                                :ssl-ctx client-context)))
              (is (equal "hello world" (read-line stream))))
         (comm:server-terminate process))))))
