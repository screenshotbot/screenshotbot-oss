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
