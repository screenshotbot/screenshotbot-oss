(defsystem "asn1"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "ASN.1 encoder/decoder"
  :depends-on ("asn1/main")
  :in-order-to ((test-op (test-op "asn1/tests"))))

(defsystem "asn1/tests"
  :class :package-inferred-system
  :depends-on ("asn1/tests/main")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))

(register-system-packages "optima" '(#:optima.core))
