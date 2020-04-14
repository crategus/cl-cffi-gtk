(in-package :gio)

(define-g-flags "GTlsCertificateFlags" g-tls-certificate-flags
  (:export t
   :type-initializer "g_tls_certificate_flags_get_type")
  (:unknown-ca 1)
  (:bad-identity 2)
  (:not-activated 4)
  (:expired 8)
  (:revoked 16)
  (:insecure 32)
  (:generic-error 64)
  (:validate-all 128))
