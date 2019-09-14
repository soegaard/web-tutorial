#lang racket/base
;;;
;;; Authentication
;;;

(provide derive-key
         random-salt)

; Principles:
;   1. Don't store passwords in plain text
;      (It's a problem if an attacker gets access to the database)
;      Store a key derived from the password.

;   2. Don't store a simple hash of the password.
;      (An attacker might have rainbow tables)
;      Conclusion: Use a salt (random string, non-secret)
;      string together with the passwords to derive a key.

;   3. Use a standard KDF (Key Derivation function)
;      Conclusion: Use the `crypto` package.

; Note:
;     Salts aren't secret. Their only purpose is to make
;     a rainbow table attack impossible.


(require crypto crypto/all
         "def.rkt")

;;;
;;; KEY DERIVATION
;;;

; This chooses the PBKDF2 function from PKCS#5

(define digest-spec 'sha1)
(define kdf-spec    (list 'pbkdf2 'hmac digest-spec))


; Get an implementation of a key derivation function from the spec.
(use-all-factories!)
(def kdf-impl (get-kdf kdf-spec))

(unless kdf-impl
  (error 'authentication.rkt
         "could not get an implementation of the key derivation function"))

; To derive a key, use:
;     (kdf k pass salt [params])
; where k is the kdf-imp.

(define params (list (list 'iterations 65536) ; see docs for crypto
                     (list 'key-size   64)))

(define (derive-key password salt)
  (kdf kdf-impl password salt params))

; (time (derive-key #"foo" #"123"))

;;;
;;; SALT
;;;

(define (random-salt)
  (crypto-random-bytes 8))
