#lang racket/base
(provide (all-defined-out))

;;;
;;; Parameters
;;;

; The server stores the current request in current-request.
(define current-request (make-parameter #f))


; The control checks login-status and sets the parameter.
; The view needs this to display the login status.
(define current-login-status (make-parameter #f))

