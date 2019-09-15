#lang racket/base
;;;
;;; Server
;;;

; This file configures a web-server.
; Starts the web-server.
; Opens the start page in a browser.

(require web-server/dispatch web-server/servlet-env
         "control.rkt")


(define-values (listit-dispatch listit-url)
    (dispatch-rules
     [("") dispatch-on-action]
     [else dispatch-on-action]))


(define (start)
  (serve/servlet listit-dispatch
                 #:servlet-regexp #rx""))

(start)


; Note: If you want to use the repl and have the web-server running in the
;       background, you can start the server in a new thread:

; (thread (λ () (start)))


