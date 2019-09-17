#lang racket/base
;;;
;;; Server
;;;

; This file configures a web-server.
; Starts the web-server.
; Opens the start page in a browser.

(require web-server/dispatch web-server/servlet-env
          (prefix-in dispatch-files: web-server/dispatchers/dispatch-files)
         racket/runtime-path
         "control.rkt")


; http://localhost:8000/favicons/favicon-32x32.png

(define-runtime-path favicons "favicons/")

(displayln favicons)


(define (start)
  (serve/servlet dispatch
                 #:servlet-path      ""    ; initial to show in browser
                 #:servlet-regexp    #rx""
                 #:extra-files-paths (list favicons)))


(start)


; Note: If you want to use the repl and have the web-server running in the
;       background, you can start the server in a new thread:

; (thread (λ () (start)))
; http://localhost:8000/favicons/favicon-32x32.png
