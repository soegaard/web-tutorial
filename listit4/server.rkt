#lang racket/base
;;;
;;; Server
;;;

; This file configures a web-server.
; Starts the web-server.
; Opens the start page in a browser.

(require web-server/dispatch web-server/servlet-env
         (prefix-in dispatch-files: web-server/dispatchers/dispatch-files)
         racket/runtime-path)

;;;
;;; The Racket Stories App
;;;

; The Racket Stories (rs) app could be one of many apps
; handled by this server. Therefore give it a prefix.

(require (prefix-in rs: "app-racket-stories/control.rkt"))


;;;
;;; Serving Files
;;;

; The folder structure is:
;     files-root/favicons     
;     files-root/static

; For the url
;    http://localhost:8000/favicons/favicon-32x32.png
; the dispatcher sees:
;    /favicons/favicon-32x32.png

; Note: If you Apache or Nginx in from of the Racket web-server,
;       then make them serve the files in files-root/
;       If you want the Racket web-server to serve the files in files-root,
;       then use `#:extra-files-paths (list files-root)) `

(define-runtime-path files-root "files-root")

;;;
;;; Server Start
;;;

; While developing it is convenient to launch a browser, when the
; server is started - but not so when the server is deployed.

; When no port is mentioned, port 8000 is used.

(define (start #:launch-browser [launch-browser? #f])
  (serve/servlet rs:dispatch
                 #:launch-browser? launch-browser?
                 #:servlet-path      ""    ; initial to show in browser
                 #:servlet-regexp    #rx""
                 #:extra-files-paths (list files-root)))

(define (start/browser)
  (start #:launch-browser? #t))

(start)

; Note: If you want to use the repl and have the web-server running in the
;       background, you can start the server in a new thread:

; (thread (λ () (start)))
; http://localhost:8000/favicons/favicon-32x32.png
