#lang racket/base
;;;
;;; Server
;;;

; Open this file and run it to start the racket-stories site on your computer.

; This file configures a web-server.
; Starts the web-server.
; Opens the start page in a browser (if we are developing).

(require web-server/dispatch web-server/servlet-env
         (prefix-in dispatch-files: web-server/dispatchers/dispatch-files)
         racket/match racket/os racket/runtime-path)

;;;
;;; The Racket Stories App
;;;

; The Racket Stories (rs) app could be one of many apps
; handled by this server. Therefore er give it a prefix `rs`.

(require (prefix-in rs: "app-racket-stories/control.rkt")
         (prefix-in rs: "app-racket-stories/structs.rkt")
         (prefix-in rs: "app-racket-stories/parameters.rkt"))

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


(define deployment
  (case (system-type 'os)
    [(macosx windows) (rs:development)]
    [(unix)           (cond
                        [(regexp-match #rx"web-rs" (gethostname))
                         (if (file-exists? "PRODUCTION")
                             (rs:production)
                             (rs:staging))]
                        [else
                         (rs:development)])]
    [else             (rs:development)]))

(define banner
  (match deployment
    [(rs:production)  #f] ; no banner
    [(rs:development) "Development"]
    [(rs:testing)     "Testing"]
    [(rs:staging)     "Staging"]
    [_                "huh?"]))
  
(parameterize ([rs:current-deployment     deployment]
               [rs:current-banner-message banner])
  (start #:launch-browser (rs:development? deployment)))

; Note: If you want to use the repl and have the web-server running in the
;       background, you can start the server in a new thread:

; (thread (λ () (start)))
