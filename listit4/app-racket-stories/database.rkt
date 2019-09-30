#lang racket/base
;;;
;;; DATABASE
;;;

(provide connect-to-database)

; For users who wants to try running racket-stories on their own machines,
; we use a simple sqlite database to avoid any database setup.
; The real racket-stories is deployed using a postgresql database.

; See "config.rkt" for configuration options.

(require racket/match racket/os db 
         "config.rkt" "parameters.rkt" "structs.rkt")


(define (connect-to-database)
  (match (or (current-deployment) (development))
    [(or (development) (testing)) (if (member (gethostname) '() #;'("mbp"))
                                      (connect-to-postgresql)
                                      (connect-to-sqlite))]
    [(or (staging) (production))  (connect-to-postgresql)]))

(define (connect-to-sqlite)
  (sqlite3-connect    #:database sqlite-db
                      #:mode     'create))


(define (connect-to-postgresql)
  (postgresql-connect #:database database-name
                      #:user     database-user
                      #:password database-password))

