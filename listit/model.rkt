#lang racket
;;;
;;; ListIt
;;;

;; The example application will be a mini version of Reddit called ListIt.
;; The front page consists of a list of links to interesting articles.
;; Users can vote the articles up and down, and submit new articles.
;; The hope is that the example is small enough to be easily understood,
;; but on the other hand large enough to illustrate as many aspects as possible.


;;;
;;; MODEL
;;;

;; This file contains the underlying model - i.e. the database.
;; 

(provide
 (schema-out entry)

 insert-entry      ; insert entry into database
 increase-score    ; increase score of entry
 decrease-score    ; decrease score of entry
 top               ; top entries (highest scores)
 page              ; page of entries
 url-in-db?        ; is url already in database?
 PAGE-LIMIT
 )


;;;
;;; Dependencies
;;;

(require db deta threading
         "def.rkt")

;;;
;;; CONFIGURATION
;;;

(define PAGE-LIMIT (make-parameter 50))   ; number of entries on each page


;;;
;;; Database Connection
;;;

; To avoid any database setup, we use a simple sqlite database.
; The database is stored in a file "list-sqlite.db". If the
; database is empty, we will populate (see populate-database
; at the end of this file).

(define db
  (sqlite3-connect #:database "listit-sqlite.db"
                   #:mode     'create))

; If you have a PostgreSQL database running, then
; you can use that too.
                   
#;(define db
    (postgresql-connect #:database "listit"
                        #:user     "listit"
                        #:password "listit"))

; Note: Don't put the password in the code.
;       Store it in an environment variable (set it with setenv in a terminal),
;       then use  getenv  to retrieve here.

;;;
;;; Model
;;;

;; Each entry in our database consists of a title to display,
;; an url to the article and a score representing the votes.

(define-schema entry
  ([id    id/f       #:primary-key #:auto-increment]
   [title string/f   #:contract non-empty-string?]
   [url   string/f   #:contract non-empty-string?]
   [score integer/f]))


;; Since we expect many entries in our database, we will think of them 
;; as divided into pages. The number of entries in each page is given by 
;; the parameter PAGE-LIMIT.


;; DATABASE INSERTION AND UPDATES

; get-entry : entry-or-entry-id -> entry
(define (get-entry entry)
  (match entry
    [(? entry?   entry) entry]
    [(? integer? id)    (lookup db (~> (from entry #:as e) (where (= id ,id))))]
    [_ (error 'get-entry (~a "entry or entry id expected, got: " entry))]))


(define (insert-entry entry)
  (insert! db entry))

(define (increase-score entry)
  (def e (get-entry entry))  
  (update! db (update-entry-score e (λ (score) (+ score 1)))))

(define (decrease-score entry)
  (def e (get-entry entry))
  (update! db (update-entry-score e (λ (score) (- score 1)))))

;;; DATABASE RETRIEVAL

(define (top n)
  (for/list ([e (in-entities db
                  (~> (from entry #:as e)
                      (order-by ([e.score #:desc]))
                      (limit ,n)))])
    e))

(define (page n)
  (for/list ([e (in-entities db
                   (~> (from entry #:as e)
                       (order-by ([e.score #:desc]))
                       (limit  ,(PAGE-LIMIT))
                       (offset ,(* (PAGE-LIMIT) n))))])
    e))

(define (entries-with-url url-str)
  (for/list ([e (in-entities db
                  (~> (from entry #:as e)
                      (where (= url ,url-str))))])
    e))

(define (sequence-first s)
  (sequence-ref s 0))

(define (count-entries-with-url url-str)
  (sequence-first
   (in-entities db
     (~> (from entry #:as e)
         (where (= url ,url-str))
         (select (count *))))))

(define (url-in-db? url-str)
  (positive? (count-entries-with-url url-str)))

(define (count-entries)
  (lookup db
          (~> (from entry #:as e)
              (select (count *)))))

(define (populate-database)
  (when (= (count-entries) 0)
    (insert-entry (make-entry #:title "Racket News"
                              #:url   "https://racket-news.com"
                              #:score 42))
    #;(insert-entry (make-entry #:title "Racket News"
                              #:url   "https://racket-news.com"
                              #:score 43))
    #;(insert-entry (make-entry #:title "Racket News"
                                #:url   "https://racket-news.com"
                                #:score 44))
    (insert-entry (make-entry #:title "Racket News - Issue 1"
                              #:url   "https://racket-news.com/2019/02/racket-news-issue-1.html"
                              #:score 11))
    (insert-entry (make-entry #:title "Racket News - Issue 2"
                              #:url   "https://racket-news.com/2019/02/racket-news-issue-2.html"
                              #:score 12))
    (insert-entry (make-entry #:title "Racket Blog"
                              #:url   "https://blog.racket-lang.org"
                              #:score 32))
    (insert-entry (make-entry #:title "Blog - Alexis King"
                              #:url   "https://lexi-lambda.github.io/index.html"
                              #:score 23))
    (insert-entry (make-entry #:title "Blog - Greg Hendershott"
                              #:url   "https://www.greghendershott.com/"
                              #:score 14))
    (insert-entry (make-entry #:title "Blogs that use Frog"
                              #:url   "http://stevenrosenberg.net/racket/2018/03/blogs-that-use-frog.html"
                              #:score 12))))

;;;
;;; DATABASE CREATION
;;; 


(define (create-database)
  ; create database will delete an existing table,
  (with-handlers ([exn? void])
    (drop-table!   db 'entry))  
  (create-table! db 'entry))

;; Create and populate tables if empty

(with-handlers ([exn? (λ (e) (create-database))])
  ; This query fails, if there is no table yet,
  ; the exception handler then creates the database
  (count-entries))

(when (= (count-entries) 0)
  (populate-database))

;;;
;;; Things to try in the repl
;;;

; (count-entries)
; (top 3)
; (PAGE-LIMIT 4)
; (page 0)
; (page 1)









