#lang racket/base
;;;
;;; ListIt 2
;;;

; ListIt 1
;   - only entries (url, title and score) in the database
;   - no users

; ListIt 2
;   - users added

; Things to consider:
;  - creation of new user accounts
;  - log in / log out
;  - only authenticated users can vote


;;;
;;; MODEL
;;;

;; This file contains the underlying model - i.e. the database.
;; 

(provide
 ;; Entry
 (schema-out entry)
 ~entry            ; format entry as a string
 insert-entry      ; insert entry into database 
 increase-score    ; increase score of entry
 decrease-score    ; decrease score of entry
 top               ; top entries (highest scores)
 page              ; page of entries
 url-in-db?        ; is url already in database?
 PAGE-LIMIT

 ;; User
 (schema-out user)
 get-user
 authenticate-user
 create-user
 )


;;;
;;; Dependencies
;;;

(require racket/string racket/match racket/format racket/sequence
         openssl/sha1 db deta gregor threading
         "def.rkt" "exn.rkt" "structs.rkt"
         "authentication.rkt"
         "user-names.rkt")

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
;;; MODEL
;;;


;;;
;;; Entry
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

;;; FORMATTING

(define (~entry e)
  (set! e (get-entry e))
  (defm (entry _ id t u s) e)
  (~a id ":'" t "':'" u "':" s))


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

(define (count-entries-with-url url-str)
  (lookup db (~> (from entry #:as e)
                 (where (= url ,url-str))
                 (select (count *)))))

(define (url-in-db? url-str)
  (positive? (count-entries-with-url url-str)))

(define (count-entries)
  (lookup db (~> (from entry #:as e)
                 (select (count *)))))


;;;
;;; User
;;;

(define (digest-value? x) (and (integer? x) (<= 0 x 3)))

(define-schema user
  ([id              id/f      #:primary-key #:auto-increment]
   [username        string/f  #:unique #:contract non-empty-string?]
   [key             string/f] ; key derived from password and salt 
   [email           string/f]
   [email-validated boolean/f]
   [send-digest     integer/f   #:contract digest-value?]  
   [last-digest     datetime/f]))

(define <digest:no>     0)
(define <digest:hourly> 1)
(define <digest:daily>  2)
(define <digest:weekly> 3)

(define (insert-user user)
  (insert! db user))

(define (count-users)
  (lookup db (~> (from user #:as u)
                 (select (count *)))))

(define (get-user [user #f] #:username [username #f])
  (match user
    [(? user? u)     u]
    [(? integer? id) (get-user/id id)]
    [_ (match username
         [(? string? un) (get-user/username un)]
         [#f (error 'get-user (~a "neither user(id) or username provided, got: " user))])]))

(define (~user u)
  (set! u (get-user u))
  (defm (struct* user ([username un] [email e] [email-validated ev])) u)
  (~a un "(" e ")" (if ev " " "!")))

(define (list-users)
  (for/list ([e (in-entities db (from user #:as u))])
    e))

(define (get-user/id id)
  (lookup db (~> (from user #:as u) (where (= id ,id)))))

(define (get-user/username username)
  (lookup db (~> (from user #:as u)
                 (where (= username ,username)))))

(define (username-in-db? username)
  (positive? (lookup db (~> (from user #:as u)
                            (select (count *))))))


(define (create-user username password email)
  (unless (good-username? username)
    (error (exn:fail:user:bad (bad-username-reason username))))

  ; TODO create-user : validate email
  
  (def u (make-user #:username        (normalize-username username)
                    #:key             (derive-key password) ; also embeds salt
                    #:email           email
                    #:email-validated #f
                    #:send-digest     <digest:no>
                    #:last-digest     (now)))
  (with-handlers ([exn:fail:sql?
                   (λ (e)
                     (raise (exn:fail:user:bad
                             "username in use" (current-continuation-marks))))])
    ; Since the field "username" has the #:unique property,
    ; inserting an existing "username" will provoke an sql error.
    ; Note: We are not using `username-in-db?` first.
    ;       In theory two different users could register at almost the same time
    (insert-user u)))


(define (authenticate-user username password)
  (match (get-user/username username)
    [#f (authentication-error "username not in db")]
    [u  (if (verify-password (user-key u))
            #t
            (authentication-error "wrong password"))]))


(define (populate-database)
  (when (= (count-entries) 0)
    (insert-entry (make-entry #:title "Racket News"
                              #:url   "https://racket-news.com"
                              #:score 42))
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

(define schemas '(entry user))

(define (create-tables)
  ; Note: This creates the tables if they don't exist.
  ;       Nothing is done if they do exist.
  (for ([s schemas])
    (create-table! db s)))

(define (drop-tables)
  (for ([s schemas])    
    (with-handlers ([exn? void])
      (drop-table! db s))))

(create-tables)

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









