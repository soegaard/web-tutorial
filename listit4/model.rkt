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
 (except-out (schema-out entry) make-entry)
 create-entry
 ~entry            ; format entry as a string
 insert-entry      ; insert entry into database 
 increase-score    ; increase score of entry
 decrease-score    ; decrease score of entry
 top               ; top entries (highest scores)
 page              ; page of entries
 url-in-db?        ; is url already in database?
 PAGE-LIMIT

 ;; User
 (except-out (schema-out user) make-user)
 create-user
 get-user
 authenticate-user

 ;; Votes
 insert-vote
 create-vote
 has-user-voted-on-entry?)


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

(define PAGE-LIMIT (make-parameter 3))   ; number of entries on each page


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
  ([id             id/f       #:primary-key #:auto-increment]
   [title          string/f   #:contract non-empty-string?]
   [url            string/f   #:contract non-empty-string?]
   [score          integer/f]
   [created        datetime/f]
   [submitter      id/f]       ; user id
   [submitter-name string/f]   ; redundant, but saves database access
   ))

(define (create-entry #:title title #:url url #:score score
                      #:created   [created (now)]
                      #:submitter submitter
                      #:submitter-name submitter-name)
  (make-entry #:title     title
              #:url       url
              #:score     score
              #:created   created
              #:submitter submitter
              #:submitter-name submitter-name))



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
  (defm (entry _ id title url score created submitter submitter-name) e)
  (~a id ":'" title "':'" url "':" score ":" submitter-name))


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
  ([id              id/f        #:primary-key #:auto-increment]
   [username        string/f    #:unique #:contract non-empty-string?]
   [key             string/f] ; key derived from password and salt 
   [email           string/f]
   [email-validated boolean/f]
   [created         datetime/f]
   [about           string/f]   ; text written by the user
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
  (displayln (list 'get-user user username))
  (match user
    [(? user? u)     u]
    [(? integer? id) (get-user/id id)]
    [_ (match username
         [(? string? un) (get-user/username un)]
         [#f (if (string? user)
                 (get-user/username user)
                 (error 'get-user (~a "neither user(id) or username provided, got: " user)))])]))

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
                    #:created         (now)
                    #:about           ""
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
    [u  (if (verify-password password (user-key u))
            #t
            (authentication-error "wrong password"))]))


;;;
;;; VOTES
;;;

; In order to prevent multiple votes for the same entry from the same user,
; we need store past votes. When we get to Hacker News size we will
; split entries into "active" and "archived" (say older than a month) 
; entries and only keep votes for active entries.
; Now we just store all votes.

(define-schema vote
  ([id              id/f        #:primary-key #:auto-increment]
   [user-id         id/f]
   [entry-id        id/f]))

; Note: We don't store whether it is an up or down vote.
;       Don't change your mind. Also ... I intend to remove down votes.

(define (has-user-voted-on-entry? user entry) ; structs or ids
  (def u (if (user?  user)  (user-id  user)  user))
  (def e (if (entry? entry) (entry-id entry) entry))
  (not (zero? (lookup db (~> (from vote #:as v)
                             (where (and (= v.user-id  ,u)
                                         (= v.entry-id ,e)))
                             (select (count *)))))))

(define (insert-vote vote)
  (insert! db vote))

(define (create-vote user-id entry-id)
  (make-vote #:user-id user-id #:entry-id entry-id))

(define (list-votes)
  (for/list ([v (in-entities db (from vote #:as v))])
    v))

(define (count-votes)
  (lookup db (~> (from vote #:as v)
                 (select (count *)))))



;;;
;;; DATABASE CREATION
;;; 

(define (populate-database)
  (when (= (count-users) 0)
    (create-user "foo" #"foo" "foo@foo.com"))
  (when (= (count-entries) 0)
    (define (create title url score)
      (create-entry #:title title #:url url #:score score #:submitter 1 #:submitter-name "foo"))
    (insert-entry (create "Racket News" "https://racket-news.com" 42))
    (insert-entry (create "Racket News - Issue 1" "https://racket-news.com/2019/02/racket-news-issue-1.html" 11))
    (insert-entry (create "Racket News - Issue 2" "https://racket-news.com/2019/02/racket-news-issue-2.html" 12))
    (insert-entry (create "Racket Blog" "https://blog.racket-lang.org" 32))
    (insert-entry (create "Blog - Alexis King" "https://lexi-lambda.github.io/index.html" 23))
    (insert-entry (create "Blog - Greg Hendershott" "https://www.greghendershott.com/" 14))
    (insert-entry (create "Blogs that use Frog" "http://stevenrosenberg.net/racket/2018/03/blogs-that-use-frog.html" 12))))

(define schemas '(entry user vote))

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









