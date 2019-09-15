#lang racket/base

;;;
;;; CONTROL
;;;

; The interface between the web-server in "server.rkt" and the control
; is the function `dispatch-on-action`.
; Each time the web-server receives an request, dispatch-on-action is called.
; In this simple web app we dispatch 

; when it receives a request. The control 

(provide dispatch-on-action)



(require racket/match
         racket/runtime-path
         web-server/servlet-env
         web-server/servlet
         web-server/http/redirect
         web-server/http/cookie
         web-server/http/id-cookie ; authenticated cookies
         ; "config.rkt"
         "def.rkt" "exn.rkt" "parameters.rkt" "structs.rkt"
         "model.rkt" "view.rkt")

;;;
;;; Utils
;;;

(define (bytes->number b)
  (string->number (bytes->string/utf-8 b)))

;;;
;;; Bindings
;;;

(define (get-binding binding-name [convert values] #:request [req (current-request)])
  (match (bindings-assq binding-name (request-bindings/raw req))
    [(? binding:form? b) (convert (binding:form-value b))]
    [_ #f]))


;;;
;;; Cookies
;;;

; We are going to store session information (such as login status)
; on the client in cookies. To make these tamper proof, we need
; a way to verify, that they haven't been changed by the user.

; In order to store `authored-seconds&data` we send
; `digest&authored-seconds&data` where digest is a
; cryptographics hash of authored-seconds&data and, very important,
; a secret salt only known to the server(s).
; If using multiple servers, they need to share the secret salt.

; In short: we are not encrypting the data, we are merely
; associating it with a digest, so it can't be altered.

(define-runtime-path cookie-salt.bin "cookie-salt.bin")
(def cookie-salt (make-secret-salt/file cookie-salt.bin))

(define (make-logged-in-cookie)
  (make-id-cookie "login-status" "in"
                  #:key        cookie-salt
                  ; only for http/https (not client side javascript)
                  #:http-only? #t           
                  ; #:expires ...
                  ; #:max-age ...                            
                  ; #:secure? #t  ; instructs client only to send cookie via https
                  ))

(define (make-logged-out-cookie)
  (make-id-cookie "login-status" "out"
                  #:key cookie-salt
                  #:http-only? #t))


(define (make-username-cookie username)
  (make-id-cookie "username" username
                  #:key        cookie-salt
                  #:http-only? #t))

(define (get-cookie-value req name)
  (request-id-cookie req #:name name #:key  cookie-salt
                     ; #:timeout ...
                     ; #:shelf-life ...
                     ))

(define (get-login-status req)
  (match (get-cookie-value req "login-status")
    ["in" #t]
    [_    #f]))
                     
;;;
;;; DISPATCH
;;;

; this web-site uses "action" to dispatch on
(define (dispatch-on-action req)
  (current-request req)
  (def login-status (get-login-status req))
  (def username     (and login-status (get-cookie-value req "username")))
  (def user         (and username (get-user #:username username)))
  (parameterize ([current-login-status (and user login-status)]
                 [current-user         (and login-status user)])
    (match (get-binding #"action")
      [#"updown"       (do-updown)]        ; a voting arrow was clicked
      [#"submitnew"    (do-submit-new)]    ; the "submit new" link on the front page was clicked
      [#"submit"       (do-submit)]        ; new entry sent from the new entry page
      [#"about"        (do-about)]         ; show the about page
      [#"login"        (do-login-page)]    ; show the login page
      [#"submit-login" (do-submit-login)]  ; check username and password
      [#"logout"       (do-logout)]        ; logout, then show front page
      [#"submit-create-account"            ; create new user
       (do-submit-create-account)]
      [_               (do-front-page)]))) ; show front page

;;;
;;; ACTIONS
;;;

(define (do-about)
  (def result (html-about-page))
  (response/output (λ (out) (display result out))))

(define (do-front-page)
  (def result (html-front-page 0 1 (page 0)))
  (response/output (λ (out) (display result out))))

(define (do-login-page)
  (def result (html-login-page))
  (response/output (λ (out) (display result out))))

(define (do-submit-login)
  (def u (bytes->string/utf-8 (get-binding #"username")))
  (def p (get-binding #"password"))
  (cond
    [(and u p) (match (authenticate-user u p)
                 ; On a successful login we generate a logged-in cookie,
                 ; and redirect to the frontpage.
                 ; The redirection prevents the form data being submitted
                 ; twice due to reloads in the browser.
                 [#t
                  (displayln (list 'do-submit-login "login ok"))
                  (redirect-to
                   "control" temporarily
                   #:headers (map cookie->header
                                  (list (make-username-cookie u)
                                        (make-logged-in-cookie))))]
                 ; If the login failed, the user must try again.
                 [(authentication-error msg)
                  (displayln (list 'do-submit-login msg))
                  (redirect-to "control?action=login" temporarily)])]
    [else      (displayln (list 'do-submit-login 'u u 'p p))
               (redirect-to "control?action=login" temporarily)]))

(define (do-submit-create-account)
  (def u (bytes->string/utf-8 (get-binding #"username")))
  (def p (get-binding #"password"))
  (def e (bytes->string/utf-8 (get-binding #"email")))
  (with-handlers ([exn:fail:user:bad?
                   (λ (e)
                     (def msg (exn-message e))
                     (displayln msg) ; todo: show user
                     (redirect-to "control?action=login" temporarily))])
    (create-user u p e)
    (redirect-to "control" temporarily
                 #:headers (map cookie->header
                                (list (make-username-cookie u)
                                      (make-logged-in-cookie))))))
    
    

(define (do-logout)
  (def result (html-login-page))
  (redirect-to "control" temporarily
               #:headers (map cookie->header
                              (list (make-logged-out-cookie)))))



(define (do-updown) ; an arrow was clicked
  (def entry_id  (get-binding #"entry_id" bytes->number))
  (def arrow (get-binding #"arrow"))
  (match arrow
    [#"down" (when entry_id
               (decrease-score entry_id))]
    [#"up"   (when entry_id
               (increase-score entry_id))]
    [else    'do-nothing])
  ; to make sure a reload doesn't resubmit, we redirect to the front page
  (redirect-to "control" temporarily))


(define (do-submit-new)
  (def result (html-submit-new-page))
  (response/output (λ (out) (display result out))))


(define (do-submit)
  (def url   (get-binding #"url"   bytes->string/utf-8))
  (def title (get-binding #"title" bytes->string/utf-8))
  (when (and url title)
    (insert-entry
     (make-entry #:title title #:url url #:score 10)))
  ; to make sure a reload doesn't resubmit, we redirect to the front page
  (redirect-to "control" temporarily))
