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
         "def.rkt" "parameters.rkt"
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

(define (get-login-status req)
  (request-id-cookie req
                     #:name "login-status"
                     #:key  cookie-salt
                     ; #:timeout ...
                     ; #:shelf-life ...
                     ))
                     

;;;
;;; DISPATCH
;;;

; this web-site uses "action" to dispatch on
(define (dispatch-on-action req)
  (current-request req)
  (parameterize ([current-login-status (get-login-status req)])
    (match (get-binding #"action")
      [#"updown"     (do-updown)]        ; a voting arrow was clicked
      [#"submitnew"  (do-submit-new)]    ; the "submit new" link on the front page was clicked
      [#"submit"     (do-submit)]        ; new entry sent from the new entry page
      [#"about"      (do-about)]         ; show the about page
      [_             (do-front-page)]))) ; show front page

;;;
;;; ACTIONS
;;;

(define (do-about)
  (def result (html-about-page))
  (response/output (λ (out) (display result out))))

(define (do-front-page)
  (def result (html-front-page 0 1 (page 0)))
  (response/output (λ (out) (display result out))))


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
