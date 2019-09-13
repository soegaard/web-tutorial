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
         web-server/servlet-env  web-server/servlet
         web-server/http/redirect
         ; "config.rkt"
         "def.rkt"
         "model.rkt" "view.rkt")

(define current-request (make-parameter #f))

(define (get-binding binding-name [convert values] #:request [req (current-request)])
  (match (bindings-assq binding-name (request-bindings/raw req))
    [(? binding:form? b) (convert (binding:form-value b))]
    [_ #f]))

(define (bytes->number b)
  (string->number (bytes->string/utf-8 b)))

;;;
;;; DISPATCH
;;;

; this web-site uses "action" to dispatch on
(define (dispatch-on-action req)
  (current-request req)
  (match (get-binding #"action")
    [#"updown"     (do-updown)]       ; a voting arrow was clicked
    [#"submitnew"  (do-submit-new)]   ; the "submit new" link on the front page was clicked
    [#"submit"     (do-submit)]       ; new entry sent from the new entry page
    [#"about"      (do-about)]        ; show the about page
    [_             (do-front-page)])) ; show front page

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
