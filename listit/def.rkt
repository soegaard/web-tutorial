#lang racket/base
(require (for-syntax racket/base)
         racket/match)

;;; shorter names used for internal definitions
(define-syntax defv (make-rename-transformer #'define-values))
(define-syntax defm (make-rename-transformer #'match-define))
(define-syntax def  (make-rename-transformer #'define))

(provide defv defm def)
