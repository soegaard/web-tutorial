#lang racket/base

(provide (struct-out authentication-error)
         (struct-out validation))


(struct authentication-error (message) #:transparent)

(struct validation (value validated? invalid? feedback) #:transparent)
