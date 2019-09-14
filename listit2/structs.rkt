#lang racket/base

(provide (struct-out authentication-error))

(struct authentication-error (message) #:transparent)
