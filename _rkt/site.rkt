#lang racket/base

(require racket/format)

(provide scheme
         host
         host/urn
         full-uri
         author)

(define (scheme)
  "https")

(define (host)
  "www.greghendershott.com")

(define (host/urn)
  "www-greghendershott-com")

(define (full-uri uri-path)
  (~a (scheme) "://" (host) "/" uri-path))

(define (author)
  "Greg Hendershott")
