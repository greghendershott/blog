#lang racket/base

(require racket/format
         net/uri-codec)

(provide scheme
         host
         full-uri
         urn
         author)

(define (scheme)
  "http")

(define (host)
  "www.greghendershott.com")

(define (host/urn)
  "urn:www-greghendershott-com")

(define (full-uri uri-path)
  (~a (scheme) "://" (host) "/" uri-path))

(define (urn uri-path)
  ;; Note that URNs have a more restricted syntax than URIs.
  (~a (host/urn) ":" (uri-encode uri-path)))

(define (author)
  "Greg Hendershott")
