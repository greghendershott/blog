#lang racket/base

(require racket/contract
         racket/format
         net/uri-codec)

(provide author
         scheme
         host
         full-uri
         urn)

(define (author)   "Greg Hendershott")
(define (scheme)   "https")
(define (host)     "www.greghendershott.com")
(define (host/urn) "urn:www-greghendershott-com")

(define/contract (full-uri uri-path)
  (-> (and/c path-string? relative-path?) string?)
  (~a (scheme) "://" (host) "/" uri-path))

(define (urn uri-path)
  ;; Note that URNs have a more restricted syntax than URIs.
  (~a (host/urn) ":" (regexp-replace* #px"/" uri-path "-")))
