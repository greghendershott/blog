#lang racket/base

(require racket/format)

(provide full-uri
         author)

(define (full-uri uri-path)
  (~a "https://www.greghendershott.com/" uri-path))

(define (author)
  "Greg Hendershott")
