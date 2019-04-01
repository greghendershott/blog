#lang racket/base

(require racket/require
         (multi-in racket (contract file format list match path string))
         "post.rkt")

(provide tag-file->sorted-posts)

(define (tag-file->sorted-posts tag-file)
  (define rktds (remove-duplicates ;sucessive makes might append again
                 (file->lines tag-file)))
  (define rktds+posts (for/list ([rktd (in-list rktds)])
                        (cons rktd (call-with-input-file* rktd read))))
  (sort rktds+posts
        #:key (compose post-datetime cdr)
        string>?))
