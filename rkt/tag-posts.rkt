#lang racket/base

(require racket/require
         (multi-in racket (contract file format list match path string))
         "post.rkt")

(provide tag-file->sorted-posts)

(define (tag-file->sorted-posts tag-file)
  (define the-posts (for/list ([rktd (in-list (file->lines tag-file))])
                      (cons rktd (call-with-input-file* rktd read))))
  ;; Sucessive makes might append again, so remove dupes. (Using
  ;; `remove-duplicates` on a sorted list is kind of stupid; it could
  ;; be faster just scanning for "runs" of dupes. But whatever, my use
  ;; case is dozens or hundreds of items, max.)
  (remove-duplicates
   (sort the-posts
         #:key (compose post-datetime cdr)
         string>?)))
