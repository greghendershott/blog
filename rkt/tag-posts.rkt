#lang racket/base

;; Copyright 2019 by Greg Hendershott.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; 	http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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
