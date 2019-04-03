#lang at-exp racket/base

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
         (multi-in racket (contract file format list match path port string))
         threading
         "post.rkt"
         "page-xexpr.rkt"
         "util.rkt"
         "xexpr.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector rktd www output-html)
     (define the-post (call-with-input-file* rktd read))
     (make-parent-directory* output-html)
     (call-with-output-file*/delete
      #:exists 'replace output-html
      (λ (out)
        (display "<!DOCTYPE html>" out)
        (display (xexpr->string
                  (post-xexpr the-post (file->uri www output-html)))
                 out)))]))

(define (post-xexpr the-post page-path)
  (match-define (post title date tags blurb more? body) the-post)
  (page-xexpr #:title       title
              #:keywords    (string-join tags)
              #:description (xexprs->description blurb)
              #:page-path   page-path
              #:atom-path   "feeds/all.atom.xml"
              #:rss-path    "feeds/all.rss.xml"
              #:contents    body))
