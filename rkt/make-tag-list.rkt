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
         (multi-in racket (contract file format match path string))
         (only-in "compile-post.rkt" datetime+tags->xexpr)
         "post.rkt"
         "page-xexpr.rkt"
         "util.rkt"
         "xexpr.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector tag-caches-path www output-html)
     (define tags (sort (for*/list ([path (in-directory tag-caches-path)]
                                    [name (in-value (file-name-from-path path))]
                                    [str  (in-value (path->string name))]
                                    #:when (not (equal? str "all")))
                          str)
                        string-ci<?))
     (make-directory* (path-only output-html))
     (call-with-output-file*/delete
      #:exists 'replace output-html
      (λ (out)
        (display "<!DOCTYPE html>" out)
        (display (xexpr->string
                  (tags-xexpr tags (file->uri www output-html)))
                 out)))]))

(define/contract (tags-xexpr tags page-path)
  (-> (listof string?) path-string? xexpr/c)
  (define links
    (for/list ([tag (in-list tags)])
      `(li (a ([href ,(~a "/tags/" tag ".html")]) ,tag))))
  (define contents
    `((ul ,@links)))
  (page-xexpr #:title       "Tags"
              #:keywords    (string-join tags ",")
              #:description "Tags"
              #:page-path   page-path
              #:atom-path   (~a "feeds/all.atom.xml")
              #:rss-path    (~a "feeds/all.rss.xml")
              #:contents    contents))
