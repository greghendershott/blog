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
         (multi-in racket (date file format function match string))
         (only-in frog/paths slug)
         "util.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector posts-dir)
     (display "Title: ")
     (define title (read-line))

     (match-define (struct* date ([year y] [month m] [day d])) (current-date))
     (define year (~a y))
     (define (~00 n) (~r n #:min-width 2 #:pad-string "0"))
     (define month (~00 m))
     (define day (~00 d))
     (define 8601-str (~a year "-" month "-" day "T00:00:00Z"))

     (define post-file (build-path posts-dir year month
                                   (~a (slug (string-downcase title)) ".md")))
     (make-parent-directory* post-file)
     (call-with-output-file*/delete
      #:exists 'error post-file
      (λ (out)
        (displayln
         @~a{    Title: @title
                 Date: @8601-str
                 Tags:

             This is part of the blurb.

             <!-- more -->

             This is the rest.
             }
         out)))
     (displayln @~a{Created @(path->complete-path post-file)})]))

