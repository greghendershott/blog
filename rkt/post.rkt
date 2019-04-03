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

(provide (struct-out post))

(struct post
  (title      ;string?
   datetime   ;string? - 8601 datetime format
   tags       ;(listof string?)
   blurb      ;(listof xexpr/c) - the post summary
   more?      ;boolean? - is `body` more than just `blurb`?
   body       ;(listof xexpr/c) - the post full contents
   ) #:prefab)
