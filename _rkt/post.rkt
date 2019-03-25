#lang racket/base

(provide (struct-out post))

(struct post
  (title      ;string?
   datetime   ;string? - 8601 datetime format
   tags       ;(listof string?)
   blurb      ;(listof xexpr/c) - the post summary
   more?      ;boolean? - is `body` more than just `blurb`?
   body       ;(listof xexpr/c) - the post full contents
   ) #:prefab)
