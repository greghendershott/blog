    Title: Fallback when required function not available
    Date: 2014-06-02T09:27:17
    Tags: Racket, racket-cookbook

Let's say we want to use `find-collects-dir`, which was added
in Racket 6.0. We get a bug report from someone using Racket 5.3.6.

To fix this, we can `dynamic-require` the desired function; when it
doesn't exist, we can use our own fallback implementation.[^1]

<!-- more -->

The basic recipe:

```racket
(define (our-fallback ....)
  ....)

(define desired-function
  (dynamic-require 'collection/path 'desired-function
                   (thunk our-fallback)))
```

Here `(thunk x)` is just shorthand for `(lambda () x)`.

A simple but full example:

```racket
#lang racket/base

;; find-collects-dir was added in Racket 6.0.
(provide find-collects-dir)

(require racket/list
         racket/function
         racket/path)

(define (our-find-collects-dir)
  (apply build-path
         (reverse (cdr (reverse (explode-path (collection-path "racket")))))))

(define find-collects-dir
  (dynamic-require 'setup/dirs 'find-collects-dir
                   (thunk our-find-collects-dir)))
```

Let's say this is in a file `find-collects-dir.rkt`. Then elsewhere in
our program instead of `(require setup/dirs)` we do `(require
"find-collects-dir.rkt")`.

Now our program uses Racket's `find-collects-dir` when running on
Racket 6.0 or newer, otherwise it falls back to using our own
implementation.

Season with parsley. Serves many.

[^1]: Of course another approach is to _only_ use our own implementation, even in newer versions of Racket. Sometimes that would be simpler. However the "official" function might someday change in beneficial ways. Also, someday we might decide to stop supporting an older Racket version, in which case it's cleaner to discard this scaffolding and adjust the `require`s to use the "official" function directly.
