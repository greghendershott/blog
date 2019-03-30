    Title: Using syntax/loc and unit test macros
    Date: 2014-01-31T18:21:56
    Tags: Racket, racket-cookbook, macros

In my [previous post], I wrote about a nuance with `syntax/loc`, using
the example of a macro that both `define`s and `provide`s a
function. But why don't I back up, and look at a simpler example of
why you'd want to use `syntax/loc`. The example is a simple macro you
might often find yourself wanting, to reduce the tedium of writing
unit test cases.

[previous post]: http://www.greghendershott.com/2014/01/using-syntax-loc.html

<!-- more -->

Let's say we have a function `my-function-that-increments`, with some
unit tests.

```racket
#lang racket

(require rackunit)

(define (my-function-that-increments x)
  (add1 x))

(check-equal? (my-function-that-increments 0)
              1)
(check-equal? (my-function-that-increments 1)
              1)
```

The last test correctly fails[^1], and the rackunit error message points
to the `check-equal?` on line 10:

[^1]: For some definition of "correctly".

```racket
--------------------
FAILURE
actual:     2
expected:   1
name:       check-equal?
location:   (#<path:/tmp/bp.rkt> 10 0 151 62)
expression: (check-equal? (my-function-that-increments 1) 1)

; Check failure
--------------------
```

Great. But let's say we have dozens of tests, and typing all the
`check-equal?` and `my-function-that-increments` is wearisome. We
think, "Aha, I can write a macro!" So we write:

```racket
#lang racket

(require rackunit)

(define (my-function-that-increments x)
  (add1 x))

(define-syntax (chk stx)
  (syntax-case stx ()
    [(_ input expected)
     #'(check-equal? (my-function-that-increments input)
                     expected)]))

(chk 1 0)
(chk 2 1)
```

Much less tedious. Nice.

But...hey wait. The rackunit error message points to line 11:

```racket
--------------------
FAILURE
actual:     3
expected:   1
name:       check-equal?
location:   (#<path:/tmp/bp.rkt> 11 7 166 80)
expression: (check-equal? (my-function-that-increments 2) 1)

; Check failure
--------------------
```

Line 11 isn't where the failing `(chk 2 1)` is. It's _inside our
macro_. Gah. We wanted to write this macro because we have dozens of
unit tests...but we can't see which one of them failed? This whole
idea of using a macro seems to have backfired.

Fortunately, this is where `syntax/loc` helps. It lets us specify the
source location for the syntax returned from our macro.

The macro above uses `#'`, which is shorthand for `syntax`. First
let's rewrite the macro using `syntax`:

```racket
(define-syntax (chk stx)
  (syntax-case stx ()
    [(_ input expected)
     (syntax (check-equal? (my-function-that-increments input)
                           expected))]))
```

Of course this still has the source location problem. But, we change
`syntax` to `syntax/loc`, supplying it the `stx` given to our macro:

```racket
(define-syntax (chk stx)
  (syntax-case stx ()
    [(_ input expected)
     (syntax/loc stx
       (check-equal? (my-function-that-increments input)
                     expected))]))
```

Here's the full new sample, so that the line numbers work for this
blog post:

```racket

#lang racket

(require rackunit)

(define (my-function-that-increments x)
  (add1 x))

(define-syntax (chk stx)
  (syntax-case stx ()
    [(_ input expected)
     (syntax/loc stx
       (check-equal? (my-function-that-increments input)
                     expected))]))

(chk 1 0)
(chk 2 1)
```

And now rackunit has the correct source location to report, line 16:

```racket
--------------------
FAILURE
actual:     3
expected:   1
name:       check-equal?
location:   (#<path:/tmp/bp.rkt> 16 0 281 9)
expression: (check-equal? (my-function-that-increments 2) 1)

; Check failure
--------------------
```

Hopefully this shows how `syntax/loc` can help with the sort of
"casual" macro you might frequently want to write.
