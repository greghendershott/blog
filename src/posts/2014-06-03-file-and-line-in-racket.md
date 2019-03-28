    Title: `__FILE__` and `__LINE__` in Racket
    Date: 2014-06-03T00:00:00
    Tags: Racket, racket-cookbook

Many languages have a variable (or preprocessor macro) called
`__FILE__` or `__file__` whose value is the pathname of the current
source file. Likewise `__LINE__` for the the source line number.

You probably need this less in Racket than you imagine. For example:

- We wouldn't test that `__FILE__` ends in `main.rkt`; instead we'd
  use a `main` submodule `(module+ main <your code here>)`.

- To get a data file `foo.dat` located in the same directory as a
  source file we'd use `(define-runtime-path foo.dat "foo.dat")`. If
  we're a package or executable this works regardless of where our
  files happen to get installed.

But if you really did need a `__FILE__` in Racket, how would you do
it?

<!-- more -->

Simply:

```racket
(syntax-source #'here) ;full path to the source file, i.e. __FILE__
(syntax-line #'here)   ;line number, i.e. __LINE__
```

You could use `#'0` or `#'42` instead. The only important thing about
`#'here` is that it's a _syntax object_ representing syntax from the
current source file. The `#'` prefix is what matters; it's reader
shorthand for the `syntax` function. So `#'here` is read as `(syntax
here)`, which you could also use if you don't mind typing a few more
keys.

```racket
;; Same as above
(syntax-source (syntax here)) ;full path to the source file, i.e. __FILE__
(syntax-line (syntax here))   ;line number, i.e. __LINE__
```

In some Lisps, a macro works on a plain s-expression -- the `0` or
`42` or `here` or `(+ 1 1)`. A _syntax object_ in Racket contains the
s-expression, and also information such as source location (line,
column, position, span) and lexical scope.[^1]

So in Racket, you simply need to make _some_ syntax object in your
source file, like `#'here`, and feed it to syntax object accessors
like `syntax-source` and `syntax-line`.[^2]

---

What if you want to write `__FILE__` and `__LINE__` like in other
languages? You could define these as macros in a file, and `require`
and use where desired:

```racket
#lang racket/base

(provide __FILE__ __LINE__)

(require (for-syntax racket/base))

(define-syntax (__FILE__ stx)
  ;; `stx` comes from where the macro was invoked, so give _that_ to
  ;; `syntax-source`.
  (with-syntax ([file (syntax-source stx)])
    (syntax-case stx ()
      [_ #'file])))

(define-syntax (__LINE__ stx)
  ;; `stx` comes from where the macro was invoked, so give _that_ to
  ;; `syntax-line`.
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [_ #'line])))
```

In this case, we care about the source file and line from where the
macro was invoked -- from where we typed `__FILE__` or `__LINE__`. So
we're careful to give that input `stx` to `source-file` or
`source-line`. (Whereas if we used some made-up syntax object literal
like `#'here`, it would give us the source or line of `#'here` in the
macro _definition_.)

---

It turns out that learning how to do a `__FILE__` in Racket ends up
being a gentle, practical introduction to syntax objects.

[^1]: For more about syntax objects see my
[Fear of Macros](http://www.greghendershott.com/fear-of-macros/). Or
if you're coming to Racket from another Lisp, it might be quicker to
read
[Eli Barzilay's blog post](http://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html).

[^2]: You might be wondering, why isn't there an error that no
identifier named `here` is defined? Great question. A syntax object
such as `#'here` a.k.a. `(syntax here)` is basically quoted data --
like `'here` a.k.a. `(quote here)`. We're not `eval`uating the syntax.
