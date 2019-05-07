    Title: Using syntax/loc
    Date: 2014-01-23T08:16:16
    Tags: Racket, macros

There's a nuance to [`syntax/loc`]. The documentation says, emphasis mine:

> Like `syntax`, except that the **immediate** resulting syntax object takes its source-location information from the result of _stx-expr_ (which must produce a syntax object), unless the template is just a pattern variable, or both the source and position of _stx-expr_ are `#f`.

What does "immediate" mean here?

<!-- more -->

Let's back up. Say that we[^1] are writing a macro to both `define` and
`provide` a function:

```racket
#lang racket

(require (for-syntax syntax/parse))

(define-syntax (defp stx)
  (syntax-parse stx
    [(_ (id:id arg:expr ...) body:expr ...+)
     #'(begin
         (define (id arg ...)
           body ...)
         (provide id))]))

(defp (f x)
  (/ 1 x))

(f 1)
;; => 1

```

A macro must return one syntax object, but we want to return both a
`define` and a `provide`. So we do the usual thing: We wrap them up in
a `begin`.

Everything fine so far.

Now let's say we call `f` and it causes a runtime error:

```racket
#lang racket

(require (for-syntax syntax/parse))

(define-syntax (defp stx)
  (syntax-parse stx
    [(_ (id:id arg:expr ...) body:expr ...+)
     #'(begin
         (define (id arg ...)
           body ...)
         (provide id))]))

(defp (f x)
  (/ 1 x))

(f 1)
; 1

(f 0)
; /: division by zero
; Context:
;  /tmp/derp.rkt:9:9 f
;  derp [running body]
```

See the problem? The error message points to line 9 -- inside the
`defp` macro. That's not helpful. We want it to say line 13 -- where
we use `defp` to define the `f` function.

Fortunately, we've heard of [`syntax/loc`]. Instead of specifying the
macro template using `#'` a.k.a. `syntax`, we can use
`syntax/loc`. Easy. So we update our macro:

```racket
(define-syntax (defp stx)
  (syntax-parse stx
    [(_ (id:id arg:expr ...) body:expr ...+)
     (syntax/loc stx  ;; <-- new
       (begin
         (define (id arg ...)
           body ...)
         (provide id)))]))
```

But that _still_ doesn't work. The error message still points to the
macro.

Huh. So we try other values, like `(syntax/loc id . . .)`. But still
no joy.

It turns out this is where that word "immediate" matters: `syntax/loc`
is setting the source location for the _entire_ `(begin . . . )`
syntax object -- but _not_ necessarily the pieces _inside_ it. That's
why the `define` isn't getting the source location we're supplying.

The solution? Use `syntax/loc` directly on the `define` piece:

```racket
#lang racket

(require (for-syntax syntax/parse))

(define-syntax (defp stx)
  (syntax-parse stx
    [(_ (id:id arg:expr ...) body:expr ...+)
     #`(begin
         #,(syntax/loc stx  ;; <-- new
             (define (id arg ...)
               body ...))
         (provide id))]))

(defp (f x)
  (/ 1 x))

(f 1)
; 1

(f 0)
; /: division by zero
; Context:
;  /tmp/derp.rkt:14:0 f
;  derp [running body]
```

And now the error message points to `f` on line 14. `\o/`

---

There's also a `quasisyntax/loc`.

Cheat sheet:

<table border='1'>
<th>
  <td>Default</td>
  <td>Source location</td>
</th>
<tr>
  <td>Plain</td>
  <td><code>#'</code> <em>a.k.a.</em> <code>syntax</code></td>
  <td><code>syntax/loc</code></td>
</tr>
<tr>
  <td>Quasi</td>
  <td><code>#`</code> <em>a.k.a.</em> <code>quasisyntax</code></td>
  <td><code>quasisyntax/loc</code></td>
</tr>
</table>

---

To anticipate a comment: Yes, I know. I ought to add this to
[Fear of Macros], too.

[^1]: The "we" in this blog post was "me" yesterday. Big thanks to Eric
Dobson for pointing this out on #racket IRC.

[`syntax/loc`]: http://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fstxcase-scheme..rkt%29._syntax%2Floc%29%29

[Fear of Macros]: http://www.greghendershott.com/2013/01/fear-of-macros.html
