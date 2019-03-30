    Title: Keyword structs
    Date: 2013-03-11T23:14:03
    Tags: Racket, macros, software

> **NOTE**: You may want to skip to [Keyword structs, revisited](/2015/07/keyword-structs-revisited.html).

A good rule of thumb in [Racket][] is to use a `struct` instead of
`list` when you're juggling more than two or three items.

<!-- more -->

For ad-hoc prototyping, you can use a `list`:

```racket
;; Return first name, last name, and age
(define (get-person)
  (list "John" "Doe" 32))

(define p (get-person))
(first p)  ; => "John"
(second p) ; => "Doe"
(third p)  ; => 32
```

Getting the stuff out is a bit cleaner using `match`, which lets you
"destructure" the list and assign to variables in one swell foop:

```racket
(match (get-person)
  [(list first second p)
   (values first second p)])
; "John"
; "Doe"
; 32
```

But what if you need to add or delete items later?  It's error-prone.

That's where a real `struct` can help:

```racket
(struct person (first last age))
(define (get-person)
  (person "John" "Doe" 32))

(define p (get-person))
(person-first p)
; "John"
(person-second p)
; "Doe"
(person-age p)
; 32

;; Or using `match`:
(match (get-person)
  [(person first last age)
   (values first last age)])
; "John"
; "Doe"
; 32
```

Now let's say you add a social security number field, `ssn`, and put it
_before_ the existing `age` field:

```racket
(struct person (first last ssn age))
(define (get-person)
  (person "John" "Doe" "xxx-xx-xxxx" 32))
```

Everything still works fine, because you're extracting the fields by-name:


```racket
(define p (get-person))
(person-first p)
; "John"
(person-second p)
; "Doe"
(person-age p)
; 32
```

Although if you used `match`, which is by-position, that needs to be
updated:

```racket
(match (get-person)
  [(person first last age)
   (values first last age)])
; match: wrong number for fields for structure person: expected 4 but got 3
;  at: (first last age)
;  in: (person first last age)
```

So you fix it:

```racket
(match (get-person)
  [(person first last ssn age)
   (values first last ssn age)])
; "John"
; "Doe"
; "xxx-xx-xxxx"
; 32
```

## Making structs

All of the above is about using already-created structs.

But what if your program needs to create the struct in many places,
too?

After all, _creating_ a struct has exactly the same form/shape as
creating a list:

```racket
(list   "John" "Doe" 32)
(person "John" "Doe" 32)
```

It's just `person` instead of `list`. Either way, you're specifying
the fields by-position, not by-name. If you have a struct with more
than a few fields:

```racket
(struct foo (a b c d e f g h))
```

Then creating the struct is itself error-prone. You will probably
start jotting down comments to help you keep track of what field
you're on:

```racket
(foo 10     ;a
     "foo"  ;b
     13     ;c
     "bar"  ;d
     "baz"  ;e
     #f     ;f
     "x"    ;g
     42     ;h
     )
```

It would help if we could turn those comments into actual
keywords. Using keyword arguments is helpful for any function with
more than a few arguments. We'd like to write:

```racket
(foo #:a 10
     #:b "foo"
     #:c 13
     #:d "bar"
     #:e "baz"
     #:f #f
     #:g "x"
     #:g 42
     )
```

That way, Racket could help us catch mistakes. Even better, we're free
to supply the arguments in a different order, and it's OK. It's
by-name, not by-position.

So certainly we could define a `foo/keyword` function like this, which
calls the plain `foo` struct constructor. I've done this many
times. Admittedly, if you change the `foo` struct, you have to
change this function, too. But usually they're adjacent in the source
code, and anyway it's only the one place to make the mistake.

Even so, it would be neat if Racket had an option to create such
keyword argument constructors for `struct`s automatically.

## A macro

Well, this is Racket. Any sentence that starts with, "It would be neat
if Racket could _XYZ_", can be answered with, "And I can add that to
Racket myself!"

Here's what we'd be writing by hand:

```racket
(struct foo (a b c ...))

(define (foo/kw #:a a #:b b #:c c ...)
  (foo a b c ...))
```

We're defining a function whose name is the struct name with `"/kw"`
appended. For each struct field, we want a keyword argument where the
keyword is the same as the field name.

So here's the macro:

```racket
#lang racket

(require (for-syntax racket/syntax
                     racket/list))

(begin-for-syntax
 (define (syntax->keyword stx)
   (string->keyword (symbol->string (syntax->datum stx)))))

(define-syntax (struct/kw stx)
  (syntax-case stx ()
    [(_ id (field ...) opt ...)
     (with-syntax ([kw-ctor (format-id stx "~a/kw" #'id)]
                   [(kw+fld ...) (append*
                                  (map (lambda (fld)
                                         (list (syntax->keyword fld)
                                               fld))
                                       (syntax->list #'(field ...))))])
       #'(begin
           (struct id (field ...)
                   opt ...)
           (define (kw-ctor kw+fld ...)
             (id field ...))))]))

;; Example usage:
(struct/kw foo (a b c) #:transparent)
(foo 1 2 3)
; => (foo 1 2 3)
(foo/kw #:a 1 #:b 2 #:c 3)
; => (foo 1 2 3)
```

Lines 2-3 `require` some modules that aren't part of the `racket/base`
environment that macros run in.

Lines 6-8 define a helper function that can be used by a macro. To do
that, the function must be `defined-for-syntax`.

Line 10 onward is the macro definition. In a fairly typical pattern,
there are two halves. Lines 13-18 use `with-syntax` to set up stuff
that we'll use in the template, and lines 19-23 are the template (the
code we want the macro to write for us).

The first `with-syntax` item is

```racket
                  [kw-ctor (format-id stx "~a/kw" #'id)]
```

This creates a new pattern variable named `kw-ctor` that is the
struct name with `"/kw"` appended.

The second `with-syntax` item looks a little hairy at first glance:

```racket
                   [(kw+fld ...) (append*
                                  (map (lambda (fld)
                                         (list (syntax->keyword fld)
                                               fld))
                                       (syntax->list #'(field ...))))]
```

This creates the arguments to our function, the sequence of keyword
and argument pairs like `#:a a`.  We get the list of field names using
`(syntax->list #'(field ...))`. We `map` that through a function that
creates each pair and puts each in a `list`. Since that yields a list
of such lists, we run it through `append*` to "flatten it" from
something like

```racket
'((#:a a) (#:b b) (#:c c))
```
to:

```racket
'(#:a a #:b b #:c c)
```

Which is the argument list we want for our constructor.


UPDATE: I posted this without sleeping on it and noticed a few typos
the next morning, as well as misstating how `match` can and can't
help. Fixed.


[Racket]: (http://www.racket-lang.org)
