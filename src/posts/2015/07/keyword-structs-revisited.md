    Title: Keyword structs, revisited
    Date: 2015-07-04T13:15:57
    Tags: Racket, macros, software

This revises my [Keyword structs] post to fix some mistakes, discuss
the `struct*` `match` pattern, and rewrite the macro to use
`syntax-parse` and support default arguments.

[Keyword structs]: /2013/03/keyword-structs.html

---

A good rule of thumb in [Racket] is to use a `struct` instead of
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
"destructure" the list and bind to identifiers in one swell foop:

```racket
(match (get-person)
  [(list first last age)
   (values first last age)])
; "John"
; "Doe"
; 32
```

But what if you need to add or delete list members later? It's
error-prone.

That's where a real `struct` can help:

```racket
(struct person (first last age))
(define (get-person)
  (person "John" "Doe" 32))

(define p (get-person))
(person-first p)
; "John"
(person-last p)
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

Now let's say you add a social security number field, `ssn`:

```racket
(struct person (first last ssn age))
(define (get-person)
  (person "John" "Doe" "xxx-xx-xxxx" 32))
```

Everything still works fine when you access the fields by-name:


```racket
(define p (get-person))
(person-first p)
; "John"
(person-last p)
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

So you need to fix it:

```racket
(match (get-person)
  [(person first last ssn age)
   (values first last ssn age)])
; "John"
; "Doe"
; "xxx-xx-xxxx"
; 32
```

## `struct*`

This is where the `struct*` `match` pattern can help. By getting the
fields by-name, it is insulated from the addition of new fields:

```racket
(match (get-person)
  [(struct* person ([first first] [last last] [age age]))
   (values first last age)])
; "John"
; "Doe"
; 32
```

This needs to be updated only if/when you need the new `ssn` field. So
although it's more verbose, using `struct*` is more resilient.

We could reduce the verbosity, by allowing either `[field pat]` or
just `field` -- where the latter expands to use the same symbol for
both the field and pattern, as we wrote out in the example above. This
would be a nice enhancement to the official `struct*` in
`racket/match`. Meanwhile here's a `struct**` match expander that
wraps `struct*` to do so:

```racket
#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(define-match-expander struct**
  (λ (stx)
    (define-syntax-class field
      (pattern [id:id pat:expr])
      (pattern id:id #:with pat #'id))
    (syntax-parse stx
      [(_ struct-id:id (field:field ...))
       #'(struct* struct-id ([field.id field.pat] ...))])))

(module+ test
  (require rackunit)
  (struct foo (a b c))
  (define x (foo 1 2 3))
  (check-equal? (match x [(struct** foo (a b [c x])) (list a b x)])
                x)
  (check-equal? (match x [(struct*  foo ([a a][b b][c c])) (list a b c)])
                (match x [(struct** foo (a    b    [c c])) (list a b c)])))
```

## Making structs

Creating an instance of a struct has exactly the same form/shape as
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
     42)    ;h
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
     #:h 42)
```

That way, Racket could help us catch mistakes. Even better, we're free
to supply the arguments in a different order, and it's OK. It's
by-name, not by-position.

As a bonus, it would be great to have optional arguments, with a
default value. (Especially since `struct`s `#:auto` option requires
all fields to share the same default value.)

Certainly we could define a `foo/keyword` function like this, which
calls the plain `foo` struct constructor. I've done this many times.
Admittedly, if you change the `foo` struct, you have to change this
function, too. But usually they're adjacent in the source code, and
anyway it's only the one place to make the mistake.

Even so, it would be neat if Racket had an option to create such
keyword argument constructors for `struct`s automatically.

## A macro

Well, this is Racket. Any sentence that starts with, "It would be neat
if Racket could ___", can be answered with, "And I can add that to
Racket myself!"

Here's what we'd be writing by hand:

```racket
(struct foo (a b c ...))

(define (foo/kw #:a a #:b b #:c [c 42] ...)
  (foo a b c ...))
```

We're defining a function whose name is the struct name with `"/kw"`
appended. For each struct field, we want a keyword argument, where the
keyword is similar to the field name. Also, we'd like to support
optional arguments.

So here's a macro:

```racket
#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(begin-for-syntax
 (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum)))

(define-syntax (struct/kw stx)
  (define-syntax-class field
    (pattern id:id
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [id:id default:expr]
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default])))
  (syntax-parse stx
    [(_ struct-id:id (field:field ...) opt ...)
     (with-syntax ([ctor-id (format-id #'struct-id "~a/kw" #'struct-id)]
                   [((ctor-arg ...) ...) #'(field.ctor-arg ...)]) ;i.e. append*
       #'(begin
           (struct struct-id (field.id ...) opt ...)
           (define (ctor-id ctor-arg ... ...) ;i.e. append*
             (struct-id field.id ...))))]))

;;; Example usage:

;; Define a struct type
(struct/kw foo (a b [c 42]) #:transparent)

;; Use normal ctor
(foo 1 2 3)                ; => (foo 1 2 3)

;; Use keyword ctor
(foo/kw #:a 1 #:b 2 #:c 3) ; => (foo 1 2 3)

;; Use keyword ctor, taking advantage of default arg for #:c field
(foo/kw #:a 1 #:b 2)       ; => (foo 1 2 42)
```

Lines 2-6 `require` some modules that aren't part of the `racket/base`
environment that macros run in.

Lines 8-9 define a helper function that can be used by a macro. To do
that, the function must be `define` in a `begin-for-syntax` form.

Line 11 onward is the macro definition.

Lines 12-16 define a syntax class to use with `syntax-parse`. The
class matches struct fields, which can be either an identifier alone
or an `[identifier default-value]` form. In both cases, the syntax
class defines an extra bit of syntax, `ctor-arg`. For each field, this
is the arg spec to use in the definition of our special constructor
function. This will be something like `#:id id` in the first case or
`#:id [id default]` in the second case.

Lines 17-24 are the `syntax-parse` form. The pattern is:

```racket
    [(_ struct-id:id (field:field ...) opt ...)
```

This means there will be an identifier for the struct, followed by
a list of zero or more fields, and finally zero or more options.

Lines 19-20 use `with-syntax` to create a couple pattern variables:


```racket
     (with-syntax ([ctor-id (format-id #'struct-id "~a/kw" #'struct-id)]
                   [((ctor-arg ...) ...) #'(field.ctor-arg ...)]) ;i.e. append*
```

The first, `ctor-id`, is simply the name of our constructor function
-- append `/kw` to the user's struct identifier.

The second, `ctor-arg`, is our list of arg specs for the constructor
function. We'll need to `append*` these -- "flatten" them one level,
from a list of lists into a list. That's the reason for the funny
nested ellipses: `((ctor-arg ...) ...)` -- it sets us up to say
`ctor-arg ... ...` down on line 23.

Finally lines 21-24 are the template -- the syntax we're returning.
This is simply a `struct` definition plus the definition of our
special constructor function. Again, the business with the double
ellipses is how we `append*` a list of lists like this:


```racket
'((#:a a) (#:b b) (#:c [c 42]))
```

down to:


```racket
'(#:a a #:b b #:c [c 42])
```

Which is the argument list we want for our constructor.

And that's it. Although this macro doesn't exhaustively cover all
possible `struct` options, it's an example of something you could use
in a project to write code that is less repetitive and more resilient.

[Racket]: http://www.racket-lang.org
