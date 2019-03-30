    Title: The ~> Threading Macro
    Date: 2013-05-24T10:02:55
    Tags: Racket, Clojure, macros

Although I prefer Racket, there are a few idioms from Clojure I
like. I'm particularly infatuated with the threading macros, `->` and
`->>`.

I was surprised how little documentation I could find for these. So
although I'm writing this for Racketeers, it's possible a few Clojure
folks might find it interesting, too.

<!-- more -->

> Note: You can use them in Racket with Asumu Takikawa's
> [#lang clojure][] or my own [#lang rackjure][]. In the latter, the
> threading macros are named `~>` and `~>>` (using a `~` instead of a
> `-`) so as not to conflict with Racket's use of `->` for
> contracts. I'll use the `~>` names in this blog post.

First, the threading macros have nothing to do with
concurrency. Instead, the macros "thread" values through a chain of
function calls.

Here's a simple example. You might write code like this:

```racket
(displayln (bytes-append (bytes->hex-string (sha1 in)) #"."))
```

The nesting makes it difficult to see the data flow. You need to read
not just right-to-left, but also inside-out. And although we
Racketeers don't exactly hate parentheses, there are an awful lot of
them.

Here's that with the `~>` "thread first" macro:

```racket
(~> (sha1 in)
    (bytes->hex-string)
    (bytes-append #".")
    (displayln))
```

It expands into _exactly_ the same code as above, but is easier to
read as a series of transformations. The `~>` macro takes the result
of each function, and "threads" it in as the first argument to the
next function.

In Clojure, `,` is whitespace so you can use it as a visual reminder
where the argument is being inserted:

```clojure
(-> (sha1 in)
    (bytes->hex-string ,)
    (bytes-append , #".")
    (displayln ,))
```

In Racket, I suppose you could use comments like so:

```racket
(~> (sha1 in)
    (bytes->hex-string #||#)
    (bytes-append #||# #".")
    (displayln #||#))
```

But that's a bit noisy. I've found that with a little experience you
don't really need that. It's fine to say:

```racket
(~> (sha1 in)
    (bytes->hex-string)
    (bytes-append #".")
    (displayln))
```

Plus, when any function takes just one argument, you can omit the
parentheses:

```racket
(~> (sha1 in)
    bytes->hex-string
    (bytes-append #".")
    displayln)
```

That's really clean.

For certain programs it's really helpful to use this style, which
emphasizes data flow. If you're nesting functions with `->` in the
name, like `port->string` or `string->bytes/utf-8` --- or functions
that _could_ be named that way --- that's often a good indicator that
`~>` might be natural.

As I searched for more information about the threading macro, I came
across the idea of [concatenative programming][]. Think of Unix
command line pipes. Think of postfix HP calculators. Think of postfix
languages like Forth, including newer ones like Cat.

If we Lispers can accept prefix notation, why not postfix? Like
extremists on opposite sides of an issue, we actually have more in
common with each other, than with the infix majority --- because we
care more than they do.  (I am mostly joking, but not entirely.)

However you view it, the emphasis is on a chain of
transformations. Maybe it's my background in music and audio
processing, but I find that a very clear and natural way to think
about many problems.

## Caveat: Don't forget it's a _macro_

Occasionally you'll have some function that _doesn't_ take the
interesting bit as the first argument:

```racket
(~> (list 1 2 3)
    (map add1) ;; won't work: map takes the list as the last argument
    displayln)
```

You might think, whatevs, I'll just supply an anonymous function that
takes a list as the first argument. I'll use `(lambda (xs) (map add1
xs))` or more elegantly `(curry map add1)`:

```racket
(~> (list 1 2 3)
    (lambda (xs) (map add1 xs))  ;; won't work!
    (curry map add1)             ;; won't work!
    displayln)
```

Why won't this work? Because `~>` is a _macro_ and it expands to:

```racket
    (lambda (list 1 2 3) (xs) (map add1 xs))
    (curry (list 1 2 3) map add1)
```

Unfortunately in such a situation you'll have to define the function
outside the `~>` macro: [^1]

```racket
(define map-add1 (curry map add1))
(~> (list 1 2 3)
    map-add1
    displayln)
```

The main take-away is that `~>` is a syntax transform, not a function
application. Often you won't need to care, but sometimes it will
matter.

## Thrush Combinator

Sometimes the threading macros are referred to as the thrush
combinator. They're not, really, because the threading macros are
_macros_, as we just saw. What they do have in common is specifying
the functions in order of execution: in data-flow order. In contrast
`compose` specifies the function in math notation order, the same
order in which we'd write them as nested expressions.

In other words, one implementation of thrush is simply:

```racket
(define (thrush . fs)
  (apply compose (reverse fs)))
```

Here are the permutations:

    Argument Order   Function   Macro
    --------------   --------   -----
     Math Notation   compose
         Data Flow   thrush      ~>


## Combined with applicable dicts

Sometimes I need to deal with JSON. Often this has nested object
literals, _a.k.a._ hash tables _a.k.a._ dictionaries.

This can be a bit painful in Racket:

```racket
(dict-ref (dict-ref (dict-ref some-dict 'a) 'b) 'c)
```

But [#lang rackjure][] has applicable dicts. When an application form
has two elements, and the second one is a `dict?`, then this expands
to a `dict-ref` using the first element as the key:

```racket
('a d)  ;; when (dict? d) is #t, expands to...
(dict-ref d 'a)
```

As a result, that nested `dict-ref` example can be written as:

```racket
('c ('b ('a some-dict)))
```

That's a bit shorter, but of course this feels "backwards and
inside-out". So, `~>` to the rescue:

```racket
(~> some-dict 'a 'b 'c)
```

Which is a very natural way to do a nested dict reference, and very
similar to JavaScript's:

```javascript
some_dict.a.b.c;
```

# Welcome your feedback

Those are my reflections on the threading macros. If you'd like to add
something, or correct some horrible misunderstanding, feel free to
leave a comment.


[#lang clojure]: https://github.com/takikawa/racket-clojure
[#lang rackjure]: https://github.com/greghendershott/rackjure
[concatenative programming]: https://en.wikipedia.org/wiki/Concatenative_programming_language


[^1]: Update: Although it's a bit ugly, you could also wrap any function-creating expressions with an extra set of parens. For example, `((lambda (xs) (map add1 xs)))` and `((curry map add1))`.
