    Title: Parameters in Racket
    Date: 2013-04-02T10:00:18
    Tags: Racket

Racket [parameters][] let you manage stateful global variables in a
way that _feels_ more functional and is also thread- and
continuation-safe.  A convenient `parameterize`[racket] form lets you
change and restore them. I'll discuss this and show how I map
parameters to a configuration file.

<!-- more -->

## Parameters

Coming from C/C++, at first I wasn't sure what to make of Racket
parameters. Telling me, "They're similar to Lisp's `defvar`" wouldn't
have helped. Neither would, "Parameters correspond to preserved thread
fluids in Scsh", as says the [reference][parameters].

First, there's some potential jargon confusion. Whereas a C person
would talk about "calling a function with arguments" or "passing
arguments to a function", a Racketeer might sometimes talk about
"applying parameters to a function". In that usage, "parameter" means
"argument".

But the "parameters" I'm talking about _aren't_ function arguments. In
fact Racket parameters can be an _alternative_ to function arguments,
as we'll see below.

One way to look at parameters is that, although we prefer to be purely
functional, sometimes it's more practical to have some stateful global
variables, and parameters are a saner way to do that.

## The function signature

A parameter is a function that takes one, optional argument.  If you
call the function with no arguments, it returns the existing value.
If you give the function a value, that becomes the new value of the
parameter[^why doesn't setting a parameter return the value?].

```racket
(define current-foo (make-parameter 42))
(current-foo) ;; => 42
(current-foo 10)
(current-foo) ;; => 10
```

By convention, parameters are named with the prefix `current-`. That's
somewhat verbose, and it's not an absolute rule. But if you're writing
code for another Racketeer, using `current-` will afford "I'm a
parameter".

Racket predefines some parameters you may already know, such as
`current-output-port`[racket], `current-input-port`[racket], and
`current-error-port`[racket].

## Thread safety

If parameters were _only_ a stateful function, you could write them
using the usual "let over lambda" pattern for a closure:

```racket
(define roll-my-own-param
  (let ([v #f])
    (case-lambda
      [() v]
      [(new) (set! v new)])))
```

However parameters do more than this. They use [thread cells][],
meaning that each thread gets its own copy of the parameter. This is
one of the ways in which using parameters is a saner way to deal with
stateful global variables--threads won't stomp on each others' values.

## "Configuration" arguments

Often you'll use a parameter to avoid passing excessive
"configuration" arguments to a function.  For example, you could have
this:

```racket
(define (fribble interesting-arg option-1 option-2 option-3)
  ;; consult option-X args for how to behave doing
  ;; something to `interesting-arg`
  )
```

But let's say only `interesting-arg` tends to vary for each call. The
`option-x` args tend to be the same. Requiring every call site to pass
them would be tedious and even error-prone, especially should you ever
redesign to change or add options.

Using parameters you can instead do this:

```racket
(define current-fribble-option-1 (make-parameter ...))
(define current-fribble-option-2 (make-parameter ...))
(define current-fribble-option-3 (make-parameter ...))

(define (fribble interesting-arg)
  ;; consult current-fribble-option-x parameters
  )
```

This way, the options can be specified only as/when needed.

## Parameterize

Another advantage of parameters is a handy form to:

1. Set them[^must be parameter] to new values.

2. Do something.

3. Automatically restore the original values.

The form is called `parameterize`[racket]:

```racket
(parameterize ([fribble-option-1 42]
               [fribble-option-2 43]
               [fribble-option-3 44])
  (fribble x))
;; Then out here, the original values of fribble-option-x are restored
```

For example if you want to temporarily redirect and capture output to
a string:

```racket
(define o (open-output-string))
(parameterize ([current-output-port o])
  (displayln "Hello")  ;; goes to `o`
  (displayln "World")) ;; goes to `o`
;; Now current-output-port is restored.
;; Do something with the output captured in `o`.
```

Of course, `displayln`[racket] does accept an optional argument for
the `output-port`[racket] to which to display. You could keep passing
`o` to each call. But using `parameterize`[racket] avoids that
repetition. That would matter more when there were many more call
sites (unlike the simple example above) and/or when there were many
more configuration options (unlike `displayln`[racket]). Anyway, some
functions might not have any such optional argument and can only be
controlled via a parameter.

### Performance

Jay McCarthy has an excellent series of blog posts about continuation
marks: [Part I][], [Part II][], [Part III][].

In [Part II][] he compares the performance of `dynamic-wind`[racket]
and `parameterize`[racket]. The take-away: `dynamic-wind`[racket] is
faster, `parameterize`[racket] uses less space.

If you've profiled and _know_ you have a hot, tight loop, inside it
you might want to use `dynamic-wind`[racket] rather than
`parameterize`[racket]. Otherwise I'd suggest avoiding premature
optimization and not worrying about it.

[Part I]: http://jeapostrophe.github.com/2012-07-16-cont-mar-post.html
[Part II]: http://jeapostrophe.github.com/2012-07-25-cont-mar-post.html
[Part III]: http://jeapostrophe.github.com/2012-07-30-cont-mar-post.html

## Dot files

In writing [Frog][] I had a number of parameters that I wanted to be
loaded from a `.frogrc` configuration file.

Also, although I followed the `current-` naming convention for the
parameters, that didn't seem so friendly for the dot file. So I wanted
`current-foo` to map to a dot file variable named just `foo`.

Although I suspect it could be improved, what I came up with is the
following `parameterize`[racket]-like form,
`parameterize-from-config`:

```racket
(require (for-syntax racket/syntax))
(define-syntax (parameterize-from-config stx)
  (syntax-case stx ()
    [(_ ([name default] ...)
        body ...)
     (with-syntax ([(id ...) (map (lambda (x)
                                    (format-id stx "current-~a" x))
                                  (syntax->list #'(name ...)))]
                   [(key ...) (map (lambda (x)
                                     (symbol->string (syntax-e x)))
                                   (syntax->list #'(name ...)))])
       #'(parameterize ([id (get-config key default)] ...)
           body ...))]))
```

Some helper functions are omitted; full source [here][Frog].

Example use:

```racket
(module+ main
  (parameterize ([top (current-directory)])
    (parameterize-from-config ([scheme/host raise-config-required-error]
                               [title "Untitled Site"]
                               [author "The Unknown Author"]
                               [index-full? #f]
                               [feed-full? #f]
                               [google-analytics-account #f]
                               [google-analytics-domain #f]
                               [disqus-shortname #f]
                               [pygments-pathname #f]
                               [decorate-feed-uris? #t]
                               [feed-image-bugs? #f]
                               [older/newer-buttons "both"])
      (command-line
          ....
```

## Conclusion

Parameters are a way to handle stateful global variables in a way that
_appears_ more functional, and is thread- and continuation-safe. This
makes it possible to move "configuration" options out of function
arguments. Finally, the `parameterize`[racket] form makes it
convenient to change parameters for awhile, then assure that the
original values are restored.

[parameters]: http://docs.racket-lang.org/reference/parameters.html
[thread cells]: http://docs.racket-lang.org/reference/threadcells.html
[Frog]: https://github.com/greghendershott/frog

[^why doesn't setting a parameter return the value?]: I've wondered
why setting a parameter doesn't return the value that you passed
it.

[^must be parameter]: Interestingly, `parameterize`[racket] doesn't
work with _any_ function that has a `parameter`-like signature, such
as `roll-my-own-param`, above. It must be a real parameter that
satisfies the `parameter?`[racket] predicate.
