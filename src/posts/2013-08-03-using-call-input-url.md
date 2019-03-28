    Title: Using call/input-url
    Date: 2013-08-03T22:28:56
    Tags: Racket, racket-cookbook

When I learned Racket, one of the first things I wanted to try was
doing HTTP requests. And Racket's `net/url` module is great.

Racket was the first real Lisp/Scheme family language I ever learned.
As a result I was focused on building blocks like ports, and assuming
I would need to open and close them directly all the time. At that
early stage, I also didn't really appreciate the value of higher-order
functions. So I overlooked the value of `call/input-url`[racket]. I
sometimes see other folks do the same, and wanted to write this short
blog post.

<!-- more -->

Let's say we want a function to take a `url?`, make an HTTP `GET`
request, and return the response as a `string?`. For lack of a better
name, we'll call our function `fetch`.

First we need to `require` the `net/url` module:

```racket
(require net/url)
```

I won't keep repeating that in the various examples that follow.

Our first cut at `fetch` uses all the basic, obvious functions:

```racket
(define (fetch url)
  (define in (get-pure-port url))
  (define out (open-output-string))
  (copy-port in out)
  (get-output-string out))
```

We try using it like this:

```racket
(fetch (string->url "http://www.racket-lang.org/"))
```

And it gives output like this:

```racket
"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML "
; ... lots more omitted ...
```

Great. So this seems to work. But how can we improve it?

First, the business with `open-output-string`, `copy-port`, and
`get-output-string` is a bit tedious. Really, we just want to change
an `input-port?` into a `string?`. Turns out Racket supplies
`port->string`[racket], which does just that. So we can simplify:

```racket
(define (fetch url)
  (define in (get-pure-port url))
  (port->string in))
```

Much better. Unfortunately, there's a problem. We don't close the
`input-port?` returned from `get-pure-port`. Let's do that:

```racket
(define (fetch url)
  (define in (get-pure-port url))
  (begin0
      (port->string in)
    (close-input-port in)))
```

If you're unfamiliar with `begin0`[racket], it's like `begin`[racket]
except it returns the value of the _first_ expression instead of the
last one. `begin0`[racket] fits the pattern where you need to return a
value from a resource then close/release the resource.

OK, but, what if an exception were thrown sometime before we reached
`close-input-port?`, for example some runtime error inside
`port->string?` on line 4? The port would remain open.

Well, we could wrap this in an exception handler:

```racket
(define (fetch url)
  (define in (get-pure-port url))
  (with-handlers ([exn:fail? (lambda (exn)
                               (close-input-port in)
                               (raise exn))])
    (begin0
        (port->string in)
      (close-input-port in))))
```

But this is really verbose. We're writing a lot of boilerplate code
to handle exceptions, compared to the task we care about.

Fortunately, Racket provides `call/input-url`[racket]. It takes three
arguments: a `url?`, a connection function such as `get-pure-port`,
and a "handler" function to do something with the opened
`input-port?`.  It guarantees that the input port will be closed, even
if an exception is thrown.

Using that:

```racket
(define (fetch url)
  (call/input-url url
                  get-pure-port
                  (lambda (in)
                    (port->string in))))
```

The handler function takes an `input-port?` and returns `any` ---
whatever you need it to -- which in turn is what `call/input-url`
returns.

Hey wait a second. I notice that `port->string` is just such a
function: `(input-port? . -> . any)`. We don't need to wrap that in a
`lambda`. We can supply it directly:

```racket
(define (fetch url)
  (call/input-url url
                  get-pure-port
                  port->string))
```

And that's the final version. This is a nice example of how using
higher-order functions can result in elegant code with a clean
separation of concerns.

tl;dr: When you need to do an HTTP `GET` in Racket, you probably want
to use `call/input-url`[racket]. It ensures the HTTP input port will
be closed, and it nudges you into using higher-order functions.
