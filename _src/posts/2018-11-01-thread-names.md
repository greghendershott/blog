    Title: Thread names
    Date: 2018-11-01T20:10:21
    Tags: Racket

Sometimes people want Racket `thread`[racket]s to have useful names --
for example to show in logger output. Here is one way to do it.

<!-- more -->

When you call `thread` it returns a _thread descriptor_.

You may also get the thread descriptor of the current thread using
`current-thread`[racket].

The `print`[racket]ed representation of a thread descriptor includes
the `object-name`[racket] of its thunk procedure.

Often the thunk is an anonymous function. In that case the function
object-name is something like `/path/to/file.rkt:1:3` -- so the thread
descriptor prints as `#<thread:/path/to/file.rkt:1:3>`.

```racket
(thread (λ () #f)) ;=> #<thread:/tmp/thread.rkt:9:8>
```

It is more interesting when you name the thunk:

```racket
(define (foo) #f)
(thread foo) ;=> #<thread:foo>
```

You can also use `object-name` on the thread descriptor to extract the
thunk's `object-name` as a symbol:

```racket
(object-name (thread (λ () #f))) ;=> '/tmp/thread.rkt:18:21
(object-name (thread foo))       ;=> 'foo
```

What if you start 10 threads that all use the same thunk procedure?
You can use `procedure-rename`[racket] to rename the thunk each time:

```racket
(thread (procedure-rename foo 'bar)) ;=> #<thread:bar>
(thread (procedure-rename foo 'baz)) ;=> #<thread:baz>
```

The new name can be any symbol -- including one generated at runtime:

```racket
(define (now-sym) 
  (string->symbol (~a (current-inexact-milliseconds))))
(thread (procedure-rename foo (now-sym))) ;=> ex: #<thread:1541116698791.029>
(thread (procedure-rename foo (now-sym))) ;=> ex: #<thread:1541116698791.18>
```

Of course, you can include in the generated name any interesting
identifying information from your problem domain -- such as an "ID" or
other details about a "job", "request", or "message".

So something like this:

```racket
#lang at-exp racket/base

(require racket/format)

(define-logger thor)

(define (thor)
  (log-thor-debug @~a{hammering job @(object-name (current-thread))}))

(define (job-id) 
  (string->symbol (~a (current-inexact-milliseconds))))

(thread (procedure-rename thor (job-id)))
(thread (procedure-rename thor (job-id)))
```

Can produce logger output like this:

```sh
[  debug] thor: hammering job 1541119025888.965
[  debug] thor: hammering job 1541119025889.021
```

In conclusion, it is possible for threads to have unique names in
Racket -- provided you're OK giving unique names to the thunks run by
the theads.

