    Title: Racket workflow
    Date: 2014-11-14T12:00:00
    Tags: Racket

If you're coming to Racket from another REPL language (such as another
Lisp), this post might be real Captain Obvious material.

But if you're coming to Racket from an edit/compile/debug language
like C or C++, it might be unclear what a typical workflow is. You
might have questions like:

- How do I compile?
- How do I debug?

<!-- more -->

After seeing a question like this recently[^1], I figured I'd write about
my typical workflow in DrRacket or Emacs. It is:

[^1]: I lie. I wrote most of this post 8 months ago. It's been sitting
around as a draft. I finally decided to review and publish it.

1. Write a small function, in a source file.
2. Try calling the function, in the REPL.
3. Write some more examples of calling it, in the source file.
4. If any problems, fix them and go to 2.
5. Change the examples into `rackunit` tests.

Let's walk through these steps with a simple example.

## Write a small function

Here's `file.rkt`:

```racket
#lang racket

(define (twice n)
  (* n 2))
```

## Try calling it in the REPL

What is "the REPL"? REPL stands for Read Eval Print Loop. You have a
prompt, and can enter Racket expressions to be evaluated.

- DrRacket calls this the "interactions" pane, on the right or
  bottom.

- Emacs [racket-mode] calls this the `*Racket REPL*` buffer.

[racket-mode]: https://github.com/greghendershott/racket-mode

In both cases:

- The REPL appears the first time you choose **Run**, for example by
  pressing <kbd>F5</kbd>.

- The REPL will be "inside" the file (module) that you're editing --
  meaning that things defined in the file are visible and usuable from
  the REPL.

In the REPL, try calling `twice` with various values and see what
happens:

```racket
file.rkt> (twice 0)
0
file.rkt> (twice 2)
4
```

Looks good.

## Put the examples in the source file

Some of these examples you try may be interesting. Copy them from the
REPL to the source file:

```racket
#lang racket

(define (twice n)
  (* n 2))

(twice 0)
(twice 2)
```

When you run the file, you'll see this output in the REPL:

```racket
0
4
```

Expressions at the top level in a file -- such as these function
applications -- print their values when the file is run.

## Fix things

There's nothing to fix in this example. `twice` is behaving as we
expect. But if you make fixes, it's easy to quickly try the revised
version, as we saw above.

## Change the examples into unit tests

Next, simply nest your examples inside:

```racket
(module+ test
  (require rackunit)
  (check-equal? _example1_ _result1_)
  (check-equal? _example2_ _result2_)
  ;; and so on...
  )
```

So our example becomes:

```racket
#lang racket

(define (twice n)
  (* n 2))

(module+ test
  (require rackunit)
  (check-equal? (twice 0) 0)
  (check-equal? (twice 2) 4))
```

If I'm sure the function result value is correct, I'll often just copy
the output from the REPL into the right hand side of the
`check-equal?` form.

But only after checking carefully that it's correct. Nothing sucks
like a unit test whose expected value came from a buggy actual output.
(Not that _I_ would make that mistake, but I have a friend who knows
someone who did this once. _\*coughs\*_)

---

And that's it. This code example is extremely simple, but even with
more realistic examples, this is my usual workflow:

1. Make simple functions.
2. Try them "live" in the REPL.
3. Accumulate some interesting examples.
4. When things look good, "bake" the examples as unit tests.


## What about debugging?

In C/C++, I was from the school that, whenever you write new code, the
_very_ first thing you do is step through it in the debugger. You
don't just hit "Go" and look at the results. Instead, you step
through, line by line, and see that it's behaving the way you want. I
believe that's a worthwhile discipline in a language where every line
of your program is mutating a variable.

When I first learned Racket, I figured it would be similar. But as I
showed above, that's not really the case. Although DrRacket has a GUI
debugger, and it works, I rarely use it. Instead, the interactive REPL
serves much of the same purpose -- especially when you're writing
small functions that don't mutate state. You can interact with those
functions, and see how they actually behave with various inputs.

Mostly that's all I need. But when that's not enough, a good
old-fashioned `printf` usually suffices. It may sound too simple, but
when I ask very experienced Racket developers, that's what they do.

Now, if you find yourself sticking `printf`s at the start of more than
a couple functinos to see the call path -- there's an easier way:
`(require racket/trace)`, then put `(trace function-name)` in your
source or in the REPL to include a function in the trace, or `(untrace
function-name)` to remove it.

Finally, when it comes to debugging _macros_, DrRacket's macro stepper
is quite brilliant. Not only does it show the expansion steps (which
you can also get using `expand-once` and `expand` at the REPL), it
draws errors to show the origin of identifiers and lets you inspect
the full syntax objects.  You may not need it for a simple macro, but
when you need it, it's a lifesaver.
