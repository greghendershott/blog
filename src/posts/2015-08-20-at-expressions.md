    Title: At-expressions
    Date: 2015-08-20T10:15:04
    Tags: Racket, racket-cookbook

If you've heard of Racket "at-expressions", maybe you think they're
"that funny Scribble notation in which you write Racket
documentation."

In fact at-expressions are a general, alternative way to write
s-expressions. They can be used in various handy ways.

Let's look at using at-expressions for a few practical things like:

- "string interpolation"
- regular expressions
- "here" strings

<!-- more -->

# `#lang at-exp ...`

You can use the at-expression reader with a language by supplying
[`at-exp`] before the language. Examples:

```racket
#lang at-exp racket
#lang at-exp typed/racket
```

In the examples below, make sure you're using:

```racket
#lang at-exp racket
```

# `~a`

Before we talk more about at-expressions, note that `racket/format`
provides the function [`~a`]. `(~a v)` is a kind of shorthand for
`(format "~a" v)`, plus it offers many more formatting options.

```racket
#lang racket
(format "~a" "hi") ;"hi"
(~a "hi")          ;"hi"
(format "~a" 1)    ;"1"
(~a 1)             ;"1"
```

We'll use `~a` below.

# Basic at-expressions

At-expressions are a very well thought-out system; you can read about
the [full syntax]. For this post, we simply need to know that
`@func{string}` is equivalent to `(func "string")`. So we can rewrite:

```racket
(~a "foo bar") ; "foo bar"
```

as:

```racket
@~a{foo bar}   ; "foo bar"
```

(Note that `~a` is the name of the function we're calling. The `~` has
nothing to do with at-expressions; it's part of this function's name.)

Also special characters like `\` and `"` are automatically escaped:

```racket
@~a{A back slash \}   ; "A back slash \\"
@~a{"Double quotes"}  ; "\"Double quotes\""
```

Inside the curly brackets, you may use `@` again to "escape" to
any Racket expression. For example an expression like `(+ 1 1)`:

```racket
@~a{The sum of one and one is @(+ 1 1).} ; "The sum of one and one is 2."
```

Or simply the name of a variable like `x`:

```racket
(define x 0)
@~a{x is @x} ; "x is 0"
```
 
# String interpolation

You can use at-exps as the equivalent of "string interpolation" in
some other languages:

```racket
(define x 0)
(define y "foo")

@~a{x is @x and y is @y} ; "x is 0 and y is foo"
```
  
Normally in Racket you'd write that as:

```racket
(format "x is ~a and y is ~a" x y)
```

Which is fine, albeit you have to flip between the `~a`s on the left
and the values on the right, making sure they match up. The string
interpolation style is arguably easier to write, to read, and to
update later without making a mistake.

How about mixing formats, such as `~a` (display) and `~v` (print)? For
example with `format` we can write

```racket
(format "x is ~a and y is ~v" x y)` ; "x is 0 and y is \"foo\""
```

How can we do this using our at-exp? Well since `~a` is the outer
function it will display the value of any `~v` inside. Remember that
`@` lets us "escape" to _any_ Racket expression, not just a variable,
it could be a function application. So:

```racket
@~a{x is @x and y is @(~v y)} ; "x is 0 and y is \"foo\""
```
  
You can also surround the Racket expression in `|` characters. This is
useful if the expression needs to end next to plain text. You can
demarcate the identifier from the text:

```racket
@~a{x is @|x| and y is @|y|!} ; "x is 0 and y is foo!"
```

The `|` keeps `!` from being read as part of the identifier `y`.

# Regular expressions

Do you enjoy writing regular expressions like `#px"\\d\\.\\d"`? Me
neither.

Another useful example is avoiding the need to use `\\` to get a
`\` in string literals. This is especially handy for regular
expressions:

```racket
@pregexp{\d\.\d}  ; #px"\\d\\.\\d"
```

If you find `pregexp` too verbose, you could define a little alias:

```racket
(define px pregexp)
@px{\d\.\d}      ; #px"\\d\\.\\d"
```

# "Here" strings

Like shells, Racket has "here" strings:

```racket
(define multi-line #<<EOF
Some multi-line
string literal.
EOF
)
multi-line ; "Some multi-line\nstring literal."
```

Cool. However the indentation is tricky. You get extra spaces if you
do this:

```racket
(define multi-line #<<EOF
  Some multi-line
  string literal.
EOF
)
multi-line ; "  Some multi-line\n  string literal."
```

Oops. 

Also the `EOF` _must_ be alone on a line _and_ in column 0. You can't
let that get indented, and you can't put the closing paren on the same
line.

At-exps are more elegant and survive typical re-indentation:

```racket
(define multi-line @~a{Some multi-line
                       string literal})
multi-line ; "Some multi-line\nstring literal"
```

# How to write a literal `@`

If `@` is a magic escape character, how do you write a literal `@`?

1. We want a string, `"@"`.

2. How do we escape to any Racket expression, including (say) a
   string? Using `@`.

3. Therefore prepend a `@` to `"@"`:

```racket
@"@"
```

So for example:

```racket
@~a{The email is foo@"@"bar.com} ; "The email is foo@bar.com"
```

# Conclusion

This was a quick look at some practical ways to use at-expressions for
more than writing Scribble documentation. Again, feel free to read up
on the [full syntax].

[`~a`]: http://docs.racket-lang.org/reference/strings.html#(def._((lib._racket/format..rkt)._~7ea))
[`at-exp`]: http://docs.racket-lang.org/scribble/reader-internals.html#%28mod-path._at-exp%29
[full syntax]: http://docs.racket-lang.org/scribble/reader.html
