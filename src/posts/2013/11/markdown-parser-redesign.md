    Title: Markdown parser redesign
    Date: 2013-11-06T15:53:28
    Tags: Racket

Whew. About 2 weeks and 86 commits later, I have a branch I'm almost
ready to merge to master: a completely redesigned [markdown parser][]
using [Parsack][].

At RacketCon when I gave my short talk about [Frog][], I had a slide:

> "How hard can it be to..."
>
> -- _Me, too often_.

<!-- more -->

When I decided to create Frog, I thought, how hard can it be to write
a static blog generator? It turned out, actually not that hard. For
the blog generator per se. But I also needed a markdown parser. I
wanted one written in Racket, to simplify installation of Frog. None
existed. So I had to write one.

It turned out _that_ was hard.

I started by looking at the original [Markdown.pl][], which uses
regular expressions. I took that approach. However, I wanted to parse
to `xexpr`s, not to an HTML `string`. That significantly complicated
things. Whereas Markdown.pl can be a series of `string? -> string?`
transformations, I had to handle mixes of elements already parsed to
`xexpr`s, and `string`s. As a result, I ended up writing a somewhat
half-arsed thing that was like `regexp-match`, but could handle
`xexprs`. Sort of. It worked, but with a lot of layers of duct tape.

On the plus side, I accumulated nearly 100 unit tests for various
markdown corner cases. I became very good at finding corner cases I'd
got wrong. And fixing them, one by one.

But still. I felt embarrassed --- this was the Racket code of which I
was the least proud. Another slide at RacketCon was this:

![You're parsing markdown with regexps? Good.](/img/grumpy-regexp-parser.png)

Shortly after RacketCon I went to Japan, and had no time for hardcore
coding. But when I got back, I decided to tackle writing a "real"
parser.

I wasn't optimistic I'd succeed. I'd always felt intimidated by
this. I never took a "compilers" CS class. When I added CAL, a
scripting language, to the Cakewalk sequencer software, I chose an
s-expression syntax. Not because of a love for lisp, back then. But
because even _I_ could parse s-expressions.

It also seemed clear that markdown isn't a context-free grammar that's
amenable to a traditional lex/yacc tool. Instead, something like a PEG
parser seemed a better bet.

I discovered the PEG grammar for [peg-markdown][]. That encouraged me.

I wanted to use the Racket PEG parser on Planet, but it looked like it
hadn't been updated in a few years, and its docs implied it could be
slow.

Then I happened to discover Stephen Chang's [Parsack][] -- a port from
Haskell to Racket of much of the [Parsec][] monadic parser
combinator. I tried using it, and it seemed to "click" for me and make
sense.  I experimented with some of the PEG markdown grammar and got
reasonable results. But I got completely stuck trying to implement
lists (bullet and ordered lists, which can recursively contain other
lists). And at times it wasn't clear how to translate a PEG Frisby-ism
to a Parsec-ism. Then Stephen pointed out that [Pandoc][] uses Parsec,
so I should check out its markdown grammar. I did.

At that point I had a mishmash of ideas from peg-markdown, from pandoc,
and from my own hacking. And plenty of bugs and confusion. But it was
working well _enough_ that I felt confident to dig in and slog it out.

In the last few days the pieces have come together quickly. When I've
found an edge case bug, or need to add a small feature, doing so has
been clear and concise. That's usually a good sign.

# Grammar and Parsack examples

Markdown consists of zero or more blank lines, zero or more _block_
elements, and zero or more blank lines:

```racket
(define $markdown
  (parser-one (many $blank-line)
              (~> (many $block))
              (many $blank-line)
              $eof))
```

The `many` parser means "zero or more" (like `*` in regular
expressions). There's also `many1` (like `+`) meaning "one or more".

The `parser-one` form in Parsack is a convenience that means, apply
each parser in sequence, but I only care about the result of one,
which is in a `~>` form. In this case, we don't care about the leading
or trailing blank lines, just the blocks between. We return just that.

What is `$block`?  It's a parser defined using Parsack's `<or>`
parser, which tries each of the parsers stopping with the first that
succeeds.

```racket
(define $block
  (<?> (<or> $blockquote
             $verbatim
             $footnote-def
             $reference
             $html/block
             $heading
             $list
             $hr
             $para
             $plain)
       "block"))
```

The `<?>` parser simply lets you provide more-concise error messages;
in this case, "expected a block".

So that's the view from orbit: A markdown file is a series of `$block`
elements. One of the simplest is `$para` (a paragraph), which is one
or more `$inline` elements, a newline, and one or more blank lines:

```racket
(define $para
  (try (parser-compose (xs <- (many1 $inline))
                       $newline
                       (many1 $blank-line)
                       (return `(p () ,@xs)))))
```

The `try` parser wraps another parser with backtracking -- if the
enclosed parser fails, input is reset to the original point.

Instead of using `parser-one`, the main parser here is defined using
`parser-compose`, which is the most general form. You can assign
parser results to variables like `xs`, and combine them to `return`
something. This corresponds to the `do` notation in Haskell; after
all, this is a monadic parser combinator.

As does `$para`, most `$block` elements consist of `$inline` elements:

```racket
(define $inline
  (<?> (<or> $str
             $smart-punctuation
             $whitespace
             $end-line
             $code
             $strong
             $emph
             $footnote-ref
             $link
             $image
             $autolink
             $html/inline
             $entity
             $special)
       "inline"))
```

The simplest of these is `$str`, which is one or more "normal" characters:

```racket
(define $str
  (try (>>= (many1 $normal-char)
            (compose1 return list->string))))
```

Using `>>=` -- the fundamental combinator -- is just for brevity here.
You could also write it using `parser-compose`, which will expand to
the equivalent using `>>=`:

```racket
(define $str
  (try (parser-compose (xs <- (many1 $normal-char))
                       (return (list->string xs)))))
```

Either way, because `many1` returns a `(listof char?)`, we convert
that to a `string?`. This is one nuance in Parsack that, if I
understand correctly, is moot for Parsec. In Haskell, a list of
characters _is_ a string. But in Racket, `(listof char?)` and `string`
are distinct.  As a result, when you write a grammar in Parsack, you
might want some of your combinators to chunk `(listof char?)` up into
`string` for the convenience of "higher level" combinators. You just
need to decide and remember where the level boundary is.

Finally, here's a less-trival `$inline`:

```racket
(define $strong
  (parser-compose
    (xs <- (<or> (enclosed (string "**") (try (string "**")) $inline)
                 (enclosed (string "__") (try (string "__")) $inline)))
    (return `(strong () ,@xs))))
```

This says that HTML `<strong>` is notated with double asterisks or
underlines enclosing one or more `$inline` elements. So here we start
to see the recursion where an `$inline` consists of one or more other
`$inline`s.

What is `enclosed`? It's a helper combinator:

```racket
(define (enclosed open close p) ;; parser? parser? parser? -> (listof any/c)
  (try (parser-compose open
                       (notFollowedBy $space)
                       (xs <- (many1Till p close))
                       (return xs))))
```

The `notFollowedBy` combinator, provided by Parsack, does a negative
look-ahead. The Frisby PEG equivalent is `doesNotMatch`.  For reasons
of elegance and efficiency, it is usually preferable to avoid it ---
better to say what you _want_, rather than what you _don't_ want. But
for grammars like markdown, there a few times when it's incredibly
useful to have this. Maybe even necessary.[^1]

The full grammar goes one for about 900 lines of code. Although some
of it is tricky to get just right, much of it is fairly
straightforward. Although not as concise as the (sort of) equivalent
regular expressions, there is such a thing as too much concision
(sorry, APL). The Parsack grammar is more readable. More importantly,
it's more maintainable. Your brain's usual Racket-parsing wetware can
see the structure due to grouping cues like parens and indents. So can
your text editor. As a result, you're more likely change it correctly
than the equivalent regexp. IMHO.

# Eat your own dog food, then share

In conclusion, I should mention that the HTML you're reading now was
created from markdown by Frog using the redesigned parser.

As I type this, I'm using my own build from the topic branch. I
haven't yet merged the branch to master. It's a big change, so,
measure twice, cut once. But I plan to do it soon. Although I'm sure
some new bugs will pop up, I have at least one recently-reported bug
that the new design already did not exhibit in the first place.


[markdown parser]: https://github.com/greghendershott/markdown
[Parsack]: https://github.com/stchang/parsack
[Frog]: https://github.com/greghendershott/frog
[Markdown.pl]: http://daringfireball.net/projects/markdown/
[peg-markdown]: https://github.com/jgm/peg-markdown
[Parsec]: https://research.microsoft.com/en-us/um/people/daan/download/parsec/parsec.html
[Pandoc]: https://github.com/jgm/pandoc

[^1]: I'm about 3 weeks into using parser combinators. A thoughtful discussion of `notFollowedBy` is above my current pay grade. Feel free to leave a comment if you know more about this, or have an interesting link.
