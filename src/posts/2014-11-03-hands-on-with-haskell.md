    Title: Hands-on with Haskell week 1
    Date: 2014-11-03T10:51:21
    Tags: Haskell, Hacker School

Last week I decided to pivot from Clojure hands-on to Haskell
hands-on.

<!-- more -->

# Rewiring brain from Lisp to Haskell

Prior to this, I had spent some time working through examples in
[Learn You a Haskell][learn-you] and armchair browsing
[Real World Haskell][real-world]. Also I'd spent some time using Typed
Racket and was already sold on the benefits of having a "real" type
system, especially for certain sorts of programs. So this wasn't
starting from scratch. However I soon discovered there were some
things about Haskell that I'd forgotten and needed to re-learn. Also I
suffered from a Lisp brain configuration.

[learn-you]: http://learnyouahaskell.com/
[real-world]: http://book.realworldhaskell.org/

## Sections

One example was sections. On Zulip I saw a code fragment like this:

```hs
primes = 2 : [i | i <- [3..], and [rem i p > 0 | p <- takeWhile ((<=i).(^2)) primes]]
```

With the comment that the `(<= i)` was equivalent to `(\j -> (j <= i))`.

Wait. Why not `(\j -> (i <= j))`?

The answer is that a section behaves differently when used with an
infix operator. The argument is applied on the missing side. In other
words `(/ 10)` and `(10 /)` are different.

So `(< 1) 2` is `False` but `(1 <) 2` is `True`. And `(compare 1) 2`
is `True`.

In Haskell it's _not_ the case that `(< 1)` is `1` partially applied
to the `<` function, waiting to take a "right" argument.[^also]

[^also]: It's also not the case in Racket or Clojure. You'd have to
say `(curry < 1)` or `(partial < 1)`. But _if_ those languages did
automatic partial application, it would look like `(< 1)`. At least
that's what my Lisp brain misunderstood at first.

## Commas

In working through [Learn You a Haskell][learn-you] I wanted to make
an example input to try with `nub`. I wanted a long list containing
duplicates, that I didn't have to type in by hand. To do so I wanted
to do a Racket `flatten` of a Haskell `replicate` of a short list. It
seemed like `foldr1` with `++` would be the way to do the `flatten`.
So I tried something like:

```hs
*Main> foldr1 ++ (replicate 5 [1 2 3 4 5])

<interactive>:107:1:
    Couldn't match expected type `[a0]'
                with actual type `(a1 -> a1 -> a1) -> [a1] -> a1'
    In the first argument of `(++)', namely `foldr1'
    In the expression: foldr1 ++ (replicate 5 [1 2 3 4 5])
    In an equation for `it': it = foldr1 ++ (replicate 5 [1 2 3 4 5])
```

Oops. Let's try something simpler, first.


```hs
*Main> foldr1 ++ [[1][2]]

<interactive>:245:1:
    Couldn't match expected type `[a0]'
                with actual type `(a1 -> a1 -> a1) -> [a1] -> a1'
    In the first argument of `(++)', namely `foldr1'
    In the expression: foldr1 ++ [[1] [2]]
    In an equation for `it': it = foldr1 ++ [[1] [2]]
```

Huh. Oh right. The `++` is infix. Needs to go in parens. Try again:

```hs
*Main> foldr1 (++) [[1][2]]

<interactive>:246:14:
    The function `[1]' is applied to one argument,
    but its type `[t0]' has none
    In the expression: [1] [2]
    In the second argument of `foldr1', namely `[[1] [2]]'
    In the expression: foldr1 (++) [[1] [2]]
```

Bzzzt. Tried to decipher the error message. Finally remembered, dang,
I need to separate list items with commas:

```hs
*Main> foldr1 (++) [[1],[2]]
[1,2]
```

OK. And building back up again to what I'd wanted originally:

```hs
*Main> replicate 5 [1,2,3,4,5]
[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]
*Main> foldr1 (++) (replicate 5 [1,2,3,4,5])
[1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5]
*Main> nub $ foldr1 (++) (replicate 5 [1,2,3,4,5])
[1,2,3,4,5]
```

In Lisps I've grown accustomed to -- and really love -- being able to
separate list items with whitespace. Needing to use commas feels a bit
weird. Also it's a bit weird that you only use them for things like
lists and tuples, but not in function applications. Oh well.

## Other tiny things

- I keep making the mistake of typing `data MyType = _` instead of
  `data MyType = MyType _`.
  
- Racket `struct`s define accessor functions prefixed with the name of
  the struct. So `(struct person (name))` defines a `person-name`
  accessor function. In Haskell, `data Person = Person {name ::
  String}` defines an accessor `name` -- no prefix. Problem being,
  what if you have a couple types that have fields called `name`? I
  guess you have to do prefix the field names explicitly yourself,
  e.g. `data Person = Person {personName :: String}`?

None of these observations are intended as a critique of Haskell. It's
an observation how the tiny things can trip you up when learning a new
language. Even after a couple days, I've developed a sort of
subconscious checklist of rookie mistakes to consider before trying
to understand the Haskell error message.

# Nice things

- I like how creating a simple Haskell "project" (just for local use)
  is as simple as creating a `.hs` file. I also like this in Racket.

- I like having an actual, mature module system with clear best
  practices (as in Racket).

# My first hands-on with Cabal

I used Cabal for the first time, to install Pandoc. This took...
awhile. Finally I got a weird version error:

```
Configuring pandoc-1.13.1...
setup: At least the following dependencies are missing:
http-client >=0.3.2 && <0.4 && ==0.4.2.2
Updating documentation index /Users/greg/Library/Haskell/doc/index.html
cabal: Error: some packages failed to install:
pandoc-1.13.1 failed during the configure step. The exception was:
ExitFailure 1
```

After puzzling this over for awhile, and reading up on Cabal
versioning, I didn't have any great ideas. Finally I decided to try
running `cabal install pandoc` a second time. And this time, it
worked. Shrug. Onward.

# Porting wffi to Haskell

So after some "warm-up" I decided to dive in and try to port wffi to
Haskell. This turned out to be a good-sized project when I was
hands-on with Clojure -- not too small, not too big. Implementing it
requires parsing markdown, parsing HTTP request templates, making HTTP
requests, using higher-order functions, and so on.

I forged ahead and got the wffi port of Haskell working to the point
that I could successfully make a request to
<http://horseebooksipsum.com/>. I had a little confusion at first
about how `HTTP.Network.simpleHTTP` returns `IO (Either String
String)`. Basically, I didn't anticipate where the IO vs. pure
"boundary" would need to be in my program. But once I realized that,
it made sense and was easy to sort out.

I pushed my commits to a GitHub public [repo], even though I'm sure
the code smells pretty badly. My normal instinct is to spend more time
with code -- beyond the "it's amazing that the dancing bear dances at
all" stage -- before pushing to public. But I'm at Hacker School, this
isn't professional coding, and I shouldn't be so cautious. In fact, by
pushing early, I could get some feedback.

[repo]: https://github.com/greghendershott/haskell-wffi

## Surprisingly easy

I was surprised by how quickly the port to Haskell went, compared to
the port to Clojure. I would have guessed the opposite, because
Clojure seems like it would be "closer to" Racket. I think there are
two reasons why it was faster.

One is that I hadn't worked on wffi for a year, so part of the Clojure
port was actually reminding myself how things worked. In other words
I've been "practicing" porting wffi, and the Haskell port got the
benefit of that.

But also, I simply found Haskell easier to use than I expected. Sure,
I frequently alternated between 10 minutes of smooth sailing and 5
minutes of trying to decipher a type error message. However, this felt
different from being stalled on a Clojure build/environment/package
issue. The Haskell type error messages felt like they turned out to be
useful information about my reasoning about my program. Resolving them
was sometimes a matter of fixing a simple mistake. Sometimes it
entailed fixing a problem with my thinking and my design.[^TR]

My early days with Haskell have been more satisfying than those with
Clojure. Whether that continues remains to be seen. So far so good.

[^TR]: This is an experience I also had with Typed Racket.
