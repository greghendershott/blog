    Title: Why macros?
    Date: 2014-10-21T13:56:54
    Tags: Racket, Clojure, Hacker School

Yesterday a couple people asked me, "How and why do you use macros in
a Lisp like Racket or Clojure?".

I gave answers like:

- The compiler can do a search-and-replace on your code.

- You can make DSLs.

- They're an "API for the compiler".

Although all true, I wasn't sure I was getting the full idea across.

<!-- more -->

Worse, one time Peter Seibel was within earshot. Although I don't know
if he heard my explanation, I imagined him biting his tongue and
politely remembering the "well, actually" rule. :)

Later I remembered Matthias Felleisen boiling down macros into three
main categories:

1. **Binding forms**. You can make your own syntax for binding values
to identifiers, including function definition forms. You may hear people
say, in a Lisp you don't have to wait for the language designers to
add a feature (like `lambda` for Java?). Using macros you can add it
yourself. Binding forms is one example.

2. **Changing order of evaluation**. Something like `or` or `if` can't
really be a function, because you want it to "short-circuit" -- if the
first test evaluates to true, don't evaluate the other test at all.

3. **Abstractions like domain specific langagues (DSLs)**. You want to
provide a special language, which is simpler and/or more task-specific
than the full/raw Lisp you're using. This DSL might be for users of
your software, and/or it might be something that you use to help
implement parts of your own program.

Every macro is doing one of those three things. Only macros can really
do the first two, at all[^order]. Macros let you do the last one more
elegantly.

I think the preceding is a better answer. However, maybe it's still
not the best way to get people from zero to sixty on, "Why
macros?".[^fear]

Maybe the ideal is a "teachable moment" -- facing a problem that
macrology would solve.[^learning] That's also good because you really
really _really_ don't want to use a macro when a normal function would
suffice. So the goal isn't to get people _so_ enthusiastic about
macros that they go forth in search of nails to which to apply that
new hammer. Macros often aren't the right approach. But once in a
while, they are the bestest approach ever.

[^order]: A language like Haskell can choose lazy evaluation, and
implement `if` as a function. I'm saying that only a macro can futz
with whatever the default evaluation order is, be it eager or lazy.

[^learning]: Certainly that's my own optimal learning situation, as
opposed to getting answers or solutions before I have the questions or
problems.

[^fear]: Although I wrote a guide called
[Fear of Macros](http://www.greghendershott.com/fear-of-macros/), it's
(a) specific to Racket macros and (b) much more about the "how" than
the "why".

