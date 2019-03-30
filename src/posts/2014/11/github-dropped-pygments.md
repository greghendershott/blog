    Title: GitHub dropped Pygments
    Date: 2014-11-17T12:00:00
    Tags: Racket, software

My first-ever open source contribution, a couple years ago, was to a
project called [Pygments]. My motivation? GitHub was displaying
[Racket] source code poorly. Pygments didn't have a Racket lexer.
GitHub was using a Scheme lexer for Racket code. The Scheme lexer was
highlighting square brackets in red as an "error". This was really
distracting and ugly.

[Pygments]: http://www.pygments.org
[Racket]: http://www.racket-lang.org

I contributed a new Racket lexer to Pygments, and waited for that to
roll into a Pygments release and in turn be deployed on GitHub.
Finally Racket code looked good! Later Dave Corbett
[substantially improved] the Racket lexer beyond my small start.

[substantially improved]: https://bitbucket.org/birkenfeld/pygments-main/commits/27846e7885d9f4b1c0084eeb9ed928a13b39a7ff

A few days ago, I was confused to see that Racket code was displaying
poorly again on GitHub. The square brackets were highlighted in red as
errors -- again??

Cartoon-me's thought balloons: WAT, OMFG, FML, &c. Why are we going in
circles?

<!-- more -->

---

Someone [submitted an issue against pygments.rb][141], which is
GitHub's Ruby library to use the Python Pygments library. I commented
on that issue, while it began to dawn on me that GitHub wasn't using
Pygments anymore. Which led to me submitting
[this issue against linguist][1717].

[141]: https://github.com/tmm1/pygments.rb/issues/141
[1717]: https://github.com/github/linguist/issues/1717

The GitHub folks responded quickly, especially during the weekend.
They proposed a reasonable way to at least improve the status quo --
use an existing TextMate lexer.

---

In [issue 1717's][1717] comments, it didn't seem appropriate for me to
editorialize beyond the issue itself. But I can editorialize here on
my blog.

I'm sure GitHub had what they felt were compelling reasons for
abandoning Pygments. My **guesses**:

- pygments.rb had an interesting history of trying to use a Python
  library in Ruby on a high-traffic web site. They had tried various
  approaches. The final approach -- piping to a long-running Python
  process -- seemed to work well. But maybe not. Or maybe the history
  was too much. Sometimes code gets a bad reputation, initially
  deserved, later not, but it can never shake the rep.

- There's probably some rationale related to "synergy" with their Atom
  editor. Something like, "GitHub needs lexers, Atom needs lexers, why
  have two systems?" (Hopefully this does _not_ include the idea that
  Atom will benefit from more/improved lexers in the course of people
  fixing this GitHub "regression". That, in my opinion, would be a
  little too clever-evil.)

- TextMate lexers are available for the top 20 languages. Maybe the
  thinking was, sure there's a long tail, but it will just have to
  sort itself out somehow.

Again, these guesses -- speculation based on my experience with
software projects, but **speculation** nonetheless. I don't have any
insider information. Hopefully at some point GitHub will share their
thinking. So far they haven't, which leads to the next point.

---

Regardless of the reasons, the change was deployed without advance
notice, as far as I can tell. The approach _seemed_ to be, if things
break, people will report it and we'll fix it. In other words, we'll
fix on-demand what people care about, rather than proactively worry
about things maybe no ones cares about.

That is actually a very valid and reasonable approach in some
situations -- particularly older, "legacy" applications with a huge
surface area and user attrition. Although that's not what GitHub seems
like to me on the outside, maybe that's how it feels to them
internally, I don't know.

But in any case, here's the problem. Although people use GitHub to
collaborate[^collaborate], many also use it as a portfolio. It is part
of how people hope to get a job, or persuade developers to try a new
language, or whatever. To suddenly doink the appearance of people's
portfolios is unfortunate.

[^collaborate]: In which case you could argue that, outside of pull
requests, people mostly view the project code locally, so the code
formatting doesn't matter quite so much.

---

In dropping Pygments, I'm sure GitHub realized they'd lose dozens of
lexers that many people have worked to contribute over the years. That
negative must have been outweighed, in their opinion, by positives.

I'm not sure they realized that they're also losing a well-documented,
relatively easy process for _contributing_ lexers. The Pygments
project makes it clear and simple for people who want to add or
improve a lexer. Documentation and help are available. Lexers are
written in simple Python, which is arguably easier than wrangling
plist XML or even JSON.[^plist] There are probably a hundred example
lexers to learn from. And finally, there is a process to review and
vet the lexers, before they reach GitHub production. Well, past-tense:
"reached".

[^plist]: The Racket lexer eventually grew to nearly 1,000 lines of
Pyton. Granted the bulk is lists of keyword and built-ins. Even so, it
seems that converting it to a plist or cson representation is going to
multiply the line count by 3, 5, maybe 10. If such a conversion were
straightforward, perhaps GitHub would have implemented that?

Effectively GitHub has opted to take all this upon themselves, now. If
they do it as well as Pygments, great, but it's a lot of work for them
and a lot of redundancy. On the other hand if they don't do it as
well, it's bad for all the not-top-twenty, not-hip-this-year
languages.

Anyway, that's my first reaction. Maybe this decision makes sense in a
way that I just don't understand yet.
