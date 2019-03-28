    Title: Does your Racket project need a makefile?
    Date: 2014-06-19T14:14:59
    Tags: Racket, racket-cookbook

> **NOTE**: **See my [newer post](/2017/04/racket-makefiles.html)**.

Most of my Racket projects don't use a makefile. Why would they? `raco
make` or `raco setup` suffices.

But a makefile can consistently define some common project tasks. And
it can _really_ help when you want to generate HTML documentation from
Scribble sources, and publish it to GitHub Pages using the automagical
`gh-pages` branch.[^pkg]

[^pkg]: Although overall I much prefer Racket's new package manager, the
old PLaneT system automatically hosts Scribble documentation. When
using the new package manager, this makefile makes it much less
tedious to host the documentation on GitHub.

<!-- more -->

GitHub Pages lets you provide a web site for each of your projects.
You:

1. Create an empty, orphan branch named `gh-pages`.
   [GitHub instructions](https://help.github.com/articles/creating-project-pages-manually).

2. Somehow make `gh-pages` contain just the HTML and other files
required for the web site (but preferably not the source files from
your `master` branch).

3. Somehow keep that up-to-date.

Steps 2 and 3 are awkward. To-date I've used some shell scripts that
felt like duct tape.

Yesterday I discovered Tony Garnock-Jones' [makefile][tonyg-makefile]
for his racket-bitsyntax project. It's brilliant. I promptly stole it,
asked him a question about how it works, and asked if I could blog
about it.

[tonyg-makefile]: https://github.com/tonyg/racket-bitsyntax/blob/master/Makefile

Here is his original makefile:

```make
PACKAGENAME=bitsyntax
COLLECTS=bitsyntax

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf htmldocs

setup:
	raco setup $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

htmldocs:
	raco scribble \
		--html \
		--dest htmldocs \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		bitsyntax/scribblings/bitsyntax.scrbl

pages:
	@(git branch -v | grep -q gh-pages || (echo local gh-pages branch missing; false))
	@echo
	@git branch -av | grep gh-pages
	@echo
	@(echo 'Is the branch up to date? Press enter to continue.'; read dummy)
	git clone -b gh-pages . pages

publish: htmldocs pages
	rm -rf pages/*
	cp -r htmldocs/. pages/.
	(cd pages; git add -A)
	-(cd pages; git commit -m "Update $$(date +%Y%m%d%H%M%S)")
	(cd pages; git push)
	rm -rf pages
```

As you can see, it defines some targets for common actions like
running `raco setup`, doing a local package link and unlink.

The really interesting bits are the `htmldocs`, `pages` and `publish`
targets.

- `htmldocs` simply runs `raco scribble` with the options I can never
remember and always need to look up.

- `pages` assumes that you've already created a `gh-pages` branch,
and... does something really weird. It does a `git clone` of the
`gh-pages` branch to a `pages/` subdirectory. Yes, it makes _another_
Git repo down there.

- `publish`

    - Deletes the _contents_ of that `pages/` subdirectory -- but
      leaving its `.git` folder alone -- and copies the `htmldocs/`
      files into `pages/`. In other words it makes the files in
      `pages/` be an exact copy of `htmldocs/`.

    - Commits all changes (if any), using an "Update _datetime_" commit
      message.

    - Does a `git push`. This is interesting. To what "remote" repo
      does it push? Not GitHub. It pushes to the repo in the parent
      directory -- the one from which it was cloned.

    - Blows away the `pages/` subdirectory.

Voila, the `gh-pages` branch has been updated with the latest HTML
output files, and only those files. Very cool.

## My confusion

When I tried using it, two things confused me:

1. When `publish` finishes, the result has _not_ yet been pushed to
the remote for the main repo -- _nothing has been pushed back to
GitHub_, yet. This confused me at first. I thought "publish" would
mean, "actually push to GitHub".[^push] To do so, there needs to be
one more `git push`, up in the main repo directory, as a new last
step.

[^push]: To be clear, Tony did it this way on purpose. He wanted the
final push to GitHub to be something he did manually, as a
safety-check. I completely understand that. I just expected and want
it to behave differently.

2. If you're using a newer Git, like version 1.9.3, when you do just
`git push` you may see this warning:

    ```
    warning: push.default is unset; its implicit value is changing in
    Git 2.0 from 'matching' to 'simple'. To squelch this message
    and maintain the current behavior after the default changes, use:

      git config --global push.default matching

    To squelch this message and adopt the new behavior now, use:

      git config --global push.default simple

    When push.default is set to 'matching', git will push local branches
    to the remote branches that already exist with the same name.

    In Git 2.0, Git will default to the more conservative 'simple'
    behavior, which only pushes the current branch to the corresponding
    remote branch that 'git pull' uses to update the current branch.

    See 'git help config' and search for 'push.default' for further information.
    (the 'simple' mode was introduced in Git 1.7.11. Use the similar mode
    'current' instead of 'simple' if you sometimes use older versions of Git)
    ```

    To make it go away, you could either:

    - Make the global configuration change it recommends.

    - Be explicit and say `git push origin gh-pages`. I'm leaning
      towards this so that the makefile will work for someone else,
      regardless of their Git config.

Combining these two points, here is the makefile I made to use with my
`#lang rackjure` project:

```make
PACKAGENAME=rackjure
COLLECTS=rackjure
SCRBL=rackjure/manual.scrbl

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf htmldocs

setup:
	raco setup $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

htmldocs: $(SCRBL)
	raco scribble \
		--html \
		--dest htmldocs \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		$(SCRBL)

pages:
	@(git branch -v | grep -q gh-pages || (echo local gh-pages branch missing; false))
	@echo
	@git branch -av | grep gh-pages
	@echo
	@(echo 'Is the branch up to date? Press enter to continue.'; read dummy)
	git clone -b gh-pages . pages

publish: htmldocs pages
	rm -rf pages/*
	cp -r htmldocs/. pages/.
	(cd pages; git add -A)
	-(cd pages; git commit -m "Update $$(date +%Y%m%d%H%M%S)")
	(cd pages; git push origin gh-pages)
	rm -rf pages
	git push origin gh-pages
```

However in case I change it after this blog post, see the latest
version [here][rackjure-makefile].

[rackjure-makefile]: https://github.com/greghendershott/rackjure/blob/master/makefile

Also, FYI, [here's][fear-makefile] a variant I made for _Fear of
Macros_. This one has targets to build both [multi-page] and
[all-in-one] HTML.

[fear-makefile]: https://github.com/greghendershott/fear-of-macros/blob/master/makefile
[multi-page]: http://www.greghendershott.com/fear-of-macros
[all-in-one]: http://www.greghendershott.com/fear-of-macros/all.html

---

Racket doesn't need as much external "infrastructure" as some other
languages. You don't need this for very casual projects. But a
makefile like this, plus perhaps a
[`travis.yml` for Travis-CI][travis], can be nice for some projects.

[travis]: http://www.greghendershott.com/2013/07/using-travis-ci-for-racket-projects.html
