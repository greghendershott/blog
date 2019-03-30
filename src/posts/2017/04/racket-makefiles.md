    Title: Racket Makefiles
    Date: 2017-04-18T12:00:00
    Tags: Racket

A few years
ago
[I wrote about makefiles for Racket](/2014/06/does-your-racket-project-need-a-makefile.html).
Some things have changed.

1. The old makefile built and pushed documentation to a GitHub Pages
   branch of the repo. That's no longer necessary: The Racket package
   catalog builds and hosts documentation.

2. The Racket package catalog puts a yellow badge of shame on packages
   with missing dependencies (`deps` and `build-deps` in the package's
   `info.rkt`). I want the makefile to check this.

3. In `.travis.yml` files for Travis CI, I think the `script` section
   ought to simply invoke targets in the makefile -- delegating
   details to the latter.

4. Likewise some details needn't even be in the makefile -- they can
   move to the collection's `info.rkt`. Example: The list of
   directories to `clean`.
   
5. The old makefile had separate `PACKAGENAME` and `COLLECTS`
   variables; for single-collection packages they were the same value.
   I wanted to simplify this to just the package name and use the
   appropriate package variants of `raco` commands.

In that spirit, here's an updated Makefile, which I recently started
using in the [rackjure], [markdown], and [frog] projects.

[rackjure]: https://github.com/greghendershott/rackjure
[markdown]: https://github.com/greghendershott/markdown
[frog]: https://github.com/greghendershott/frog

<!-- more -->

```make
PACKAGE-NAME=rackjure

# Racket 6.1 adds pkg dep checking.
ifeq ($(findstring "$(RACKET_VERSION)", "6.0", "6.0.1"),)
	DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps
else
	DEPS-FLAGS=
endif

all: setup

# Primarily for use by CI.
# Installs dependencies as well as linking this as a package.
install:
	raco pkg install --deps search-auto

remove:
	raco pkg remove $(PACKAGE-NAME)

# Primarily for day-to-day dev.
# Note: Also builds docs (if any) and checks deps.
setup:
	raco setup --tidy $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)

# Note: Each collection's info.rkt can say what to clean, for example
# (define clean '("compiled" "doc" "doc/<collect>")) to clean
# generated docs, too.
clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)

# Primarily for use by CI, after make install -- since that already
# does the equivalent of make setup, this tries to do as little as
# possible except checking deps.
check-deps:
	raco setup --no-docs $(DEPS-FLAGS) $(PACKAGE-NAME)

# Suitable for both day-to-day dev and CI
test:
	raco test -x -p $(PACKAGE-NAME)
```

The two main scenarios here:

- Day-to-day development: `make setup` and `make test`.

- CI: `make install`, `make check-deps`, and `make test`.

I think you could probably use this as a template for any
single-collection package project. Just change `PACKAGE-NAME`.
Possibly append a target or two for something unique to your project.

---

This Makefile is designed to work with Racket 6.0 or newer -- because
I have some existing packages that support Rackets that old. If you
only care about Racket 6.1 or newer, then all this:

```make
# Racket 6.1 adds pkg dep checking.
ifeq ($(findstring "$(RACKET_VERSION)", "6.0", "6.0.1"),)
	DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps
else
	DEPS-FLAGS=
endif
```

can become just this:

```make
DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps
```

---

By the way, I wouldn't call myself a very experienced user of `make`.

For example the way I check for Racket < 6.1 seems smelly -- but it
was the least-worst way I could figure out.

So please feel free to share any suggestions or corrections in the
comments.
