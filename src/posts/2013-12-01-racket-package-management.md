    Title: Racket package management
    Date: 2013-12-02T00:00:00
    Tags: Racket


Racket's new package manager is great. It debuted with Racket
5.3.5. Although officially still in beta, it was already good enough
to use for real projects. Racket developers wanted people to use it
for real projects, to get the experience needed to make it even
better.

Over many months, the Git `HEAD` version of Racket --- what would
become Racket 6 --- gradually introduced a few new and changed features
for package management.

However, you might not want to use the newer features, yet. Not if you
want your package to be usable by people still using Racket 5.3.5 or
5.3.6 --- or usable by other packages that wish to support such
people.

Fortunately, the older features are still supported in Racket 6, and
it's not very difficult or inconvenient to use them. You just need to
know what they are.

<!-- more -->

# Why bother?

Someday it will be easy to say, "Hey, my package only supports Racket
6 or newer". As I type this, it would be a bit strange to say that,
because Racket 6 isn't released yet. Even after Racket 6 ships,
there's an argument to be made for supporting folks still using the
previous version of Racket.

Admittedly each package has different requirements. Ultimately it's
your decision.

# How to deploy a package that works with Racket 5.3.x

There are two areas to be aware of: Your `info.rkt`, and the package
source you list on <http://pkg.racket-lang.org>. Let's look at each.

## info.rkt

Here's an example of my `info.rkt` file for Frog, which is compatible
with 5.3.5, 5.3.6, and 6.

```racket
#lang setup/infotab
(define version "0.11")
(define collection 'multi)
(define deps '(("markdown" "0.9")
               "rackjure"
               "find-parent-dir"))
```

The key points:

1. Use `#lang setup/infotab` (_not_ `#lang info`).

2. Use `(define collection 'multi)` (_not_ a single collection package).

    This means you need to put your collection in a subdirectory of
    your package's root directory. If your package is named `foo`,
    you'll have:
    
    ```
    foo/
      .gitignore
      info.rkt  # for your package
      README.md
      foo/
        info.rkt  # for the collection (if needed)
        main.rkt
        ... other source files ...
    ```
    
    Is this a bit of a pain in the butt? Yes. In fact I was one of the
    people who asked to have single-collection packages as the new,
    simpler default. I'm happy it was added. I just can't use it,
    quite yet. Not until I'm ready to drop support for older versions
    of Racket.

3. To state a specific version of a package you depend on, use
`("package" "version")` (_not_ the `#:version` keyword).

    Unfortunately even using the old form, `raco pkg update
    --auto-deps <your-package>` in 5.3.x won't work for your package.
    It will error when trying to remove a package it thinks is called
    `name version` instead of just `name`. As a result, you need to
    tell users that, to update, they should use `raco pkg remove
    <your-package>` followed by `raco pkg install <your-package>`. The
    install _will_ handle and honor the version requirement, which is
    the most important thing you want to work.
    
    Remember, if you _were_ to use `#:version`, 5.3.x users couldn't
    even install your package in the first place. Instead, by sticking
    with the old way of specifying the dependency version, at least
    people can install your package. Then to update it, they simply
    need to remove then install again. It's two steps instead of one
    "update". Not a huge inconvenience.

## Package source

When you list your package on <http://pkg.racket-lang.org>, you should
use the old form for GitHub URLs:

    github://github.com/<user>/<repo>/<rev>[/<path>]

For example:

    github://github.com/greghendershott/frog/master
    
Racket 5.3.5 and 5.3.6 do _not_ recognize the new `git:` variant.

# Conclusion

As you can see, it's not difficult to make a package compatible with
the `raco pkg` in 5.3.5 and 5.3.6, as well as the upcoming version 6
of Racket. Once you know how, it's easy.

If you have any additional tips, or corrections, please comment below.
