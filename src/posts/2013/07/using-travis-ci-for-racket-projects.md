    Title: Using Travis CI for Racket projects
    Date: 2013-07-05T08:14:12
    Tags: Racket

(_Updated 2014-06-06_)

[Travis CI][] is a continuous integration service for open source
projects that has nice integration with [GitHub][].

Whenever you push a commit to GitHub, a build/test can
launch. Notification of the result comes via a variety of
methods. Also there's a "badge" to show the build status, which you
can link to in your `README.md`.


<!-- more -->

For example here's the badge for [Frog][], which hopefully says "build
passing" when you read this:

[![Build Status](https://travis-ci.org/greghendershott/frog.png?branch=master)](https://travis-ci.org/greghendershott/frog)

One way I've seen Travis CI help is with pull requests. When someone
submits a PR via GitHub, Travis runs your tests on it. The OK-or-fail
result is inserted in the comment thread for that pull request on
GitHub. As a result, the contributor gets some feedback, even if they
neglected or forgot to run tests themselves. Ideally, the submittor
notices and fixes before the acceptor even needs to point out the
problem.

I've wanted to use Travis CI on my own projects. Mainly I wanted the
spiffy badge for my `README.md`. Seriously, I wanted the other
benefits, too. Unfortunately Racket is not one of the dozen languages
officially supported by Travis CI.

The proper way to support Racket in Travis CI is, well, the proper
way: Making [Chef cookbooks][]. Hopefully someone[^someone] will do
this.

[^someone]: Translation: I passive-aggressively issue a call-to-action for someone to do this so that I don't have to.

## Meanwhile

In the meantime, here's a work-around.

Add a `.travis.yml` file to the root of your project. The contents
should be something like this:

```yaml
language: racket

env:
 - RACKET_VERSION=6.0
 - RACKET_VERSION=HEAD

before_install:
- git clone https://github.com/greghendershott/travis-racket.git
- cat travis-racket/install-racket.sh | bash # pipe to bash not sh!

install:

before_script:

script:
 - /usr/racket/bin/raco make main.rkt
 - /usr/racket/bin/raco test -x .

after_script:
```

Or simply this:

```yaml
env:
 - RACKET_VERSION=6.0
 - RACKET_VERSION=HEAD

before_install:
- git clone https://github.com/greghendershott/travis-racket.git
- cat travis-racket/install-racket.sh | bash # pipe to bash not sh!

script:
 - /usr/racket/bin/raco make main.rkt
 - /usr/racket/bin/raco test -x .

```

The key points:

* `RACKET_VERSION` is the version of Racket you want to be downloaded,
  installed, and used.

* `RACKET_VERSION` can be specified mulitple times. Travis CI will run
  a build for each version.

* The `script` section says what should be run. For a simple project
  this might consist of running `raco make` on one or more files, and
  running `raco test` on all files. Just `raco test` might be
  sufficient. If you have a `makefile`, it could simply be `make`. You
  get the idea.

### If your repo is for a Racket package

If your repo is for a Racket package, and your `info.rkt` has a `deps`
listing some dependencies, there's a more elegant way to do the
`script:` section. Let's say your repo name is `foo`:

```yaml
script:
- cd ..   # Travis did a cd into the dir. Back up, for the next:
- /usr/racket/bin/raco pkg install --deps search-auto --link foo
- /usr/racket/bin/raco test -x -p foo
```

Using the `--link` option of `raco pkg install` lets you install a
package locally.  The `--deps search-auto` option will automatically
install all the `deps` from your `info.rkt`, and, do so without
asking.

Then, using `-p foo` with `raco test` tells it that `foo` is an
installed package.  (I also use the `-x` flag because I only put tests
inside `(module+ test)` submodules, and _don't_ want `raco test` to
run other code. But if you put tests in top-level modules, you'll want
to omit that.)

## How and why does this work?

In the `before_install` step, Racket is installed by
obtaining[^obtaining] a `install-racket.sh` script and piping it to
`bash`. The `install-racket.sh` script figures out where to download
the Ubuntu 64-bit Racket installer for the specified `RACKET_VERSION`,
uses `curl` to download it, and runs it using the default values.

Below is the current version, as I write this blog post. You can look
at the latest
[here](https://github.com/greghendershott/travis-racket/blob/master/install-racket.sh).

Most of the work here is simply translating a version string like
`5.3.5` or `6.0` or `HEAD` into the location from which it can be
downloaded.

```sh
# IMPORTANT: Your .travis.yml must pipe this to bash (not to sh)!
# In the Travis CI environment a #!/bin/bash shebang here won't help.

set -e

if [[ "$RACKET_VERSION" = "HEAD" ]]; then
    URL="http://www.cs.utah.edu/plt/snapshots/current/installers/racket-current-x86_64-linux-precise.sh"
elif [[ "$RACKET_VERSION" = 5.9* ]]; then
    URL="http://download.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-ubuntu-quantal.sh"
elif [[ "$RACKET_VERSION" = 6.0* ]]; then
    URL="http://download.racket-lang.org/installers/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux-ubuntu-precise.sh"
else
    URL="http://download.racket-lang.org/installers/${RACKET_VERSION}/racket/racket-${RACKET_VERSION}-bin-x86_64-linux-debian-squeeze.sh"
fi

INSTALL="./racket-${RACKET_VERSION}.sh"

echo "Downloading $URL to $INSTALL:"
curl -L -o $INSTALL $URL

echo "Running $INSTALL to install Racket:"
chmod u+rx "$INSTALL"
sudo "$INSTALL" <<EOF
no
/usr/racket

# EOF

exit 0
```

[^obtaining]: The `.travis.yml` needs to get the `install-racket.sh`
    script onto the Travis CI VM in order to run it. Simple? Well, I
    went through a few approaches.

    1. Originally I simply used `curl` to fetch the file from
       `raw.github.com`. Bonus: That automatically gets the latest
       version of `install-racket.sh`. Unfortunately, someone told me
       that GitHub doesn't want people using `raw.github.com` to
       download files. Instead we should use GitHub's "Releases"
       feature.

    2. I used the GitHub "Releases" feature. However this meant
       manually creating a new release every time I had to update
       `install-racket.sh`. Worse, it meant that everyone had to
       update their `.travis.yml` to supply the URL to the new
       release's file. (Although GitHub has a "latest version" URL
       that redirects to the _information page_ for the latest
       release, there is no such URL (that I could find) that
       redirects to the _file to be downloaded_, i.e. what `curl`
       needs).

    3. Finally, I realized that `.travis.yml` could simply `git clone`
       the whole `travis-racket` repo, and use the `install-racket.sh`
       file from it. Like approach 1, this means that `.travis.yml`
       doesn't need to be updated, it will automatically always get
       the latest version of `install-racket.sh`. (Even though
       `travis-racket` isn't a big repo, it seems to me that a full
       `git clone` is more bandwidth for GitHub than the
       `raw.github.com` approach #1 ever was. But it's their system
       architecture and their rules, so I defer to what they request.)

## Caveats and Conclusion

The Ubuntu 64-bit Racket installer is about 75 MB. This is downloaded
and installed every time Travis CI runs your test. Although it's
relatively quick (single digit seconds), it is a bit wasteful of
bandwidth.

Feel free to use this. Maybe have fun understanding how it works.
Perhaps even point out some improvements I could make. However do keep
in mind this is an interim hack rather than the long-term solution.

[Travis CI]: https://travis-ci.org/
[GitHub]: https://github.com/
[Frog]: https://github.com/greghendershott/frog
[Chef cookbooks]: https://github.com/travis-ci/travis-cookbooks
