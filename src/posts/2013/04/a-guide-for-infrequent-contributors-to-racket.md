    Title: A guide for infrequent contributors to Racket
    Date: 2013-04-16T17:46:49
    Tags: Racket, Git

Do the following apply to you?

- Maybe you want to contribute something to [Racket][]: You'd like to
  improve the documentation, or you'd like to add a small feature.

- Maybe you're comfortable with Git, but haven't made a pull request
  before.

- Maybe you've made a one-off pull request, but haven't tried to
  contribute to the same project over time and stay in sync with the
  upstream project.

If so, you may find my guide helpful. I was hopelessly confused about
how to handle the branches and merging. After I figured it out, I
wrote this down in a Gist as a note to my future self. Today I figured
I'd dust it off and make it into a blog post.

<!-- more -->

# One-time fork and setup

Fork [plt/racket](https://github.com/plt/racket) and `git clone` your
fork to your computer.

For more help on this, as well as building Racket locally, see [Joe
Gibbs Politz's excellent blog post][]. Go read it now. I'll wait.

# Getting updates over time

If you haven't already, define a remote for plt/racket:

```sh
git remote add upstream git://github.com/plt/racket.git
```

Because [nobody likes a dirty fork][], get updates _only_ using this
command:

```sh
git pull --ff-only upstream master
```

If you _only_ update this way, it will ensure that your `master`
branch is a _simple, exact mirror_ of the plt/racket `master` branch.

Your `master` won't become cluttered with extraneous merge commits
(like "Merge branch 'master' of github.com:plt/racket").

This makes it easy to create pull requests containing only the
commit that you intend.

> Important: The `--ff-only` flag is the key to this. Don't use `git
> pull` without this flag.

> Note: `git pull --ff-only upstream master` is a shortcut for two
> steps, `git fetch upstream` and `git merge --ff-only
> upstream/master`.

After doing:

```sh
git pull --ff-only upstream master
```

You can push this back your forked repo on GitHub:

```sh
git push origin master
```

## Rebuild all after Racket version change

Eventually the Racket version number will increase and you may end up
with stale `compiled/` subdirectories:

```sh
compiled/reader_rkt.zo::0: read (compiled): wrong version for compiled code
  compiled version: 5.3.1.9
  expected version: 5.3.2.1
```

The usual incantation may be sufficient:

```sh
make && make install
```

To force a rebuild of all collections, also do:

```sh
raco setup -c && raco setup
```

# Contributing something

## Create a topic branch

Never make changes on your `master` branch. Instead, create a
so-called "topic branch", named after your feature:

```sh
git checkout -b my-feature-branch
```

## Hack, hack, hack

Make your changes and commit.

Don't forget [tests and documentation][].

## Prepare your topic branch for the pull request

1. Maybe you like to commit often as you work on a feature. I do. It's
   a form of backup and a paper trail. That's great, but preferably
   your pull request should be _just one commit_. Easier on the
   maintainer. Plus the upstream project doesn't need 20 commits in
   its history for your 1 feature.

2. You should _rebase your topic branch on the upstream master_, to
   catch any changes or conflicts.

You can do both with `git rebase`.

```sh
# First, get the latest upstream master
git checkout master
git pull --ff-only upstream master

# Rebase our topic branch on that
git checkout my-feature-branch
git rebase -i master
  # 1. In the editor, change 'pick' to 'squash' to combine
  #    multiple commits into one.
  # 2. If any conflicts from upstream, resolve them.
```

See [squashing commits with rebase][].

## Push your topic branch to GitHub

To push your topic branch to GitHub:

```sh
git push -f origin my-feature-branch
```

The point of the `-f` (force flag) is in case you had already pushed
this topic branch. After you use `rebase` (above), you'll probably
need to force the push.

> Note: Using `push -f` is usually bad. Here it's OK because we're
> `push -f`-ing to a topic branch in our forked repo that no one has
> pulled from yet.

## Make your pull request on GitHub

Visit your forked repo's page on GitHub.

If you recently pushed your topic branch, you'll see a handy button to
make a pull request for the branch:

![Recent pushed branch](http://i.imgur.com/qVF2l.png)

You can also use the main Pull Request button.

Either way, you get the Pull Request page:

![Pull request page](http://i.imgur.com/WdyV7.png)

The pull request page has three "tabs":

1. _New Pull Request_. This is a title and description. The title
   defaults to your commit message's first line. The description
   defaults to the rest of your commit message. (If you have more than
   commit, you won't have these defaults. But as described above, you
   really shouldn't include more than one commit in your pull
   request.)

2. _Commits_. Preferably this should have the number 1 next to it --
   just one commit in your pull request. You should click the tab and
   double-check that the commit is what you expect.

3. _Files Changed_. Again, double-check to make sure it looks correct.

When all looks good, click __Send Pull Request__.

## Waiting is the hardest part

What next?  You wait for your pull request to be accepted and
merged. Then it will flow back to you when you do `git pull --ff-only
upstream master`, because your commit is now part of the official
plt/racket repo.

But it might take awhile for the pull request to be accepted. It might
_never_ be accepted.

In the meantime, you might be tempted to merge your topic branch into
your own `master`.  __Don't!__

If you were to do that, your `master` would cease being a nice clean
mirror of the upstream master.  Remember how no one likes a dirty
fork, so we're diligently using `git pull --ff-only upstream master`?

So the simplest thing to do is just wait.

But if you really want to use your feature, in your own custom build,
I suggest making some other branch (e.g. `custom-build`). Feel free to
merge from your topic branches (and from upstream master) into _that_
branch, from which you can build your custom variation of Racket.

Just remember that topic branches for pull requests should still
always be based off your `master`, which should be a fast-forward
mirror of upstream.

[Racket]: http://www.racket-lang.org
[Joe Gibbs Politz's excellent blog post]: http://blog.racket-lang.org/2012/11/tutorial-contributing-to-racket.html
[nobody likes a dirty fork]: http://blog.evan.pro/keeping-a-clean-github-fork-part-1
[tests and documentation]: http://blog.racket-lang.org/2012/11/tutorial-contributing-to-racket.html
[squashing commits with rebase]: http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html
