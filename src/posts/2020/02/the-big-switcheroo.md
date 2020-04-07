    Title: The Big Switcheroo
    Date: 2020-02-24T00:00:00Z
    Tags: Racket, Emacs

In my [previous post] I talked about how Racket Mode now will often
want the back end command server, before actually needing a live REPL
-- but it has to start a REPL anyway. This was bothering me. So I went
on to address that. Doing so entailed reversing the I/O model for the
back end. As a bonus, that set up fairly short strokes to supporting
multiple REPLs.

[previous post]: /2020/02/using-drracket-check-syntax-in-racket-mode.html

<!-- more -->

In the beginning, Racket Mode was a thin wrapper around `xrepl`. As a
result, it used a `comint-mode` buffer to start Racket, and the REPL
stdin/stdout was connected to the `comint-mode` buffer. The back end
also started a little TCP server; connecting to that established the
I/O for commands.

This was mostly fine, especially when `racket-run` tended to be the
first command you would use.

However it might take a second or two for the TCP server to be ready
to accept connections. As a result, the front end either needed to
block (boo!) or use an Emacs timer to retry at, say, one second
intervals until success. That meant some extra complexity. More
basically, it meant more delay until a command request and response
could occur.
  
# Flip

So I decided to flip that around. Now, the stdin/stdout of the back
end process is the command request/response channel. The same format
of sexpr command requests and responses are sent -- they just happen
to go over ports that are stdin/stdout instead of TCP ports. The TCP
server is for creating a REPL session, if/when that is desired. Make a
TCP connection, read a session ID, et voila you have a new REPL
session. The back end `parameterize`s the REPL's `current-{input
output error}-mode` to the TCP ports. And fortunately, `comint-mode`
makes it as easy to connect the buffer to I/O from a TCP connection,
as to stdin/stdout of a process.

The other thing is, a process can have only one stdin/stdout. But of
course it can accept multiple TCP connections. Now that we use the
latter for REPL I/O, this removes a basic barrier to an old feature
request: Supporting multiple REPLs would now be possible.

Of course "possible" is different than "implemented and working
correctly". So at first I focused mainly on the back end: Make sure
that it wasn't using global variables when it could use per-thread
parameters, for things related to the REPL. Add a lookup table from
unique REPL session IDs to information about each REPL session. Have
the front end obtain the session ID and supply it later with commands
-- but otherwise, for now, the front end still just created one REPL
session at a time.

# Multiple REPLs

After some time to get that working, I wanted to tackle supporting
multiple REPLs in the front end. For a day or two, I procrastinated. I
_really_ did not want to write a bunch of "ceremonial" code related to
REPL sessions. I did not want some whole UI for that, complete with
more code and bugs.

I'm glad I hesitated, because I realized something simple. Emacs
already has the concept that variable values can be global to all
buffers -- but also can be set so that a buffer has its own, local
value.

So, assume a variable `racket-repl-buffer-name`. It only has meaning
for a `racket-mode` buffer. It means, "what is the name of the
`racket-repl-mode` buffer that commands like `racket-run` should use".
Initially such a variable could have a global value, `*Racket REPL*`.
That default is the status quo behavior: All edit buffers take turns
sharing the same, one and only REPL buffer.

Now, what if a `racket-mode` buffer for `foo.rkt` did a `(setq-local
racket-repl-buffer-name "*Racket REPL foo.rkt*")`? Now commands issued
with that buffer selected would use (creating if necessary) a REPL
buffer of _that_ name. If every edit buffer did this, you'd have one
REPL per edit buffer (much like DrRacket).

You could also imagine wanting edit buffers for files that are in the
same `projectile` project, to share a REPL buffer: "One REPL per
project". OK: `setq-local` their `racket-repl-buffer-name` to some
common name that includes the project name or root directory.

And so that is the mechanism I went with. A customization variable
`racket-repl-buffer-name-function` points to a function that will set
the `racket-repl-buffer-name` variable value for a `racket-mode` edit
buffer when it is created. I defined three such functions, for the
three strategies described above. And best of all, as a user you can
supply your own such function.

I also added a `kill-buffer-hook`: When you kill a `racket-mode` edit
buffer, and it is the last one using a `racket-repl-mode` buffer, we
`y-or-n-prompt` offering to kill the REPL buffer, too. I figure that
will help keep things cleaner, especially for the case where people
want 1:1 edit:REPL buffers.

As a result, there is no Big UI about "sessions". There are just edit
buffers, and names of the REPL buffer(s) each should use. Because it
rides directly on Emacs semantics for variables and buffers, there is
non-trivial code that I did not need to write -- and "out of sync"
bugs that I could not create.

# That cleaving feeling

Of course there are _some_ bugs lurking, about which I don't yet know.
Someday I'll probably feel discouraged about this code, for reasons I
don't yet know. A lot of programming "innovation" amounts to trading
an old set of problems for a new set. Sometimes the new problems
aren't better, they just feel better, at first, because novelty.

All stipulated and admitted. Even so, I _also_ feel this was one of
those situations where, finally, I saw a way to define a problem so
that it aligns with the material I had to work with: Just a few gentle
taps, and the material cleaves along the lines I need and want. That
is a pretty awesome feeling. While it lasts.

# Availability

The stuff I'm describing in this blog post initially lived on a
`new-comm` branch. I did just merge it back to the `check-syntax`
branch. It's _not_ yet merged to `master`. I'm still reviewing
documentation, dogfooding, and such. I'm coming up on two full months
working on this, so, I'd like to merge to `master` in the near future.
