    Title: Hacker school week 6
    Date: 2014-11-14T13:42:14
    Tags: Hacker School

I'm at the end of week 6 at Hacker School, which marks the halfway
point. There are overlapping 12-week batches. As a result, the
previous batch never-graduated yesterday.

My early weeks here included _some_ pairing, and it was fun, but I
spent more time learning and coding solo. My previous week was nearly
the opposite. I spent most of my time pairing with Sumana
Harihareswara.

<!-- more -->

Her project idea was a web service that would help people find
vulnerabilities in C code, and return a report card itemizing the
issues and giving a score[^1].

[^1]: Silicon Valley has its "Weisman Score". Well _this_ is a
"Hendershott-Harihareswara Score". Really rolls off the tongue,
doesn't it?

Writing a static code analyzer from scratch seemed like it would be
too ambitious for the time available. So for version 0.1 we decided to
have our web service wrap existing tools like
[Clang Static Analyzer](http://clang-analyzer.llvm.org/) and
[Cppcheck](http://cppcheck.sourceforge.net/). Someone can use the
analyzers without needing to install them.

We installed and got familiar with `clang`'s `scan-build`. I wrote
some C code with deliberate problems, and we saw what was reported.

For our web server, we decided to make something using Python. We
started to look at our choices for frameworks, like Django, Flask, and
several dozen others. But then we realized, hey -- our web service is
really simple. It has one resource. A request will be:

```http
POST /api/analyze HTTP/1.1
Content-Length: 42

/* my awful C code */
main () {
}
```

Our response body will be `scan-build` output parsed into some sort of
JSON.

Do we need any framework at all?

We decided no. We derived from `BaseHTTPServer`, overriding `do_POST`.
This became a great opportunity to look at how HTTP requests and
responses work.

Of course, if our web service were to grow, we would find ourselves
copying and pasting a bunch of code. We would DRY it into some helper
functions. One day we'd say, "Let's share this with the world!". And
voila, the world would have another framework.

Or better yet, we'd say, "OK, now let's use some existing framework.
Now we know what problems it's solving for us. Now we have a
reasonable mental model for how it probably works; it's not some
magical black box."

When it came time to parse the `scan-build` output into JSON, I found
it quicker to write that in Racket than in Python. Which was fine,
because that was something we needed done, as opposed to something we
wanted to learn about.

At this point we had a reasonably well-working application that ran...
on our laptops. We wanted to deploy it on a Digital Ocean droplet.
Putting one web service on one droplet isn't difficult. But we opted
to pretend that our web service would become wildly popular and we'd
need to scale it across multiple machines. A couple people had asked
if we were going to try Docker. We did.

It took us awhile to wrap our head around the concepts. Eventually we
understood that an _image_ is a read-only thing created from a
`Dockerfile`. It is built from a base image, e.g. Ubuntu, plus
additional packages that you install, and other programs that you run
and settings that you configure. Thereafter you launch _containers_
for an image. These have a read/write layer and are a kind of
lightweight VM in which your application runs. You can `docker run -d
<image>` to detach these and they run in the background; you `docker
logs <container>` to see their output. You can `docker stop
<container>` to stop one. After stopped it still exists. You can see
all these with `docker ps -a`, or just the running ones with `docker
ps`. You can delete a stopped container with `docker rm <container>`.

Therefore we wanted to create an image based on Ubuntu, install some
packages like `git` and Racket 6.1, and finally do a `git clone` of
our app code.

A couple issues that slowed us down briefly:

1. `apt-get` wouldn't work until we changed `/etc/default/docker/` to
   un-comment the line saying to use Google's DNS servers.

2. How to set an environment variable in our `Dockerfile`, that would
   survive into the container's environment. Doing `RUN export
   PATH=$PATH:/new/path` did not work. Turned out we needed `ENV`, as
   in `ENV PATH $PATH:/new/path`.

In the end we got things working.

The final day we made a simple web _site_ to go along with our web
_service_. So as a human you can visit a page, type C code in a form
or do a file upload, and get the report card on a web page rather than
as raw JSON. Although we didn't learn a whole lot by doing this part,
it was a satisfying way to wrap up the project.

I enjoyed the project because I got a chance to:

- Try more Python coding in a week than I've done, ever.

- Understand Docker hands-on.

- Exercise developer teamwork skills.

- Validate my understanding of HTTP mechanics by explaining them
  effectively.

For my second half of Hacker School, I'm looking forward to doing more
pairing. After all, I can study things solo anytime. Now is when I
have so many great opporunities to pair.
