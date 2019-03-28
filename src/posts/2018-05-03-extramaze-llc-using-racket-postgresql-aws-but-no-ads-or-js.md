    Title: Extramaze LLC: Using Racket, PostgreSQL, AWS (but no ads or JS)
    Date: 2018-05-04T00:00:00
    Tags: Racket, Extramaze

For Extramaze LLC I'm using Racket in a commercial project --- a search
engine with email alerts for deals on music gear ---
[deals.extramaze.com](https://deals.extramaze.com).

This blog post is a sort of whirlwind tour of the use case and
business model, as well as how I use things like Racket, PostgreSQL,
and AWS -- but don't use advertising or JavaScript.

<!-- more -->

# Use case

Who needs this?

Let's say I play guitar. In fact, I seem to collect guitars. My
partner is skeptical of this use of financial and space resources. I
have my eye on a certain 7-string guitar. I don't _need_ it. But I
want it. If it goes on sale in the months ahead, I'd like to grab it.
I can point out that I saved (say) $500. Although my partner will see
right through this pathetic justification, they will appreciate that
at least I am hearing them and making an effort. I hope.

Or more virtuously: I'm a parent and my child needs a trombone when
school starts this autumn. I create an alert in case something goes on
sale over the summer.

# Business model

Currently you must join (create an account) to use the site. It's free
to search deals that are at least a day old. If you subscribe (pay),
you can search the very newest deals --- and save searches to get
alerted when matching deals appear.

Why ask people to pay? I'm disenchanted with services that survive on
money from advertising --- where users are the product being sold, and
the actual customers are advertisers.

Instead I want to do something where **users = customers**.

- I like making things that people like enough to pay for.

- As a practical matter, it can be hard enough to make one
  constituency happy. Two is double the difficulty -- even more when
  interests conflict. A small company has enough challenges; why build
  more into the foundation.

If it turns out that not enough people want to pay for this service?
I'd rather discontinue it than turn to advertising.

# Picking a name

The last time I picked a company name, the name was "Cakewalk"[^name]
and the year was 1987. Nearly a decade passed, the internet grew
popular, and we registered `cakewalk.com`. Easy.

[^name]: I'm simplifying for narrative flow. Day zero, the _product_ name was Cakewalk. The company name was Twelve Tone Systems. Later we adopted Cakewalk as the company name, too. The point is, it was much easier to pick domain names in ye olden times.

The experience now is... different. You may be surprised to learn that
many desirable domain names are already registered. I know, right?
After too many days agonizing with dictionaries and a thesaurus, I
settled on a contraction of "extra" and "amaze" -- Extramaze.

It turns out that "extramaze" is sometimes used to describe cues
outside a lab maze. Although that's a bit creepy, I'm relieved the
cues are _outside_ the maze -- this alludes to beacons of hope shining
on those of us stuck inside our own mazes! Or something. Moving on....

# Trying to do the right thing: Privacy

The web site does not use third-party JavaScript. No Google Analytics.
No social buttons. Nothing.[^fonts] The one exception -- if you subscribe --
is Stripe for payments.

[^fonts]: UPDATE: When I wrote this, I overlooked that I was using Google-served fonts. Since then [I stopped](/2018/05/extramaze-llc-using-system-fonts-not-google-fonts.html).

If you don't believe me look at the relevant parts of the
`Content-Security-Policy` response header value:

```
default-src 'none'; script-src https://checkout.stripe.com;
```

(Indeed notice that `script-src` doesn't include `'self'` -- the site
itself supplies no JavaScript. If you ask "Why not?" I can only
respond with another question: "Why?". The UX doesn't require it.
Maybe someday. Meanwhile it is one less facet to develop, test, debug,
and examine for vulnerabilities.)

Recently I experimented with Google and Twitter ads. Click-throughs
were OK but conversion was poor. I wondered if account-creation was a
speed bump, especially on mobile. So I added "Login with {Google,
Twitter}" buttons. Of course if you use those, you are sharing some
information with {Google, Twitter}. However you can still create and
use a native Extramaze account.

So much for third-party data collection. How about first-party?

- The web site does basic web server request and response logging.
- When people create an account:
    - The service asks for:
        - an email address
        - a how-would-you-like-to-be-addressed name
        - a password
    - Successful and failed logins are stored -- the latter to do an
      exponential back-off on subsequent attempts.
- When people also pay to subscribe:
    - Stripe asks for credit card and zip code information. Extramaze does
      not see or store this.
    - They gain the ability to create saved searches, and get email
      alerts. Of course we need to store the search phrase, e.g.
      `7-string left hand guitar`.

Beyond that, the service collects no personal information.

# Trying to do the right thing: Security

I've spent a lot of time thinking through the privacy and security
implications of account sign-up and password resets. This was the
scariest part for me. I found Troy Hunt's [_Everything you ever wanted
to know about building a secure password reset
feature_](https://www.troyhunt.com/everything-you-ever-wanted-to-know/)
incredibly helpful and follow its recommendations. Also useful: [_The
definitive guide to form-based website
authentication_](https://stackoverflow.com/questions/549/the-definitive-guide-to-form-based-website-authentication).

I already mentioned `Content-Security-Policy`. Information about this
and other security headers is at [securityheaders.com]. Some headers
let browsers report problems and <https://report-uri.com> is helpful
here.

[securityheaders.com]: https://securityheaders.com/?q=https%3A%2F%2Fdeals.extramaze.com&followRedirects=on

I won't say, "We take security very seriously", because that's become
a cliché. (Is there a web site that does for incident disclosures what
OurIncredibleJourney.com does for acquihire notices?)

But. I think about security frequently. I read as much as I can about
other people's experiences and recommendations. I assume that
vulnerabilities exist and the only question is who will find them; I
need to exercise a commercially reasonable effort to find them first.

# Ingredients

There are three main pieces:

- Crawl certain music retailer web sites looking for deals (daily).
- Send search alert emails (daily).
- Run a web site (24/7).

All use some mix of:

- Racket
- PostgreSQL
- AWS (EC2, ELB, ECS, SES, SNS, RDS, CloudWatch)

## Racket: Crawling and scraping

Most music product web stores have a section --- variously called
"outlet", "clearance", "deal zone", or "blowout" --- where the deals
are featured. Deals include demo units, price drops, and so on. For
demo units the product might be "like new" condition, or there might
be a cosmetic flaw which is described.

Currently we crawl these areas of [Sweetwater] and [Zzounds].

[Sweetwater]: https://www.sweetwater.com/dealzone/
[Zzounds]: https://www.zzounds.com/blowouts

More would be better. On the other hand, one step at a time.
Furthermore, these days many web sites block crawlers by default. It
doesn't matter if you `GET /robots.txt` and follow its rules
scrupulously. Instead you must ask a human for permission and get
white-listed. When you ask, the human might say no, or simply not
reply at all.

Racket has great "batteries included" for making HTTP requests, and
parsing HTML responses into [x-expressions]. That is, HTML such as

[x-expressions]: https://docs.racket-lang.org/xml/index.html#%28def._%28%28lib._xml%2Fprivate%2Fxexpr-core..rkt%29._xexpr~3f%29%29

```html
<p class="class">
  Some
  <em>awesome</em>
  HTML
  &tm;
</p>
```

is equivalent to the x-expression

```racket
'(p ([class "class"]) 
   "Some " 
   (em () "awesome")
   " HTML"
   tm)
```

As the Lisp Evangelism Task Force will point out on Hacker News every
few weeks, x-expressions are what XML would be if it weren't produced
by the Department of Redundancy Department.

Finding the "interesting bits" within an x-expression can be done in
various ways. In simple cases, you can use Racket's `match`[racket],
but for complicated HTML that can be tedious and/or brittle.
`se-path*/list`[racket] is a better idea, but you can only select a
direct path of element tags. You can't express CSS selector things
like, "Find all `p` elements somewhere under `div`s having a
`"container"` `class`".

So I wrote a function to do a depth-first folding walk of an
x-expression. In addition to accumulating a result value, it
accumulates a "path" -- a list of full x-expressions from "this" one
up through its ancestors to the root, full x-expression:

```racket
(define path? (listof xexpr/c))

;; A depth-first folding walk of the xexpr. The "path" is a list of
;; xexpr from the current one to its ancestors. You can `match` on
;; this to do the equivalent of `se-path*/list`, but with the full
;; power of `match`.
(define (walk f v x)
  (let recur ([path '()]
              [v v]
              [x x])
    (define this-path (cons x path))
    (f (match x
         [(list* (? symbol? tag) (? list?) xs)
          (for/fold ([v v]) ([x xs])
            (recur this-path v x))]
         [_ v])
       this-path)))
```

I wrapped this in a simple `select` function that `conjoin`s one or
more predicates:

```racket
;; Using quasiquoted match expressions with `walk` can be tedious and
;; error-prone. Often you end up specifying the match in more detail
;; than is really necessary. A sometimes friendlier way is to use
;; `select` with selector combinators grouped using `conjoin` (and
;; maybe `disjoin`).
(define (select x . fs) ;(-> xexpr/c (-> path? any) ...+ list?)
  (walk (λ (vs path)
          (match ((apply conjoin fs) path)
            [#f vs]
            [v (cons v vs)]))
        '()
        x))
```

I wrote a few functions intended to be used as combinators with
`conjoin`, to do roughly CSS selector style matching. A few example
pieces:

```racket
(define (this path)
  (match path
    [(cons v _) v]
    [_          #f]))

(define (tag path)
  (match (this path)
    [(list (? symbol? v) _ ...) v]
    [_                          #f]))

(define ((tag? sym) path)
  (equal? (tag path) sym))

```

For example, to extract the value of the `href` attribute for all
`a.some-class` elements whose immediate parent is a `div.box-class`:

```racket
(select xexpr
        (tag? 'a)
        (class? "some-class")
        (parent? (conjoin (tag? 'div)
                          (class? "some-box")))
        (attr-val 'href))
```

Of course you can layer on some shorthand compositions like
`tag.class?`:

```racket
(define (tag.class? sym class)
  (conjoin (tag? sym)
           (class? class)))
```

And the example becomes:

```racket
(select xexpr
        (tag.class? 'a "some-class")
        (parent? (tag.class? 'div "some-box")
        (attr-val 'href))

```

You _could_ also write a parser from CSS selector syntax to these
`select` expressions -- maybe even define a `#lang css-selector`. But
I like s-expressions. Even if I didn't, I'm not using this extensively
enough to warrant that work.

Likewise, although I'd like to open-source this as a distinct,
complete project, I just haven't had time to review it thoroughly,
write documentation, and so on.

## Racket: web server

Racket has great "batteries included" for making web site servers. In
addition to Racket's documentation for this, I can recommend Jesse
Alama's resources including his blog [lisp.sh](https://lisp.sh) and
his book [Server: Racket](https://serverracket.com/).

Racket provides a macro called [`dispatch-rules`] to define
bi-directional "routes". You give it rules, of each which is a URI
pattern and a handler function. It defines two functions covering all
of your rules:

[`dispatch-rules`]: https://docs.racket-lang.org/web-server/dispatch.html

- A `request? -> response?` dispatcher.
- A `procedure? -> string?` URI path maker.

```racket
(define-values (dispatch handler->path)
  (dispatch-rules
   [("")                  home]
   [("about")             about]
   [("path" "to" "foo")   path/to/foo]
   [("user" (string-arg)) user/id]
   [else                  not-found]))
```

Because I needed to do authorization, I wrapped this in my own macro,
`dispatch-rules+roles`. This also defines a third function, `request?
-> roles?`: Given a request that matches one of the patterns, what
roles is a user required to have to be authorized to access it?

In the following example, we use three roles:

- `'anon` means an anonymous user.

- `'free` is a user who is authenticated (logged in); certain
  routes should only be available to them.

- `'paid` is an authenticated user who has also subscribed (paid) and
  therefore is authorized for even more routes.

```racket
(define-values (dispatch handler->path request->roles)
  (dispatch-rules+roles
   ;; Routes requiring 'anon or 'free or 'paid roles
   [(anon free paid)
    [("")      home]
    [("about") about]
    [("join")  join]]
   ;; Routes requiring 'free or 'paid roles
   [(free paid)
    [(logout)      logout]
    [(preferences) preferences]
    [(subscribe)   subscribe]
   ;; Routes requiring 'paid role
   [(paid)
    [(payment)     payment]
    [(unsubscribe) unsubscribe]]
   [else not-found]))
```

My `dispatch-rules+roles` macro doesn't itself do authorization -- it
defines the same old `dispatch` function that plain `dispatch-rules`
does. You need to wrap that `dispatch` with something that consults
`request->roles` and calls `dispatch` -- or returns a `403 Forbidden`
response (for an API) or redirects to a login or subscribe page (web
app).

Of course, _that_ in turn should be wrapped in something that sets the
current user (so we know their roles) from e.g. a session key -- or
returns a `401` response (for an API) or redirects to a login page
(web app).

Speaking of multiple wrappers around `dispatch`, this is a nice way to
compose functionality, which I've seen in the Clojure [Ring]
community. It's cleaner to have one wrapper per bit of functionality,
as opposed to one handler with a monolithic hairball of conditionals.

[Ring]: https://github.com/ring-clojure/ring/wiki/Concepts#middleware

So a `handler?` is a function from `request?` to `response?`, like
`dispatch`. A `wrapper?` is a function that takes a `handler?`, and
returns a new `handler?`.

```racket
(define handler? (-> request? response?))
(define wrapper? (-> handler? handler?))
```

For instance, a wrapper to enforce https:

```racket
;; This assumes we're behind ELB or nginx which gets both http and
;; https, talks to us only via http, setting an X-Forwarded-Proto
;; header to say the original protocol and an X-Forwarded-For header
;; to say the original IP.

(define/contract ((wrap-http->https handler) req) wrapper?
  (match (headers-assq* #"x-forwarded-proto" (request-headers/raw req))
    [(header _ #"http")
     (redirect-to (path->external-uri
                   (url->string (struct-copy url (request-uri req)
                                             [scheme #f]
                                             [port   #f])))
                  permanently)]
    [_ (handler req)]))
```

A whole chain of such wrappers can be composed --- using `compose` or
the `~>` threading macro -- to wrap the original `dispatch` function
when we start the Racket web server:

```racket
(serve/servlet (~> ;Note: requests go UP this chain, responses DOWN
                dispatch
                wrap-gzip
                wrap-not-modified
                wrap-authorize
                wrap-authenticate
                wrap-http->https
                wrap-timed-and-logged)
               #:servlet-path      "/"
               #:servlet-regexp    #px""
               #:listen-ip         #f
               #:port              (current-internal-port)
               #:launch-browser?   (not (current-production))
               #:servlet-responder error-responder)
```

# PostgreSQL

I was skeptical about using PostgreSQL: Is it web scale?

Seriously, I don't have anything very exciting to describe about
using PostgreSQL for this --- which is wonderful. Currently:

- I'm hosting at AWS RDS.

- Initial capture from crawling goes into a simple [star
  schema](https://en.wikipedia.org/wiki/Star_schema). The fact table
  has information about each deal, including its first-seen and
  last-seen times. When we already have a row for a deal, we just
  update the last-seen time. Dimension tables are what you'd expect
  --- brand, product, and so on.

- A simple view joins to denormalize, fitting the kind of query done
  by the web site's search page. This is already quite fast; a
  materialized view makes it even faster.

- Full-text search is delicious.
  
Toward the end of my time at Cakewalk, I got some experience with
Microsoft SQL Server, including optimizations. As a result, I'm aware
that I can likewise do much more with PostgreSQL --- looking at query
execution plans, tuning indexes and queries, and so on. For now I'm
satisfied I know roughly what I can do if/as/when necessary.

Racket has an excellent [db] package. It also has a [sql] package that
lets you write SQL as s-expressions rather than blobs of text, for
example:

[db]: http://docs.racket-lang.org/db/index.html
[sql]: http://docs.racket-lang.org/sql/index.html

```sql
SELECT first, last
FROM tribbles
WHERE id = $1
```

```racket
(select first last
  #:from tribbles
  #:where (= id ,id))
```

# AWS

I'm using Amazon Web Services because I feel badly that it is such an
unpopular choice and want to see them get at least a _little_ business.

Seriously, this has been a reasonable choice to get going quickly
and it is affordable initially within the free tier.

One of my earliest Racket packages is
[aws](https://pkgs.racket-lang.org/package/aws). Although I've used it
intermittently, lately I'm _really_ eating my own dog food.

I made a local change to support getting AWS credentials from EC2
instance meta-data. After living with that in production for a couple
months, I shared that back in [commit
84c28ba](https://github.com/greghendershott/aws/commit/84c28ba16f238a8c48c54a9e858ba4bb2ec53742).

Just a quick overview of other parts:

- ELB: This can distribute load among multiple web servers. Even with
  just one server (to start) it is a convenient way to handle SSL.

- SES: I'm only sending "transactional" emails (for account creation,
  password reset, and search alerts) so it's been easy so far to
  maintain a good reputation.

- Docker and ECS. Very helpful: [_Running Docker on AWS from
  the ground
  up_](https://www.ybrikman.com/writing/2015/11/11/running-docker-aws-ground-up/).

- CloudWatch Logs:

    - It is pretty easy to make a Racket [log receiver] that accumulates
      things into batches and does [`PutLogEvents`].
      
    - It is handy to use JSON format for request/response logs. Not
      only does this display nicely, there's a decent query feature,
      e.g. `{$.response.duration > 100}` or `{$.request.headers.Host =
      "deals.extramaze.com"}`.

[log receiver]: https://docs.racket-lang.org/reference/logging.html#%28def._%28%28quote._~23~25kernel%29._make-log-receiver%29%29
[`PutLogEvents`]: https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html

# Conclusion

I hope this helps give a taste of what it's like to start a small SaaS
business c. 2018 using Racket, PostgreSQL, and AWS -- but without
using advertising or JavaScript.

I realize this post has a somewhat uneven level of detail, so maybe I
will loop back later and drill down on some parts.


---
