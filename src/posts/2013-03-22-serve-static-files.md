    Title: Serve static files
    Date: 2013-03-22T12:00:00
    Tags: Racket

I wanted [Frog][] to provide a "preview" feature: Launch a local web
server with a version of the site, and open a web browser.

This local web server simply needs to serve static files. No
server-side applications. (Not even features you'd likely want in a
production static file server like gzip compression or `If-Modified`
handling.)  It just needs to start quickly, and preferably not be a
lot of work to code.

<!-- more -->

At first glance, the Racket web server's [serve/servlet][] function
has a somewhat overwhelming set of options--approximately 25 keyword
arguments. Fortunately only a few apply to this situation. If you want
to start a web server that _only_ serves static files, it's simply
this:

```racket
(require web-server/servlet-env
         web-server/http
         web-server/dispatchers/dispatch)
(serve/servlet (lambda (_) (next-dispatcher))
               #:servlet-path "/"
               #:extra-files-paths (list path/to/files)
               #:port 8080
               #:launch-browser? #t)
```

Easy.

- - -

Just as I was finishing this post, Jay McCarthy pointed out to me that
the docs have [an example of a more direct way][serve]:

```racket
(require web-server/web-server)
(serve #:dispatch (files:make #:url->path (make-url->path path/to/files)
                              #:path->mime-type (lambda (_)
                                                  #"application/octet-stream"))
       #:port 8080)
```

This handles requests more directly, as well as not requiring modules
that are hardly utilized, since we're not using any servlets.

Just keep in mind a few wrinkles:

- `serve` returns immediately with a procedure you call to shut down
  the server.

- You'll need to implement your own version of `serve/servlet`
  conveniences:

  - Print messages like, `Your Web application is running at
    http://localhost:8080/.` `Stop this program at any time to terminate
    the Web Server.`.
  
  - Call [send-url][] to launch the browser.
  
  - Wait for the user to press `CTRL-C`, and call the shutdown
    procedure given to you by `serve`.

For example, [here's][repo] how `serve/servlet` does these things.

Using `serve` is more efficient and flexible. You can use it for a
variety of web server scenarios. On the other hand it looks like
`serve/servlet` is probably more convenient for the specific use case
of "preview this in a local web server and browser".


[Frog]: https://github.com/greghendershott/frog
[serve/servlet]: http://docs.racket-lang.org/web-server/run.html#%28def._%28%28lib._web-server%2Fservlet-env..rkt%29._serve%2Fservlet%29%29
[serve]: http://docs.racket-lang.org/web-server-internal/web-server.html#%28def._%28%28lib._web-server%2Fweb-server..rkt%29._serve%29%29
[send-url]: http://docs.racket-lang.org/net/sendurl.html#%28def._%28%28lib._net%2Fsendurl..rkt%29._send-url%29%29
[repo]: https://github.com/plt/racket/blob/611e8d0d17ce096ef624ef442d632f954a783e7b/collects/web-server/servlet-dispatch.rkt#L114-L160
