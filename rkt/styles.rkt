#lang at-exp racket/base

(require racket/require
         (multi-in racket (match format function))
         "util.rkt")

(module+ main (main))

(define (main)
  (match (current-command-line-arguments)
    [(vector css-file)
     (call-with-output-file*/delete
      #:exists 'replace css-file
      (curry display styles))]))

(define text "#333")

(define red "#ef5350")
(define blue "#1976d2")
(define green "#66bb6a")
(define accent "rgba(125,197,238,0.22)")

(define main-styles
  @~a{body {margin: 0;
            font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI","Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans","Droid Sans", "Helvetica Neue", sans-serif;
            font-size: 18px;
            background-color: #fff;
            color: @|text|}

      main, header.site, footer.site {padding: 4px 8px}
      header.site, footer.site {text-align: center;
                                margin: 0;
                                background-color: #fafafa}
      header.site {box-shadow: 0px 2px 5px #aaa;
                   margin-bottom: 16px}
      footer.site {box-shadow: 0px -2px 5px #aaa;
                   font-size: smaller;
                   margin-top: 32px}
      header.site nav ul li a img {height: 12px;
                                   width: 12px}

      main {margin: 0px auto;
            max-width: 768px}

      p {line-height: 1.6}
      ul {line-height: 2}

      a {text-decoration: none;
         color: #a57e74}
      a:hover {text-decoration:underline}

      nav {}
      nav ul {list-style-type: none;
              padding: 0;
              margin: 0}
      nav ul li {margin-right: 1em;
                 white-space: nowrap}
      nav ul li {display: inline}

      h1, h2, h3 {font-weight: 500}
      h1 {margin: 0 0 8px 0;
          font-size: 1.5em}
      h2 {font-size: 1.3em}

      article.index {padding-bottom: 18px;
                     border-bottom: 2px dashed #ccc}

      div.footnotes {font-size: 90%;
                     border-top: 1px dotted}

      kbd {padding: 0.1em 0.6em;
           border: 1px solid #CCC;
           font-size: 80%;
           background-color: #F7F7F7;
           color: #333;
           box-shadow: 0px 1px 0px rgba(0, 0, 0, 0.2), 0px 0px 0px 2px #FFF inset;
           border-radius: 3px;
           display: inline-block;
           margin: 0px 0.1em;
           line-height: 1.4;
           white-space: nowrap}

      code {font-family: monospace;
            font-size: 90%;
            background-color: #f0f0fa;
            border: 1px solid #bbb;
            border-radius: 4px;
            padding: 1px 4px}})

(define pygments-styles
  @~a{.linenos pre {font-size: 85%;
                    color: #ddd;
                    background-color: #fff;
                    border: none}

      .source pre {color: #333;
                   font-size: 85%;
                   background-color: #fdf6e3;
                   padding: 10px;
                   border: 1px solid #ccc;
                   border-radius: 4px;
                   min-width: 40em}

      .hll {background-color: #ffffcc}
      .p {color: #999} @;Parens
      .c {color: #e90;
          font-style: italic} @;Comment
      .err {color: #333} @;Error
      .k {color: #553} @;Keyword
      .o {color: #334} @;Operator
      .cm {color: #e90;
           font-style: italic} @;Comment.Multiline
      .cp {color: #e90;
           font-weight: bold;
           font-style: italic} @;Comment.Preproc
      .c1 {color: #e90;
           font-style: italic} @;Comment.Single
      .cs {color: #e90;
           font-weight: bold;
           font-style: italic} @;Comment.Special
      .gd {color: #333} @;Generic.Deleted
      .ge {color: #333;
           font-style: italic} @;Generic.Emph
      .gr {color: #a00} @;Generic.Error
      .gh {color: #333} @;Generic.Heading
      .gi {color: #333} @;Generic.Inserted
      .go {color: #333} @;Generic.Output
      .gp {color: #333} @;Generic.Prompt
      .gs {font-weight: bold} @;Generic.Strong
      .gu {color: #333} @;Generic.Subheading
      .gt {color: #333} @;Generic.Traceback
      .kc {color: #cd5c5c} @;Keyword.Constant
      .kd {color: #cd5c5c} @;Keyword.Declaration
      .kn {color: #cd5c5c} @;Keyword.Namespace
      .kp {color: #cd5c5c} @;Keyword.Pseudo
      .kr {color: #cd5c5c} @;Keyword.Reserved
      .kt {color: #cd5c5c} @;Keyword.Type
      .m {color: #090} @;Literal.Number
      .s {color: #090} @;Literal.String
      .na {color: #333} @;Name.Attribute
      .nb {color: #355} @;Name.Builtin
      .nc {color: #333} @;Name.Class
      .no {color: #090} @;Name.Constant
      .nd {color: #333} @;Name.Decorator
      .ni {color: #333} @;Name.Entity
      .ne {color: #900} @;Name.Exception
      .nf {color: #335} @;Name.Function
      .nl {color: #333} @;Name.Label
      .nn {color: #333} @;Name.Namespace
      .nt {color: #333} @;Name.Tag
      .nv {color: #335} @;Name.Variable
      .ow {color: #334} @;Operator.Word
      .w {color: #333} @;Text.Whitespace
      .mf {color: #090} @;Literal.Number.Float
      .mh {color: #090} @;Literal.Number.Hex
      .mi {color: #090} @;Literal.Number.Integer
      .mo {color: #090} @;Literal.Number.Oct
      .sb {color: #090} @;Literal.String.Backtick
      .sc {color: #090} @;Literal.String.Char
      .sd {color: #090} @;Literal.String.Doc
      .s2 {color: #090} @;Literal.String.Double
      .se {color: #090} @;Literal.String.Escape
      .sh {color: #090} @;Literal.String.Heredoc
      .si {color: #090} @;Literal.String.Interpol
      .sx {color: #090} @;Literal.String.Other
      .sr {color: #090} @;Literal.String.Regex
      .s1 {color: #090} @;Literal.String.Single
      .ss {color: #090} @;Literal.String.Symbol
      .bp {color: #090} @;Name.Builtin.Pseudo
      .vc {color: #090} @;Name.Variable.Class
      .vg {color: #090} @;Name.Variable.Global
      .vi {color: #090} @;Name.Variable.Instance
      .il {color: #090}}) @;Literal.Number.Integer.Long

(define (minify s)
  (regexp-replaces s '([#rx"[ \n]+" " "])))

(define styles
  (minify @~a{@main-styles
              @pygments-styles}))
