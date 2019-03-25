# A big simplifying assumption here is that we won't support post HTML
# outs like {year}/{month}/{title}.html. Instead, the HTMLs are all in
# one flat directory, with the same basename as their source. So, no
# "permalink patterns" magic. Instead, name the source files the way
# you want the URL to be, including a date or not, as you prefer.
#
# That does mean the URLs won't be backward compatible, but, we'll
# just need to handle that with server or meta redirects. Doing so is
# easier than the Makefile contortions required, IMHO.

# Configure where are sources, build cache, and root of the web site
# output. Below is something suitable for how I use GitHub Pages. But
# someone else might want the sources in the root of the repo, and OUT
# to be a subdir that is .gitignored because it is copied to a web
# server. In any case, everyone would .gitignore CACHE.

POSTS     := _src/posts
CACHE     := .cache
OUT       := .

# Make normally "pulls" targets from sources, but we want to "push"
# sources to targets. As a result, we need to build lists of sources
# like post .md files, and from those build lists of targets.
## TODO: Generate from Scribble sources, too
POST-SOURCES := $(wildcard $(POSTS)/*.md)
POST-CACHES  := $(patsubst %.md,$(CACHE)/%.rktd,$(notdir $(POST-SOURCES)))
POST-HTMLS   := $(patsubst %.rktd,$(OUT)/%.html,$(notdir $(POST-CACHES)))

TAG-SOURCES    := $(wildcard $(CACHE)/tags/*)
TAG-HTMLS      := $(patsubst %,$(OUT)/tags/%.html,$(notdir $(TAG-SOURCES)))
TAG-ATOM-FEEDS := $(patsubst %,$(OUT)/feeds/%.atom.xml,$(notdir $(TAG-SOURCES)))
TAG-RSS-FEEDS  := $(patsubst %,$(OUT)/feeds/%.rss.xml,$(notdir $(TAG-SOURCES)))

# Racket commands
#
# Note: For now these are in _rkt subdir. Someday move to a package?
COMPILE-POST       := racket _rkt/compile-post.rkt
RENDER-POST        := racket _rkt/render-post.rkt
MAKE-TAG-INDEX     := racket _rkt/make-tag-index.rkt
MAKE-TAG-ATOM-FEED := racket _rkt/make-tag-atom-feed.rkt
MAKE-TAG-RSS-FEED  := racket _rkt/make-tag-rss-feed.rkt


######################################################################

# Given my current level of Makefile fu, I can only see how to make
# this work with two distinct passes. The main issue seems to be that
# compile-post.rkt needs to produce $(TAG-SOURCES), which in turn are
# sources for index and feed files. My attempts to do it as one single
# chain never worked.

.PHONY: all
all:
	make posts-cache
	make htmls-and-feeds

.PHONY: clean
clean: clean-posts-cache clean-htmls-and-feeds

######################################################################
# Stage 1

.PHONY: posts-cache
posts-cache: $(POST-CACHES)

$(CACHE)/%.rktd: $(POSTS)/%.md
	$(COMPILE-POST) $< $@

.PHONY: clean-posts-cache
clean-posts-cache:
	-rm $(CACHE)/tags/*
	-rmdir $(CACHE)/tags
	-rm $(CACHE)/*
	-rmdir $(CACHE)


######################################################################
# Stage 2

.PHONY: hmtls-and-feeds
htmls-and-feeds: htmls feeds

.PHONY:  clean-htmls-and-feeds
clean-htmls-and-feeds: clean-htmls clean-feeds


.PHONY: hmtls
htmls: $(POST-HTMLS) $(TAG-HTMLS) $(OUT)/index.html

$(OUT)/%.html: $(CACHE)/%.rktd
	$(RENDER-POST) $< $@

$(OUT)/tags/%.html: $(CACHE)/tags/%
	$(MAKE-TAG-INDEX) $< $@

$(OUT)/index.html: $(OUT)/tags/all.html
	cp $< $@

.PHONY: clean-htmls
clean-htmls:
	-rm $(OUT)/*.html
	-rm $(OUT)/tags/*.html
	-rmdir $(OUT)/tags


.PHONY: feeds
feeds: $(TAG-ATOM-FEEDS)  $(TAG-RSS-FEEDS)

$(OUT)/feeds/%.atom.xml: $(CACHE)/tags/%
	$(MAKE-TAG-ATOM-FEED) $< $@

$(OUT)/feeds/%.rss.xml: $(CACHE)/tags/%
	$(MAKE-TAG-RSS-FEED) $< $@

.PHONY: clean-feeds
clean-feeds:
	-rm $(OUT)/feeds/*.xml
	-rmdir $(OUT)/feeds
