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
# output.
POSTS     := src/posts
CACHE     := .cache
WWW       := www

# Make normally "pulls" targets from sources, but we want to "push"
# sources to targets. As a result, we need to build lists of sources
# like post .md files, and from those build lists of targets.
## TODO: Generate from Scribble sources, too
POST-SOURCES := $(wildcard $(POSTS)/*.md)
POST-CACHES  := $(patsubst %.md,$(CACHE)/%.rktd,$(notdir $(POST-SOURCES)))
POST-HTMLS   := $(patsubst %.rktd,$(WWW)/%.html,$(notdir $(POST-CACHES)))

TAG-CACHES     := $(wildcard $(CACHE)/tags/*)
TAG-HTMLS      := $(patsubst %,$(WWW)/tags/%.html,$(notdir $(TAG-CACHES)))
TAG-ATOM-FEEDS := $(patsubst %,$(WWW)/feeds/%.atom.xml,$(notdir $(TAG-CACHES)))
TAG-RSS-FEEDS  := $(patsubst %,$(WWW)/feeds/%.rss.xml,$(notdir $(TAG-CACHES)))

## TODO: Non-post sources.

# Racket commands
#
# Note: For now these are in rkt subdir. Someday move the generic
# pieces to a "tadpole" package?
COMPILE-POST   := racket rkt/compile-post.rkt
RENDER-POST    := racket rkt/render-post.rkt
MAKE-TAG-INDEX := racket rkt/make-tag-index.rkt
MAKE-TAG-LIST  := racket rkt/make-tag-list.rkt
MAKE-TAG-FEED  := racket rkt/make-tag-feed.rkt
PREVIEW        := racket rkt/preview.rkt

######################################################################

# Given my current level of Makefile fu, I can only see how to make
# this work with two distinct passes. The main issue seems to be that
# compile-post.rkt needs to produce $(TAG-CACHES), which in turn are
# sources for index and feed files. My attempts to do it as one single
# chain never worked.

.PHONY: all clean preview

all:
	make posts-cache
	make htmls-and-feeds

clean: clean-posts-cache clean-htmls-and-feeds

preview: $(WWW)/index.html all
	$(PREVIEW) $<

######################################################################
# Stage 1

.PHONY: posts-cache clean-posts-cache

posts-cache: $(POST-CACHES)

clean-posts-cache:
	-rm $(CACHE)/tags/*
	-rmdir $(CACHE)/tags
	-rm $(CACHE)/*
	-rmdir $(CACHE)

$(CACHE)/%.rktd: $(POSTS)/%.md
	$(COMPILE-POST) $< $@


######################################################################
# Stage 2

.PHONY: hmtls-and-feeds clean-htmls-and-feeds

htmls-and-feeds: htmls feeds

clean-htmls-and-feeds: clean-htmls clean-feeds


# HTMLs

.PHONY: hmtls clean-htmls

htmls: $(POST-HTMLS) $(TAG-HTMLS) \
       $(WWW)/tags/index.html $(WWW)/index.html $(WWW)/main.css \
       rkt/render-page.rkt rkt/site.rkt

clean-htmls:
	-rm $(POST-HTMLS)
	-rm $(TAG-HTMLS)
	-rm $(WWW)/index.html
	-rm $(WWW)/tags/index.html
	-rmdir $(WWW)/tags

$(WWW)/%.html: $(CACHE)/%.rktd rkt/render-page.rkt
	$(RENDER-POST) $< $@

$(WWW)/tags/%.html: $(CACHE)/tags/% rkt/render-page.rkt
	$(MAKE-TAG-INDEX) $< $@

$(WWW)/tags/index.html: $(TAG-CACHES) rkt/render-page.rkt rkt/make-tag-list.rkt
	$(MAKE-TAG-LIST) $(CACHE)/tags/ $@

$(WWW)/index.html: $(WWW)/tags/all.html rkt/render-page.rkt rkt/make-tag-feed.rkt
	cp $< $@

$(WWW)/main.css: rkt/styles.rkt
	racket rkt/styles.rkt $@

# Feeds

.PHONY: feeds clean-feeds

feeds: $(TAG-ATOM-FEEDS) $(TAG-RSS-FEEDS)

clean-feeds:
	-rm $(WWW)/feeds/*.xml
	-rmdir $(WWW)/feeds

$(WWW)/feeds/%.atom.xml: $(CACHE)/tags/%
	$(MAKE-TAG-FEED) $< atom $@

$(WWW)/feeds/%.rss.xml: $(CACHE)/tags/%
	$(MAKE-TAG-FEED) $< rss $@

######################################################################
# Deploy

AWS    := aws --profile greg
BUCKET := s3://www.greghendershott.com


.PHONY: deploy
deploy:
	$(AWS) s3 sync --delete --no-follow-symlinks $(WWW) $(BUCKET)
