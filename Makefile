# Configure where are sources, build cache, and root of the web site
# output.
SRC       := src
POSTS     := $(SRC)/posts
CACHE     := .cache
WWW       := www

# Make normally "pulls" targets from sources, but we want to "push"
# sources to targets. As a result, we need to build lists of sources
# like post .md files, and from those build lists of targets.
POST-SOURCES := $(wildcard $(POSTS)/*.md)
POST-CACHES  := $(patsubst %.md,$(CACHE)/%.rktd,$(notdir $(POST-SOURCES)))
POST-HTMLS   := $(patsubst %.rktd,$(WWW)/%.html,$(notdir $(POST-CACHES)))

TAG-CACHES     := $(wildcard $(CACHE)/tags/*)
TAG-HTMLS      := $(patsubst %,$(WWW)/tags/%.html,$(notdir $(TAG-CACHES)))
TAG-ATOM-FEEDS := $(patsubst %,$(WWW)/feeds/%.atom.xml,$(notdir $(TAG-CACHES)))
TAG-RSS-FEEDS  := $(patsubst %,$(WWW)/feeds/%.rss.xml,$(notdir $(TAG-CACHES)))

NON-POST-SOURCES := $(wildcard $(SRC)/non-posts/*.md)
NON-POST-HTMLS   := $(patsubst %.md,$(WWW)/%.html,$(notdir $(NON-POST-SOURCES)))

# Racket commands
#
# Note: For now these are in rkt subdir. Someday move the generic
# pieces to a "tadpole" package?
COMPILE-POST   := racket rkt/compile-post.rkt
RENDER-POST    := racket rkt/render-post.rkt
RENDER-COMPILE-NON-POST := racket rkt/compile-render-non-post.rkt
MAKE-TAG-INDEX := racket rkt/make-tag-index.rkt
MAKE-TAG-LIST  := racket rkt/make-tag-list.rkt
MAKE-TAG-FEED  := racket rkt/make-tag-feed.rkt
MAKE-SITEMAP   := racket rkt/make-sitemap.rkt
PREVIEW        := racket rkt/preview.rkt

.PHONY: rkt
rkt:
	(cd rkt; raco make *.rkt)

######################################################################

# Given my current level of Makefile fu, I can only see how to make
# this work with two distinct passes. The main issue seems to be that
# compile-post.rkt needs to produce $(TAG-CACHES), which in turn are
# sources for index and feed files. My attempts to do it as one single
# chain never worked.

.PHONY: all clean preview

all:
	make cache
	make www

clean: clean-cache clean-www

preview: $(WWW)/index.html all
	$(PREVIEW) $<

######################################################################
# Stage 1

.PHONY: cache clean-cache

cache: $(POST-CACHES)

clean-cache:
	-rm $(CACHE)/tags/*
	-rmdir $(CACHE)/tags
	-rm $(CACHE)/*
	-rmdir $(CACHE)

$(CACHE)/%.rktd: $(POSTS)/%.md
	$(COMPILE-POST) $< $@


######################################################################
# Stage 2

.PHONY: hmtls-and-feeds clean-www

www: htmls feeds static sitemap

clean-www: clean-htmls clean-feeds clean-sitemap

# HTMLs

.PHONY: hmtls clean-htmls

htmls: $(POST-HTMLS) $(TAG-HTMLS) $(NON-POST-HTMLS) \
       $(WWW)/tags/index.html $(WWW)/index.html $(WWW)/main.css \
       rkt/render-page.rkt rkt/site.rkt

clean-htmls:
	-rm $(POST-HTMLS)
	-rm $(TAG-HTMLS)
	-rm $(NON-POST-HTMLS)
	-rm $(WWW)/index.html
	-rm $(WWW)/tags/index.html
	-rmdir $(WWW)/tags

$(WWW)/%.html: $(CACHE)/%.rktd rkt/render-page.rkt
	$(RENDER-POST) $< $@

$(WWW)/%.html: $(SRC)/non-posts/%.md rkt/compile-render-non-post.rkt
	$(RENDER-COMPILE-NON-POST) $< $@

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

# Static assets

.PHONY: static

static:
	cp -r $(SRC)/static/* $(WWW)/

# Sitemap

.PHONY: sitemap clean-sitemap

sitemap:
	$(MAKE-SITEMAP) $(WWW)/sitemap.txt

clean-sitemap:
	rm $(WWW)/sitemap.txt

######################################################################
# GitHub pages deploy

REPO := /home/greg/src/greghendershott.github.com/

github-deploy:
	@(echo 'Press enter to rm -r and copy files $$(date +%Y%m%d%H%M%S)'; read dummy)
	rm -r $(REPO)
	cp -r $(WWW)/* $(REPO)
	cd $(REPO) && git commit -am "Update $$(date +%Y%m%d%H%M%S)"
# && git push

######################################################################
# S3bucket deploy

AWS    := aws --profile greg
BUCKET := www.greghendershott.com

.PHONY: s3-deploy s3-full-deploy

s3-deploy:
	$(AWS) s3 sync --no-follow-symlinks $(WWW) s3://$(BUCKET)

s3-full-deploy:
	$(AWS) s3 sync --delete --no-follow-symlinks $(WWW) s3://$(BUCKET)
	racket rkt/old-post-redirs.rkt s3 $(BUCKET)
