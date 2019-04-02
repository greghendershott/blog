# Configure where are sources, build cache, and root of the web site
# output.
src   := src
cache := .cache
www   := www

# Make normally "pulls" targets from sources, but we want to "push"
# sources to targets. As a result, we need to build lists of sources
# and from those build lists of targets.
post-sources := $(shell find $(src)/posts -type f)
post-caches  := $(patsubst $(src)/posts/%.md,$(cache)/%.rktd,$(post-sources))
post-htmls   := $(patsubst $(cache)/%.rktd,$(www)/%.html,$(post-caches))

tag-caches     := $(wildcard $(cache)/tags/*)
tag-htmls      := $(patsubst %,$(www)/tags/%.html,$(notdir $(tag-caches)))
tag-atom-feeds := $(patsubst %,$(www)/feeds/%.atom.xml,$(notdir $(tag-caches)))
tag-rss-feeds  := $(patsubst %,$(www)/feeds/%.rss.xml,$(notdir $(tag-caches)))

non-post-sources := $(wildcard $(src)/non-posts/*.md)
non-post-htmls   := $(patsubst %.md,$(www)/%.html,$(notdir $(non-post-sources)))

# Racket commands
#
# Note: For now these are in rkt subdir. Someday move the generic
# pieces to a "tadpole" package?
compile-post   := racket rkt/compile-post.rkt
render-post    := racket rkt/render-post.rkt
render-non-post := racket rkt/compile-render-non-post.rkt
make-tag-index := racket rkt/make-tag-index.rkt
make-tag-list  := racket rkt/make-tag-list.rkt
make-tag-feed  := racket rkt/make-tag-feed.rkt
make-sitemap   := racket rkt/make-sitemap.rkt
make-css       := racket rkt/styles.rkt
new-post       := racket rkt/new-post.rkt
preview        := racket rkt/preview.rkt

.PHONY: rkt
rkt:
	(cd rkt; raco make *.rkt)

######################################################################

# Given my current level of Makefile fu, I can only see how to make
# this work with two distinct passes. The main issue seems to be that
# compile-post.rkt needs to produce $(tag-caches), which in turn are
# sources for index and feed files. My attempts to do it as one single
# chain never worked.

.PHONY: all clean new serve preview

all:
	make cache
	make www

clean: clean-cache clean-www

new:
	$(new-post) $(src)/posts

serve: $(www)/index.html all
	$(preview) $<

preview: $(www)/index.html all
	$(preview) $< "browser"

######################################################################
# Stage 1

.PHONY: cache clean-cache

cache: $(post-caches)

clean-cache:
	-rm -rf $(cache)

$(cache)/%.rktd: $(src)/posts/%.md
	$(compile-post) $< $(abspath $(cache)/tags) $@

######################################################################
# Stage 2

.PHONY: hmtls-and-feeds clean-www

www: htmls feeds static sitemap

clean-www: clean-htmls clean-feeds clean-sitemap

# HTMLs

.PHONY: hmtls clean-htmls

htmls: $(post-htmls) $(tag-htmls) $(non-post-htmls) \
       $(www)/tags/index.html $(www)/index.html $(www)/main.css \
       rkt/render-page.rkt rkt/site.rkt

clean-htmls:
	-rm $(post-htmls)
	-rm $(tag-htmls)
	-rm $(non-post-htmls)
	-rm $(www)/index.html
	-rm $(www)/tags/index.html
	-rmdir $(www)/tags
	-rm $(www)/main.css

$(www)/%.html: $(cache)/%.rktd rkt/render-page.rkt
	$(render-post) $< $(www) $@

$(www)/%.html: $(src)/non-posts/%.md rkt/compile-render-non-post.rkt
	$(render-non-post) $< $(www) $@

$(www)/tags/%.html: $(cache)/tags/% rkt/render-page.rkt
	$(make-tag-index) $< $(www) $@

$(www)/tags/index.html: $(tag-caches) rkt/render-page.rkt rkt/make-tag-list.rkt
	$(make-tag-list) $(cache)/tags/ $(www) $@

$(www)/index.html: $(www)/tags/all.html rkt/render-page.rkt rkt/make-tag-feed.rkt
	cp $< $@

$(www)/main.css: rkt/styles.rkt
	$(make-css) $@

# Feeds

.PHONY: feeds clean-feeds

feeds: $(tag-atom-feeds) $(tag-rss-feeds)

clean-feeds:
	-rm $(www)/feeds/*.xml
	-rmdir $(www)/feeds

$(www)/feeds/%.atom.xml: $(cache)/tags/%
	$(make-tag-feed) $< atom $(www) $@

$(www)/feeds/%.rss.xml: $(cache)/tags/%
	$(make-tag-feed) $< rss $(www) $@

# Static assets

.PHONY: static

# Note the $(src)/static/. to copy dotfiles, too!
static:
	cp -pr $(src)/static/. $(www)/

# Sitemap

.PHONY: sitemap clean-sitemap

sitemap:
	$(make-sitemap) $(www)/sitemap.txt

clean-sitemap:
	rm $(www)/sitemap.txt


######################################################################
# GitHub pages deploy

# Just set $(www) to /home/greg/src/greghendershott.github.com


######################################################################
# S3 bucket deploy

AWS    := aws --profile greg
BUCKET := www.greghendershott.com

.PHONY: s3-deploy s3-full-deploy

s3-deploy:
	$(AWS) s3 sync --no-follow-symlinks $(www) s3://$(BUCKET)

s3-full-deploy:
	$(AWS) s3 sync --delete --no-follow-symlinks $(www) s3://$(BUCKET)
