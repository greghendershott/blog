# Copyright 2019 by Greg Hendershott.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# 	http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
make-post-cache    := racket rkt/make-post-cache.rkt
make-post-html     := racket rkt/make-post-html.rkt
make-non-post-html := racket rkt/make-non-post-html.rkt
make-tag-index     := racket rkt/make-tag-index.rkt
make-tag-list      := racket rkt/make-tag-list.rkt
make-tag-feed      := racket rkt/make-tag-feed.rkt
make-sitemap       := racket rkt/make-sitemap.rkt
make-css           := racket rkt/make-css.rkt
new-post           := racket rkt/make-new-post.rkt
preview            := racket rkt/make-preview.rkt

.PHONY: rkt
rkt:
	(cd rkt; raco make *.rkt; raco test -x .)

######################################################################

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
	$(make-post-cache) $< $(abspath $(cache)/tags) $@

######################################################################
# Stage 2

.PHONY: www clean-www

www: htmls feeds static sitemap

clean-www: clean-htmls clean-feeds clean-sitemap

# HTMLs

.PHONY: hmtls clean-htmls

htmls: $(post-htmls) $(tag-htmls) $(non-post-htmls) \
       $(www)/tags/index.html $(www)/index.html $(www)/main.css \
       rkt/page-xexpr.rkt rkt/site.rkt

clean-htmls:
	-rm $(post-htmls)
	-rm $(tag-htmls)
	-rm $(non-post-htmls)
	-rm $(www)/index.html
	-rm $(www)/tags/index.html
	-rmdir $(www)/tags
	-rm $(www)/main.css

$(www)/%.html: $(cache)/%.rktd rkt/page-xexpr.rkt
	$(make-post-html) $< $(www) $@

$(www)/%.html: $(src)/non-posts/%.md rkt/make-non-post-html.rkt
	$(make-non-post-html) $< $(www) $@

$(www)/tags/%.html: $(cache)/tags/% rkt/page-xexpr.rkt
	$(make-tag-index) $< $(www) $@

$(www)/tags/index.html: $(tag-caches) rkt/page-xexpr.rkt rkt/make-tag-list.rkt
	$(make-tag-list) $(cache)/tags/ $(www) $@

$(www)/index.html: $(www)/tags/all.html rkt/page-xexpr.rkt rkt/make-tag-feed.rkt
	cp $< $@

$(www)/main.css: rkt/make-css.rkt
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
# GitHub Pages deploy

# Just set $(www) to /home/greg/src/greghendershott.github.com to
# build directly in that dir. Then commit and push in that repo.

######################################################################
# S3 bucket deploy

aws  := aws --profile greg
dest := s3://www.greghendershott.com
cfid := E2LPR1YW069SHG

.PHONY: deploy

deploy:
	$(aws) s3 sync --no-follow-symlinks $(www) $(dest)
	$(aws) cloudfront create-invalidation --distribution-id $(cfid) --paths "/*" > /dev/null
