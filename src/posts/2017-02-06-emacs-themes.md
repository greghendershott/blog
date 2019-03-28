    Title: Emacs Themes
    Date: 2017-02-08T00:00:00
    Tags: Emacs

Until a few months ago I didn't use Emacs [themes]. A
`custom-set-faces` form in my init file gradually accumulated face
specs like a lint-roller.

[themes]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html

Then I started to use [Solarized]. Mostly light, sometimes dark.
Switching between them using <kbd>M-x load-theme</kbd> worked fine.

[Solarized]: https://melpa.org/#/solarized-theme

Later I liked the look of [Material]. Although too high-contrast to
use full-time, it works well in certain situations.

[Material]: https://melpa.org/#/material-theme

After I installed it I had two annoyances:

1. I didn't love the 3D "button" look it gives org-mode headings. Must
   tweak.

2. Switching between the Solarized and Material themes using
   `load-theme` definitely did _not_ work well: If the old theme
   defined a face, but the new theme did not, the old face would
   remain in effect. So for example I might switch to Material then
   back to Solarized, and get a weird mix of mostly Solarized but with
   Material org headings.

Here's what I'm doing to address both issues.

<!-- more -->

## Multiple themes

What I didn't understand at first is that Emacs supports many themes
enabled simultaneously. The variable `custom-enabled-themes` is
plural, a list. As a result, `load-theme` doesn't mean "use this
one theme" -- it means, "layer this theme on top of those already
enabled".

If want just one theme at a time? I must first `disable-theme` each
currently-enabled theme. A little function/command:[^prefix]

[^prefix]: In my init file I do the customary thing: Prefix identifiers with my initials and a slash: `gh/`.

```elisp
(defun gh/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))
```

So fine, but, now I need to issue two commands. Give me convenience or
give me death.

One idea would be to make a new `gh/load-theme` command that composes
`gh/disable-all-themes` and `load-theme`. But will I remember to
<kbd>M-x</kbd> _that_ instead?

Another idea would be to advise `load-theme` so I can continue doing
<kbd>M-x load-theme</kbd>.

I did that. As a bonus, such advice can also provide a kind of "theme
hooks" feature -- a way to call a function after a specific theme is
loaded.

## Emacs init file

Here are some snippets from my `~/.emacs.d/init.el`.

### Advice and hooks

```elisp
;;; Theme hooks

(defvar gh/theme-hooks nil
  "((theme-id . function) ...)")

(defun gh/add-theme-hook (theme-id hook-func)
  (add-to-list 'gh/theme-hooks (cons theme-id hook-func)))

(defun gh/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `gh/add-theme-hook'."
  (unless no-enable
    (gh/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id gh/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'gh/load-theme-advice)
```

### Packages

Now my configuration of each theme package can add a theme hook to
make tweaks.

I manage package install and config in my init file using the
wonderful [use-package].

[use-package]: https://melpa.org/#/use-package

My personal preferences for Material:

```elisp
(use-package material-theme
  :ensure t
  :defer t
  :init
  (defun gh/material-theme-hook ()
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground))
    (loop for n from 1 to 8
          do (set-face-attribute (intern-soft (format "org-level-%s" n))
                                 nil
                                 :height     'unspecified
                                 :background 'unspecified
                                 :box        'unspecified)))
  (gh/add-theme-hook 'material       #'gh/material-theme-hook)
  (gh/add-theme-hook 'material-light #'gh/material-theme-hook))
```

And for Solarized:

```elisp
(use-package solarized
  :ensure solarized-theme
  :defer t
  :init
  (defun gh/solarized-theme-hook ()
    (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground)))
  (gh/add-theme-hook 'solarized-dark  #'gh/solarized-theme-hook)
  (gh/add-theme-hook 'solarized-light #'gh/solarized-theme-hook)
  :config
  (setq solarized-use-variable-pitch nil
        solarized-use-less-bold t
        solarized-use-more-italic nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))
```

## Hydra

Finally, let's use the [hydra] package ...

[hydra]: https://melpa.org/#/hydra

```elisp
(use-package hydra
  :ensure t
  :config
  (setq hydra-lv nil) ;use echo area)
```

... to make it easy to switch among themes with a single key press ...

```elisp
(defhydra gh/themes-hydra (:hint nil :color pink)
  "
Themes

^Solarized^   ^Material^   ^Other^
----------------------------------------------------
_s_: Dark     _m_: Dark    _z_: Zenburn  _DEL_: none
_S_: Light    _M_: Light
"
  ("s" (load-theme 'solarized-dark  t))
  ("S" (load-theme 'solarized-light t))
  ("m" (load-theme 'material        t))
  ("M" (load-theme 'material-light  t))
  ("z" (load-theme 'zenburn         t))
  ("DEL" (gh/disable-all-themes))
  ("RET" nil "done" :color blue))

(bind-keys ("C-c w t"  . gh/themes-hydra/body))
```

Incidentally I bind this hydra to <kbd>C-c w t</kbd> following an idea
I saw in [lunaryorn's init file]:

[lunaryorn's init file]: https://github.com/lunaryorn/.emacs.d/blob/master/init.el#L302-L427

1. Use `bind-keys` (from `use-package`) to put commands on
   <kbd>C-c</kbd> prefixes, with an additional prefix key to group
   related commands. So for example window-related commands have a
   <kbd>C-c w</kbd> prefix.

2. Use [`which-key`] to label prefixes and bindings.

[`which-key`]: https://melpa.org/#/which-key

Anyway, the upshot is I can type <kbd>C-c w t</kbd>, then keep
pressing single hydra keys to switch among themes. Upon finding the
optimal mood management, tap <kbd>RETURN</kbd> to exit the hydra.

---
