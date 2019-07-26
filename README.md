![Release tag](https://img.shields.io/github/tag/hlissner/emacs-doom-themes.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/doom-themes-badge.svg?style=flat-square)](http://melpa.org/#/doom-themes)
[![Build Status](https://travis-ci.org/hlissner/emacs-doom-themes.svg?branch=master&style=flat-square)](https://travis-ci.org/hlissner/emacs-doom-themes)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

# doom-themes

DOOM Themes is an opinionated UI plugin and pack of themes extracted from my
[emacs.d], inspired by some of my favorite color themes.

[See the screenshots.][screenshots]

**Table of Contents**

- [Features](#features)
- [Install](#install)
- [Common Issues](#common-issues)
- [Contribute](#contribute)

## Features

- **Color themes:**
  - Flagship themes
    - [X] `doom-one`: doom-themes' flagship theme, inspired by [Atom]'s One Dark themes
    - [X] `doom-one-light`: light version of doom-one (thanks to [ztlevi])
    - [X] `doom-vibrant`: a slightly more vibrant version of `doom-one`
  - Additional themes
    - [X] `doom-city-lights` (thanks to [fuxialexander])
    - [X] `doom-dracula` (thanks to [fuxialexander])
    - [X] `doom-gruvbox`: adapted from Mohretz's [Gruvbox][gruvbox] (thanks to [JongW])
    - [X] `doom-Iosvkem`: adapted from [Iosvkem][Iosvkem] (thanks to [neutaaaaan])
    - [X] `doom-molokai`: based on Textmate's monokai
    - [X] `doom-nord` (thanks to [fuxialexander])
    - [X] `doom-nord-light` (thanks to [fuxialexander])
    - [X] `doom-opera` (thanks to [jwintz])
    - [X] `doom-opera-light` (thanks to [jwintz])
    - [X] `doom-nova`: adapted from [Nova] (thanks to [bigardone])
    - [X] `doom-peacock`: based on Peacock from [daylerees' themes][daylerees] (thanks to [teesloane])
    - [X] `doom-solarized-dark`: dark variant of [Solarized][solarized] (thanks to [ema2159])
    - [X] `doom-solarized-light`: light variant of [Solarized][solarized] (thanks to [fuxialexander])
    - [X] `doom-sourcerer`: based on [Sourcerer][sourcerer] (thanks to [defphil])
    - [X] `doom-spacegrey`: [I'm sure you've heard of it][spacegrey] (thanks to [teesloane])
    - [X] `doom-tomorrow-night`: by [Chris Kempson][tomorrow]
    - [x] `doom-tomorrow-day`: by [Chris Kempson][tomorrow] (thanks to [emacswatcher])
    - [ ] `doom-mono-dark` / `doom-mono-light`: a minimalistic, monochromatic theme
    - [ ] `doom-tron`: based on Tron Legacy from [daylerees' themes][daylerees]
- **Included features:**
  - `(doom-themes-visual-bell-config)`: flash the mode-line when the Emacs bell
    rings (i.e. an error occurs).
  - `(doom-themes-neotree-config)`: a [neotree] theme that takes after [Atom]'s
    file drawer, and is simpler than the built in icon theme in neotree
    ([screenshot](/../screenshots/doom-one.png), [more details][wiki]).

    This requires `all-the-icons`' fonts to be installed: `M-x
    all-the-icons-install-fonts`
  - `(doom-themes-treemacs-config)`: a [treemacs] icon theme that takes after
    [Atom]'s (WIP).
  - `(doom-themes-org-config)`: corrects and improves some of org-mode's native
    fontification.
    -  Re-set `org-todo' & `org-headline-done' faces to make them respect
       underlying faces (i.e. don't override the :height or :background of
       underlying faces).
    -  Make statistic cookies respect underlying faces.
    -  Fontify item bullets (make them stand out)
    -  Fontify item checkboxes (and when they're marked done), like TODOs that
       are marked done.
    -  Fontify dividers/separators (5+ dashes)
    -  Fontify #hashtags and @at-tags, for personal convenience; see
       `doom-org-special-tags` to disable this.
- **Resources that may interest you:**
  - To get dimmed sidebars and brighter source buffers (like in the
    [screenshots]), see [solaire-mode].
  - To get line number highlighting for `nlinum`, set
    `nlinum-highlight-current-line` to non-nil (or see [hlinum] for `linum`).
  - [AnthonyDiGirolamo] added doom-one ([screenshot][airline-doom-one]) and
    doom-molokai ([screenshot][airline-doom-molokai]) skins to
    [airline-themes][airline-themes].
  - I'm working on making my modeline more accessible. In the meantime, check
    out [my mode-line configuration][mode-line] in my [emacs.d].

## Install

`M-x package-install RET doom-themes`

A comprehensive configuration example:

```emacs-lisp
(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)
```

[The wiki contains details for customizing the neotree theme][wiki].

## Common Issues

+ If you use `nlinum` or `linum` in org-mode, the larger headline sizes in some
  themes [could bleed into the line numbers](https://github.com/hlissner/emacs-doom-themes/issues/86).

  Fix this by setting :height explicitly for your line number plugins, after
  you've loaded the theme. e.g.

  ```emacs-lisp
  (let ((height (face-attribute 'default :height)))
    ;; for all linum/nlinum users
    (set-face-attribute 'linum nil :height height)
    ;; only for `linum-relative' users:
    (set-face-attribute 'linum-relative-current-face nil :height height)
    ;; only for `nlinum-relative' users:
    (set-face-attribute 'nlinum-relative-current-face nil :height height))
  ```

## Contribute

I welcome contributions of any kind, be they pull requests, bug reports or elisp
pointers. Additional theme and plugin support requests are welcome too.


[AnthonyDiGirolamo]: https://github.com/AnthonyDiGirolamo
[Atom]: http://atom.io
[Nova]: https://trevordmiller.com/projects/nova
[airline-doom-molokai]: https://github.com/AnthonyDiGirolamo/airline-themes/raw/master/screenshots/airline-doom-molokai-theme.png
[airline-doom-one]: https://github.com/AnthonyDiGirolamo/airline-themes/raw/master/screenshots/airline-doom-one-theme.png
[airline-themes]: https://github.com/AnthonyDiGirolamo/airline-themes
[all-the-icons]: https://github.com/domtronn/all-the-icons.el
[bigardone]: https://github.com/bigardone
[daylerees]: http://daylerees.github.io/
[defphil]: https://github.com/defphil
[ema2159]: https://github.com/ema2159
[emacs.d]: https://github.com/hlissner/.emacs.d
[emacswatcher]: https://github.com/emacswatcher
[fuxialexander]: https://github.com/fuxialexander
[gruvbox]: https://github.com/morhetz/gruvbox
[hlinum]: https://melpa.org/#/hlinum
[issues]: https://github.com/hlissner/emacs-doom-themes/issues
[Iosvkem]: https://github.com/neutaaaaan/iosvkem
[jwintz]: https://github.com/jwintz
[JongW]: https://github.com/JongW
[mode-line]: https://github.com/hlissner/.emacs.d/blob/master/modules/ui/doom-modeline/config.el
[neotree]: https://github.com/jaypei/emacs-neotree
[nlinum-hl]: https://github.com/hlissner/emacs-nlinum-hl
[neutaaaaan]: https://github.com/neutaaaaan
[screenshots]: https://github.com/hlissner/emacs-doom-themes/tree/screenshots
[solarized]: http://ethanschoonover.com/solarized
[solaire-mode]: https://github.com/hlissner/emacs-solaire-mode
[sourcerer]: https://github.com/xero/sourcerer.vim
[spacegrey]: http://kkga.github.io/spacegray/
[teesloane]: https://github.com/teesloane
[tomorrow]: https://github.com/ChrisKempson/Tomorrow-Theme
[treemacs]: https://github.com/Alexander-Miller/treemacs
[wiki]: https://github.com/hlissner/emacs-doom-themes/wiki
[ztlevi]: https://github.com/ztlevi
