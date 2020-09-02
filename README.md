![Release tag](https://img.shields.io/github/tag/hlissner/emacs-doom-themes.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/doom-themes-badge.svg?style=flat-square)](http://melpa.org/#/doom-themes)
[![Build Status](https://travis-ci.org/hlissner/emacs-doom-themes.svg?branch=master&style=flat-square)](https://travis-ci.org/hlissner/emacs-doom-themes)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

# doom-themes

DOOM Themes is an opinionated UI plugin and pack of themes extracted from my
[emacs.d], inspired by some of my favorite color themes.

[See the screenshots.][screenshots]

**Table of Contents**

- [Theme list](#theme-list)
- [Features](#features)
- [Install](#install)
    - [Doom Emacs](#doom-emacs)
    - [Manually](#manually)
- [Common Issues](#common-issues)
- [Contribute](#contribute)

## Theme list

- Flagship themes
  - [X] `doom-one`: doom-themes' flagship theme, inspired by [Atom]'s One Dark themes
  - [X] `doom-one-light`: light version of doom-one (thanks to [ztlevi])
  - [X] `doom-vibrant`: a slightly more vibrant version of `doom-one`
- Additional themes
  - [X] `doom-acario-dark`: an original dark theme (thanks to [gagbo])
  - [X] `doom-acario-light`: an original light theme (thanks to [gagbo])
  - [X] `doom-city-lights`: based on Atom's [City lights][city-lights] (thanks to [fuxialexander])
  - [X] `doom-challenger-deep`: based on Vim's [Challenger deep][challenger-deep] theme (thanks to [fuxialexander])
  - [X] `doom-dark+` ported from VS Code's [Dark+][dark+] theme (thanks to [ema2159])
  - [X] `doom-dracula`: an implementation of [Dracula][dracula] theme (thanks to [fuxialexander])
  - [X] `doom-ephemeral`: inspired in the Ephemeral Theme from [elenapan's dotfiles] (thanks to [karetsu])
  - [X] `doom-fairy-floss`: a candy colored Sublime theme by [sailorhg] (thanks to [ema2159])
  - [X] `doom-flatwhite`: a unique light theme ported from [Flatwhite Syntax][flatwhite] (thanks to [ShaneKilkelly])
  - [X] `doom-gruvbox`: adapted from Morhetz's [Gruvbox][gruvbox] (thanks to [JongW])
  - [X] `doom-gruvbox-light`: adapted from Morhetz's [Gruvbox][gruvbox] light variant (thanks for [jsoa])
  - [X] `doom-henna`: based on VS Code's [Henna][henna] (thanks to [jsoa])
  - [X] `doom-horizon`: ported from VS Code's [Horizon][horizon] (thanks to [karetsu])
  - [X] `doom-Iosvkem`: adapted from [Iosvkem][Iosvkem] (thanks to [neutaaaaan])
  - [X] `doom-laserwave`: a clean 80's synthwave / outrun theme inspired by VS Code's [laserwave][laserwave] (thanks to [hyakt])
  - [X] `doom-material` adapted from [Material Themes] (thanks to [tam5])
  - [X] `doom-manegarm`: an original autumn-inspired dark theme (thanks to [kenranunderscore])
  - [X] `doom-miramare`: a port of [Franbach's][franbach] [Miramare][miramare], a variant of gruvbox theme (thanks to [sagittaros])
  - [X] `doom-molokai`: a theme based on Texmate's Monokai
  - [X] `doom-monokai-classic`: port of [Monokai]'s Classic variant (thanks to [ema2159])
  - [X] `doom-monokai-pro`: port of [Monokai]'s Pro variant (thanks to [kadenbarlow]) 
  - [X] `doom-moonlight` ported from VS Code's [Moonlight Theme] (thanks to [Brettm12345])
  - [X] `doom-nord`: dark variant of [Nord][nord] (thanks to [fuxialexander])
  - [X] `doom-nord-light`: light variant of [Nord][nord] (thanks to [fuxialexander])
  - [X] `doom-nova`: adapted from [Nova] (thanks to [bigardone])
  - [X] `doom-oceanic-next`: adapted from [Oceanic Next] theme (thanks to [juanwolf])
  - [X] `doom-old-hope`: based on [An Old Hope] theme (thanks to [teesloane])
  - [X] `doom-opera`: an original dark theme (thanks to [jwintz])
  - [X] `doom-opera-light`: an original light theme (thanks to [jwintz])
  - [X] `doom-outrun-electric`: a neon colored theme inspired in VS Code's [Outrun Electric][outrun] (thanks to [ema2159])
  - [X] `doom-palenight` adapted from [Material Themes] (thanks to [Brettm12345])
  - [x] `doom-plain`: based on [plain][plain] (thanks to [mateossh])
  - [X] `doom-peacock`: based on Peacock from [daylerees' themes][daylerees] (thanks to [teesloane])
  - [X] `doom-rouge`: ported from [VSCode's Rouge Theme][rouge theme]  (thanks to [JordanFaust])
  - [X] `doom-snazzy`: a dark theme inspired in Atom's [Hyper Snazzy][snazzy] (thanks to [ar1a])
  - [X] `doom-solarized-dark`: dark variant of [Solarized][solarized] (thanks to [ema2159])
  - [X] `doom-solarized-light`: light variant of [Solarized][solarized] (thanks to [fuxialexander])
  - [X] `doom-sourcerer`: based on [Sourcerer][sourcerer] (thanks to [defphil])
  - [X] `doom-spacegrey`: [I'm sure you've heard of it][spacegrey] (thanks to [teesloane])
  - [x] `doom-tomorrow-day`: [Tomorrow][tomorrow]'s light variant (thanks to [emacswatcher])
  - [X] `doom-tomorrow-night`: one of the dark variants of [Tomorrow][tomorrow] (thanks to [emacswatcher])
  - [X] `doom-wilmersdorf`: port of Ian Pan's [Wilmersdorf] (thanks to [ema2159])
  - [X] `doom-zenburn`: port of the popular [Zenburn] theme (thanks to [jsoa])
  - [ ] `doom-mono-dark` / `doom-mono-light`: a minimalistic, monochromatic theme
  - [ ] `doom-tron`: based on Tron Legacy from [daylerees' themes][daylerees]

## Features

- `(doom-themes-visual-bell-config)`: flash the mode-line when the Emacs bell
  rings (i.e. an error occurs).
- `(doom-themes-neotree-config)`: a [neotree] theme that takes after [Atom]'s
  file drawer, and is simpler than the built in icon theme in neotree
  ([screenshot](/../screenshots/doom-one.png), [more details][wiki]).

  This requires `all-the-icons`' fonts to be installed: `M-x
  all-the-icons-install-fonts`
- `(doom-themes-treemacs-config)`: two [treemacs] icon themes, one that takes after
  [Atom]'s, and a second more colorful implementation (WIP).
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

## Themes customization
There are several themes which have their own customization options. For example, `doom-dark+` default modeline color can be changed with the `doom-dark+-blue-modeline` custom variable, or you can switch between `doom-gruvbox-light` variants with the `doom-gruvbox-light-variant` custom variable. 

## Install

### Doom Emacs

The built-in `:ui doom` module installs `doom-themes` for you, and loads
`doom-one` by default. You can change the theme by changing the `doom-theme`
variable from `~/.doom.d/config.el`:

``` emacs-lisp
(setq doom-theme 'doom-city-lights)
```

Or by loading it manually:

``` emacs-lisp
(load-theme 'doom-city-lights t)
```

### Manually

`doom-themes` is available on MELPA.

- [Straight](https://github.com/raxod502/straight.el) users can install it with
  `M-x straight-use-package RET doom-themes`
- And package.el users: `M-x package-install RET doom-themes`

Here is a example configuration for `doom-theme`:

```emacs-lisp
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
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


[An Old Hope]: https://github.com/mohkale/an-old-hope-theme
[AnthonyDiGirolamo]: https://github.com/AnthonyDiGirolamo
[Atom]: http://atom.io
[Nova]: https://trevordmiller.com/projects/nova
[airline-doom-molokai]: https://github.com/AnthonyDiGirolamo/airline-themes/raw/master/screenshots/airline-doom-molokai-theme.png
[airline-doom-one]: https://github.com/AnthonyDiGirolamo/airline-themes/raw/master/screenshots/airline-doom-one-theme.png
[airline-themes]: https://github.com/AnthonyDiGirolamo/airline-themes
[all-the-icons]: https://github.com/domtronn/all-the-icons.el
[ar1a]: https://github.com/ar1a
[bigardone]: https://github.com/bigardone
[Brettm12345]: https://github.com/Brettm12345
[challenger-deep]: https://github.com/challenger-deep-theme/vim
[city-lights]: http://citylights.xyz/
[dark+]: https://github.com/microsoft/vscode/blob/master/extensions/theme-defaults/themes/dark_plus.json
[daylerees]: http://daylerees.github.io/
[defphil]: https://github.com/defphil
[dracula]: https://draculatheme.com/
[elenapan's dotfiles]: https://github.com/elenapan/dotfiles
[ema2159]: https://github.com/ema2159
[emacs.d]: https://github.com/hlissner/.emacs.d
[emacswatcher]: https://github.com/emacswatcher
[flatwhite]: https://github.com/biletskyy/flatwhite-syntax
[franbach]: https://github.com/franbach
[fuxialexander]: https://github.com/fuxialexander
[gagbo]: https://github.com/gagbo 
[gruvbox]: https://github.com/morhetz/gruvbox
[henna]: https://github.com/httpsterio/vscode-henna
[horizon]: https://github.com/jolaleye/horizon-theme-vscode
[hlinum]: https://melpa.org/#/hlinum
[issues]: https://github.com/hlissner/emacs-doom-themes/issues
[Iosvkem]: https://github.com/neutaaaaan/iosvkem
[juanwolf]: https://github.com/juanwolf
[JongW]: https://github.com/JongW
[jsoa]: https://github.com/jsoa
[jwintz]: https://github.com/jwintz
[kadenbarlow]: https://github.com/kadenbarlow
[karetsu]: https://github.com/karetsu
[kenranunderscore]: https://github.com/kenranunderscore
[mateossh]: https://github.com/mateossh
[Material Themes]: https://github.com/equinusocio/vsc-material-theme
[miramare]: https://github.com/franbach/miramare
[Moonlight Theme]: https://github.com/atomiks/moonlight-vscode-theme
[mode-line]: https://github.com/hlissner/.emacs.d/blob/master/modules/ui/doom-modeline/config.el
[Monokai]: https://monokai.pro/
[neotree]: https://github.com/jaypei/emacs-neotree
[nlinum-hl]: https://github.com/hlissner/emacs-nlinum-hl
[neutaaaaan]: https://github.com/neutaaaaan
[nord]: https://www.nordtheme.com/
[Oceanic Next]: https://github.com/voronianski/oceanic-next-color-scheme
[outrun]: https://github.com/samrap/outrun-theme-vscode
[plain]: https://github.com/gko/plain
[sagittaros]: https://github.com/sagittaros/
[sailorhg]: https://sailorhg.github.io/fairyfloss/
[screenshots]: https://github.com/hlissner/emacs-doom-themes/tree/screenshots
[ShaneKilkelly]: https://github.com/ShaneKilkelly
[snazzy]: https://github.com/sindresorhus/hyper-snazzy
[solarized]: http://ethanschoonover.com/solarized
[solaire-mode]: https://github.com/hlissner/emacs-solaire-mode
[sourcerer]: https://github.com/xero/sourcerer.vim
[spacegrey]: http://kkga.github.io/spacegray/
[tam5]: https://github.com/tam5
[teesloane]: https://github.com/teesloane
[tomorrow]: https://github.com/ChrisKempson/Tomorrow-Theme
[treemacs]: https://github.com/Alexander-Miller/treemacs
[wiki]: https://github.com/hlissner/emacs-doom-themes/wiki
[Wilmersdorf]: https://github.com/ianpan870102/wilmersdorf-emacs-theme
[ztlevi]: https://github.com/ztlevi
[laserwave]: https://github.com/Jaredk3nt/laserwave
[hyakt]: https://github.com/hyakt
[rouge theme]: https://github.com/josefaidt/rouge-theme 
[JordanFaust]: https://github.com/JordanFaust
[Zenburn]: https://github.com/bbatsov/zenburn-emacs
