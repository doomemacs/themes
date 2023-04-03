![Release tag](https://img.shields.io/github/tag/doomemacs/themes.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/doom-themes-badge.svg?style=flat-square)](http://melpa.org/#/doom-themes)
![Build status](https://img.shields.io/github/workflow/status/doomemacs/themes/CI/master?style=flat-square)
[![Discord Server](https://img.shields.io/discord/406534637242810369?color=738adb&label=Discord&logo=discord&logoColor=white&style=flat-square)](https://doomemacs.org/discord)
[![Discourse server](https://img.shields.io/discourse/users?server=https%3A%2F%2Fdiscourse.doomemacs.org&logo=discourse&label=Discourse&style=flat-square&color=9cf)](https://discourse.doomemacs.org)

# Doom Emacs' Theme Pack

A theme megapack for GNU Emacs, inspired by community favorites. Special
attention is given for [Doom Emacs](https://doomemacs.org) and [solaire-mode]
support, but will work fine anywhere else. 

[See the screenshots.][screenshots]


# Table of Contents
- [Install](#install)
    - [Doom Emacs](#doom-emacs)
    - [Manually + use-package](#manually--use-package)
- [Theme list](#theme-list)
- [Extensions](#extensions)
- [Complementary plugins](#complementary-plugins)
- [Customization](#customization)
- [Contribute](#contribute)

## Install

### Doom Emacs

The built-in `:ui doom` module installs and configures `doom-themes` for you,
and loads `doom-one` by default. To change the theme, change `doom-theme`:

``` emacs-lisp
;; in ~/.doom.d/config.el
(setq doom-theme 'doom-city-lights)
```

### Manually + `use-package`

`doom-themes` is available on MELPA. Here is an example configuration with some
common defaults laid out:

```emacs-lisp
(use-package doom-themes
  :ensure t
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
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
```


## Theme list

This megapack's first and flagship theme is `doom-one` (inspired by Atom One
Dark), but also includes 67 themes submitted to us by the Emacs community. We
welcome PRs to help us maintain and address inconsistencies in them.

| Name                              | Source                                                                                 | Description                                                                                                                       |
|-----------------------------------|----------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| doom-1337                         | [link](https://github.com/MarkMichos/1337-Scheme)                                      | ported from VSCode's 1337 theme (ported by [@ccmywish](https://github.com/ccmywish))                                              |
| doom-acario-dark                  | original                                                                               | an original dark theme (ported by [@gagbo](https://github.com/gagbo))                                                             |
| doom-acario-light                 | original                                                                               | an original light theme (ported by [@gagbo](https://github.com/gagbo))                                                            |
| doom-ayu-dark                     | [link](https://github.com/dempfi/ayu)                                                  | inspired by Ayu Dark (ported by [@ashton](https://github.com/ashton))                                                             |
| doom-ayu-light                    | [link](https://github.com/dempfi/ayu)                                                  | inspirted by Ayu Light (ported by [@LoveSponge](https://github.com/LoveSponge))                                                   |
| doom-ayu-mirage                   | [link](https://github.com/dempfi/ayu)                                                  | inspired by Ayu Mirage (ported by [@LoveSponge](https://github.com/LoveSponge))                                                   |
| doom-badger                       | [link](https://github.com/ccann/badger-theme/)                                         | inspired by cann's Badger colorscheme (ported by [@jsoa](https://github.com/jsoa))                                                |
| doom-challenger-deep              | [link](https://github.com/challenger-deep-theme/vim)                                   | inspired by Vim's Challenger Deep theme (ported by [@fuxialexander](https://github.com/fuxialexander))                            |
| doom-city-lights                  | [link](https://citylights.xyz)                                                         | inspired by Atom's City Lights theme (ported by [@fuxialexander](https://github.com/fuxialexander))                               |
| doom-dark+                        | n/a                                                                                    | ported from equinusocio's VSCode Theme, dark+ (ported by [@ema2159](https://github.com/ema2159))                                  |
| doom-dracula                      | [link](https://draculatheme.com)                                                       | inspired by the popular Dracula theme (ported by [@fuxialexander](https://github.com/fuxialexander))                              |
| doom-earl-grey                    | original                                                                               | a gentle color scheme, for code (ported by [@JuneKelly](https://github.com/JuneKelly))                                            |
| doom-ephemeral                    | [link](https://github.com/elenapan/dotfiles)                                           | inspired by the Ephemeral Theme from elenapan's dotfiles (ported by [@karetsu](https://github.com/karetsu))                       |
| doom-fairy-floss                  | [link](https://github.com/sailorhg/fairyfloss)                                         | a candy colored theme by sailorhg (ported by [@ema2159](https://github.com/ema2159))                                              |
| doom-feather-dark                 | original                                                                               | a candy colored theme by sailorhg (ported by [@Plunne](https://gitlab.com/Plunne))                                                |
| doom-feather-light                | original                                                                               | a candy colored theme by sailorhg (ported by [@Plunne](https://gitlab.com/Plunne))                                                |
| doom-flatwhite                    | [link](https://github.com/biletskyy/flatwhite-syntax)                                  | inspired by Atom's Flatwhite Syntax theme (ported by [@JuneKelly](https://github.com/JuneKelly))                                  |
| doom-gruvbox                      | [link](https://github.com/morhetz/gruvbox)                                             | inspired by morhetz's Gruvbox (ported by [@JongW](https://github.com/JongW))                                                      |
| doom-gruvbox-light                | [link](https://github.com/morhetz/gruvbox)                                             | inspired by morhetz's Gruvbox (light) (ported by [@jsoa](https://github.com/jsoa))                                                |
| doom-henna                        | [link](https://github.com/httpsterio/vscode-henna)                                     | based on VSCode's Henna theme (ported by [@jsoa](https://github.com/jsoa))                                                        |
| doom-homage-black                 | original                                                                               | a minimalistic, colorless theme inspired by eziam, tao, and jbeans (ported by [@mskorzhinskiy](https://github.com/mskorzhinskiy)) |
| doom-homage-white                 | original                                                                               | minimal white theme inspired by editors from 2000s (ported by [@mskorzhinskiy](https://github.com/mskorzhinskiy))                 |
| doom-horizon                      | [link](https://github.com/aodhneine/horizon-theme.el)                                  | ported from VSCode Horizon (ported by [@karetsu](https://github.com/karetsu))                                                     |
| doom-Iosvkem                      | [link](https://github.com/neutaaaaan/iosvkem)                                          | ported from the default dark theme for Adobe Brackets (ported by [@neutaaaaan](https://github.com/neutaaaaan))                    |
| doom-ir-black                     | [link](https://github.com/twerth/ir_black)                                             | ported from Vim's ir_black colorscheme (ported by [@legendre6891](https://github.com/legendre6891))                               |
| doom-lantern                      | [link](https://github.com/Gitleptune/lantern-theme)                                    | based on Gitleptune's Lantern theme (ported by [@paladhammika](https://github.com/paladhammika))                                  |
| doom-laserwave                    | [link](https://github.com/Jaredk3nt/laserwave)                                         | a clean synthwave/outrun theme inspired by VSCode's Laserwave (ported by [@hyakt](https://github.com/hyakt))                      |
| doom-manegarm                     | original                                                                               | an original autumn-inspired dark theme (ported by [@kenranunderscore](https://github.com/kenranunderscore))                       |
| doom-material                     | [link](https://github.com/equinusocio/vsc-material-theme)                              | adapted from equinusocio's Material themes (ported by [@tam5](https://github.com/tam5))                                           |
| doom-material-dark                | [link](https://github.com/xrei/material-dark-vscode)                                   | inspired by Material Theme by xrei (ported by [@trev-dev](https://github.com/trev-dev))                                           |
| doom-meltbus                      | [link](https://github.com/equinusocio/vsc-material-theme)                              | a dark (mostly) monochromatic theme (ported by [@spacefrogg](https://github.com/spacefrogg))                                      |
| doom-miramare                     | [link](https://github.com/franbach/miramare)                                           | a port of Franbach's Miramare theme; a variant of Grubox (ported by [@sagittaros](https://github.com/sagittaros))                 |
| doom-molokai                      | [link](https://github.com/tomasr/molokai)                                              | inspired by Tomas Restrepo's Molokai (ported by [@hlissner](https://github.com/hlissner))                                         |
| doom-monokai-classic              | [link](https://monokai.pro)                                                            | port of Monokai Classic (ported by [@ema2159](https://github.com/ema2159))                                                        |
| doom-monokai-machine              | [link](https://monokai.pro)                                                            | port of Monokai Classic (ported by [@minikN](https://github.com/minikN))                                                          |
| doom-monokai-octagon              | [link](https://monokai.pro)                                                            | port of Monokai Octagon (ported by [@minikN](https://github.com/minikN))                                                          |
| doom-monokai-pro                  | [link](https://monokai.pro)                                                            | Port of Monokai Pro (ported by [@minikN](https://github.com/minikN))                                                              |
| doom-monokai-ristretto            | [link](https://monokai.pro)                                                            | Port of Monokai Ristretto (ported by [@minikN](https://github.com/minikN))                                                        |
| doom-monokai-spectrum             | [link](https://monokai.pro)                                                            | port of Monokai Spectrum (ported by [@minikN](https://github.com/minikN))                                                         |
| doom-moonlight                    | [link](https://github.com/atomiks/moonlight-vscode-theme)                              | inspired by VS code's Moonlight (ported by [@Brettm12345](https://github.com/Brettm12345))                                        |
| doom-nord                         | [link](https://www.nordtheme.com)                                                      | dark variant of Nord (ported by [@fuxialexander](https://github.com/fuxialexander))                                               |
| doom-nord-aurora                  | [link](https://www.nordtheme.com)                                                      | a light variant of Nord (ported by [@MoskitoHero](https://github.com/MoskitoHero))                                                |
| doom-nord-light                   | [link](https://github.com/arcticicestudio/nord)                                        | light variant of Nord (ported by [@fuxialexander](https://github.com/fuxialexander))                                              |
| doom-nova                         | [link](https://github.com/trevordmiller/nova-colors)                                   | inspired by Trevord Miller's Nova (ported by [@bigardone](https://github.com/bigardone))                                          |
| doom-oceanic-next                 | [link](https://github.com/voronianski/oceanic-next-color-scheme)                       | inspired by Oceanic Next (ported by [@juanwolf](https://github.com/juanwolf))                                                     |
| doom-old-hope                     | [link](https://github.com/jesseleite/an-old-hope-syntax-atom)                          | inspired by An Old Hope, in a galaxy far far away (ported by [@teesloane](https://github.com/teesloane))                          |
| doom-one                          | [link](https://github.com/atom/one-dark-ui)                                            | inspired by Atom One Dark (ported by [@hlissner](https://github.com/hlissner))                                                    |
| doom-one-light                    | [link](https://github.com/atom/one-light-ui)                                           | inspired by Atom One Light (ported by [@ztlevi](https://github.com/ztlevi))                                                       |
| doom-opera                        | original                                                                               | an original light theme (ported by [@jwintz](https://github.com/jwintz))                                                          |
| doom-opera-light                  | original                                                                               | an original light theme (ported by [@jwintz](https://github.com/jwintz))                                                          |
| doom-outrun-electric              | [link](https://github.com/samrap/outrun-theme-vscode)                                  | a high contrast, neon theme inspired by Outrun Electric on VSCode (ported by [@ema2159](https://github.com/ema2159))              |
| doom-palenight                    | [link](https://github.com/equinusocio/vsc-material-theme)                              | adapted from equinusocio's Material themes (ported by [@Brettm12345](https://github.com/Brettm12345))                             |
| doom-peacock                      | [link](https://github.com/daylerees/colour-schemes/blob/master/emacs/peacock-theme.el) | inspired by daylerees' Peacock (ported by [@teesloane](https://github.com/teesloane))                                             |
| doom-pine                         | [link](https://github.com/morhetz/gruvbox)                                             | a green-tinged Gruvbox (by [@RomanHargrave](https://github.com/RomanHargrave))                                           |
| doom-plain                        | [link](https://github.com/gko/plain/)                                                  | inspired by gko's plain theme for VSCode (ported by [@das-s](https://github.com/das-s))                                           |
| doom-plain-dark                   | [link](https://github.com/gko/plain/)                                                  | inspired by gko's plain theme for VSCode (ported by [@das-s](https://github.com/das-s))                                           |
| doom-rouge                        | [link](https://github.com/josefaidt/rouge-theme)                                       | ported from VSCode's Rouge Theme (ported by [@das-s](https://github.com/das-s))                                                   |
| doom-shades-of-purple             | [link](https://github.com/ahmadawais/shades-of-purple-vscode)                          | a port of VSCode's Shades of Purple (ported by [@jwbaldwin](https://github.com/jwbaldwin))                                        |
| doom-snazzy                       | [link](https://github.com/sindresorhus/hyper-snazzy)                                   | inspired by Hyper Snazzy (ported by [@ar1a](https://github.com/ar1a))                                                             |
| doom-solarized-dark               | [link](https://ethanschoonover.com/solarized)                                          | a dark variant of Solarized (ported by [@ema2159](https://github.com/ema2159))                                                    |
| doom-solarized-dark-high-contrast | [link](https://ethanschoonover.com/solarized)                                          | a high-contrast variant of Solarized Dark (ported by [@jmorag](https://github.com/jmorag))                                        |
| doom-solarized-light              | [link](https://ethanschoonover.com/solarized)                                          | a light variant of Solarized (ported by [@fuxialexander](https://github.com/fuxialexander))                                       |
| doom-sourcerer                    | [link](https://github.com/xero/sourcerer)                                              | a port of xero's Sourcerer (ported by [@fm0xb](https://github.com/fm0xb))                                                         |
| doom-spacegrey                    | [link](http://kkga.github.io/spacegray/)                                               | I'm sure you've heard of it (ported by [@teesloane](https://github.com/teesloane))                                                |
| doom-tokyo-night                  | [link](https://github.com/enkia/tokyo-night-vscode-theme)                              | inspired by VSCode's Tokyo Night theme (ported by [@FosterHangdaan](https://github.com/FosterHangdaan))                           |
| doom-tomorrow-day                 | [link](https://github.com/ChrisKempson/Tomorrow-Theme)                                 | a light variant of Tomorrow (ported by [@emacswatcher](https://github.com/emacswatcher))                                          |
| doom-tomorrow-night               | [link](https://github.com/ChrisKempson/Tomorrow-Theme)                                 | One of the dark variants of Tomorrow (ported by [@hlissner](https://github.com/hlissner))                                         |
| doom-vibrant                      | doom-one                                                                               | a more vibrant variant of doom-one (ported by [@hlissner](https://github.com/hlissner))                                           |
| doom-wilmersdorf                  | [link](https://github.com/ianpan870102/wilmersdorf-emacs-theme)                        | port of Ian Pan's Wilmersdorf (ported by [@ema2159](https://github.com/ema2159))                                                  |
| doom-xcode                        | Xcode.app                                                                              | based off of Apple's Xcode Dark Theme (ported by [@kadenbarlow](https://github.com/kadenbarlow))                                  |
| doom-zenburn                      | [link](https://github.com/bbatsov/zenburn-emacs)                                       | port of the popular Zenburn theme (ported by [@jsoa](https://github.com/jsoa))                                                    |


## Extensions
[Check out the wiki for details on customizing our neotree/treemacs/etc
extensions][wiki].

- `(doom-themes-visual-bell-config)`: flash the mode-line when the Emacs bell
  rings (i.e. an error occurs). May not be compatible with all mode line
  plugins.
- `(doom-themes-neotree-config)`: a [neotree] theme that takes after [Atom]'s
  file drawer; a more minimalistic icon theme plus variable pitch file/directory
  labels, as seen [in the doom-one screenshot](/../screenshots/doom-one.png).

  (This requires `all-the-icons`' fonts to be installed: `M-x
  all-the-icons-install-fonts`)
- `(doom-themes-treemacs-config)`: two [treemacs] icon themes, one that takes after
  [Atom]'s, and a second more colorful implementation.
- `(doom-themes-org-config)`: corrects and improves some of org-mode's native
  fontification issues.
  -  Re-set `org-todo` & `org-headline-done` faces to make them respect
     underlying faces (i.e. don't override the :height or :background of
     underlying faces).
  -  Make statistic cookies respect underlying faces.
  -  Fontify item bullets (make them stand out)
  -  Fontify item checkboxes (and when they're marked done), like TODOs that
     are marked done.
  -  Fontify dividers/separators (5+ dashes)
  -  Fontify #hashtags and @at-tags, for personal convenience; see
     `doom-org-special-tags` to disable this.
     
## Complementary plugins
The following plugins complement our themes:

- To get dimmed sidebars and brighter source buffers (like in the
  [screenshots]), see [solaire-mode].
- [AnthonyDiGirolamo] added doom-one ([screenshot][airline-doom-one]) and
  doom-molokai ([screenshot][airline-doom-molokai]) skins to
  [airline-themes][airline-themes].
- The modeline in the screenshots is
  [doom-modeline](https://github.com/seagle0128/doom-modeline).
     
## Customization
There are three ways to customize themes in this package:

1. Explore the available variables provided by our themes, starting with the
   ones provided for all packages:
   + `doom-themes-enable-bold` (default: `t`): if `nil`, disables bolding as
     much as possible (only affects faces that our theme supports; it won't
     catch them all).
   + `doom-themes-enable-italic` (default: `t`): if `nil`, disables
     italicization as much as possible (only affects faces that our theme
     supports; it won't catch them all).
   + `doom-themes-padded-modeline` (default: `nil`): if `t`, pad the mode-line
     in 4px on each side. Can also be set to an integer to specify the exact
     padding.  or `M-x customize-group RET doom-themes` to explore them.
  
2. Use the `custom-set-faces` macro (Doom users should use `custom-set-faces!`
   instead) to customize any face. e.g.
  
   ```elisp
   ;; Must be used *after* the theme is loaded
   (custom-set-faces
     `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
     `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))
   ```
  
3. Copy your favorite theme into your `custom-theme-directory` (normally
   `~/.emacs.d/`, or `~/.doom.d/themes` for Doom users), and tweak it there.
   
  
## Contribute

PRs are welcome to maintain our themes, including additional theme and plugin
support.


[An Old Hope]: https://github.com/mohkale/an-old-hope-theme
[AnthonyDiGirolamo]: https://github.com/AnthonyDiGirolamo
[Atom]: http://atom.io
[Ayu]: https://github.com/ayu-theme/ayu-colors
[Nova]: https://trevordmiller.com/projects/nova
[airline-doom-molokai]: https://github.com/AnthonyDiGirolamo/airline-themes/raw/master/screenshots/airline-doom-molokai-theme.png
[airline-doom-one]: https://github.com/AnthonyDiGirolamo/airline-themes/raw/master/screenshots/airline-doom-one-theme.png
[airline-themes]: https://github.com/AnthonyDiGirolamo/airline-themes
[all-the-icons]: https://github.com/domtronn/all-the-icons.el
[ar1a]: https://github.com/ar1a
[ashton]: https://github.com/ashton
[bigardone]: https://github.com/bigardone
[Brettm12345]: https://github.com/Brettm12345
[challenger-deep]: https://github.com/challenger-deep-theme/vim
[city-lights]: http://citylights.xyz/
[dark+]: https://github.com/microsoft/vscode/blob/master/extensions/theme-defaults/themes/dark_plus.json
[das-s]: https://github.com/das-s
[daylerees]: http://daylerees.github.io/
[defphil]: https://github.com/defphil
[Dracula]: https://draculatheme.com/
[elenapan's dotfiles]: https://github.com/elenapan/dotfiles
[ema2159]: https://github.com/ema2159
[emacs.d]: https://github.com/hlissner/.emacs.d
[emacswatcher]: https://github.com/emacswatcher
[flatwhite]: https://github.com/biletskyy/flatwhite-syntax
[franbach]: https://github.com/franbach
[fuxialexander]: https://github.com/fuxialexander
[gagbo]: https://github.com/gagbo
[Gruvbox]: https://github.com/morhetz/gruvbox
[Henna]: https://github.com/httpsterio/vscode-henna
[Horizon]: https://github.com/jolaleye/horizon-theme-vscode
[hlinum]: https://melpa.org/#/hlinum
[issues]: https://github.com/doomemacs/themes/issues
[Iosvkem]: https://github.com/neutaaaaan/iosvkem
[juanwolf]: https://github.com/juanwolf
[jmorag]: https://github.com/jmorag
[JongW]: https://github.com/JongW
[jsoa]: https://github.com/jsoa
[jwintz]: https://github.com/jwintz
[kadenbarlow]: https://github.com/kadenbarlow
[karetsu]: https://github.com/karetsu
[kenranunderscore]: https://github.com/kenranunderscore
[legendre6891]: https://github.com/legendre6891
[LoveSponge]: https://github.com/LoveSponge
[mateossh]: https://github.com/mateossh
[Material Themes]: https://github.com/equinusocio/vsc-material-theme
[Material Dark Theme]: https://github.com/xrei/material-dark-vscode
[trev-dev]: https://github.com/trev-dev
[minikN]: https://github.com/minikN
[Miramare]: https://github.com/franbach/miramare
[Moonlight Theme]: https://github.com/atomiks/moonlight-vscode-theme
[mode-line]: https://github.com/hlissner/.emacs.d/blob/master/modules/ui/doom-modeline/config.el
[Monokai]: https://monokai.pro/
[mskorzhinskiy]: https://github.com/mskorzhinskiy
[neotree]: https://github.com/jaypei/emacs-neotree
[nlinum-hl]: https://github.com/hlissner/emacs-nlinum-hl
[neutaaaaan]: https://github.com/neutaaaaan
[nord]: https://www.nordtheme.com/
[Oceanic Next]: https://github.com/voronianski/oceanic-next-color-scheme
[outrun]: https://github.com/samrap/outrun-theme-vscode
[paladhammika]: https://github.com/paladhammika
[plain]: https://github.com/gko/plain
[sagittaros]: https://github.com/sagittaros/
[sailorhg]: https://sailorhg.github.io/fairyfloss/
[screenshots]: https://github.com/doomemacs/themes/tree/screenshots
[ShaneKilkelly]: https://github.com/ShaneKilkelly
[shades-of-purple]: https://github.com/ahmadawais/shades-of-purple-vscode
[snazzy]: https://github.com/sindresorhus/hyper-snazzy
[solarized]: http://ethanschoonover.com/solarized
[solaire-mode]: https://github.com/hlissner/emacs-solaire-mode
[sourcerer]: https://github.com/xero/sourcerer.vim
[spacefrogg]: https://github.com/spacefrogg/
[spacegrey]: http://kkga.github.io/spacegray/
[tam5]: https://github.com/tam5
[teesloane]: https://github.com/teesloane
[Tokyo Night]: https://github.com/enkia/tokyo-night-vscode-theme
[tomorrow]: https://github.com/ChrisKempson/Tomorrow-Theme
[treemacs]: https://github.com/Alexander-Miller/treemacs
[wiki]: https://github.com/doomemacs/themes/wiki
[Wilmersdorf]: https://github.com/ianpan870102/wilmersdorf-emacs-theme
[ztlevi]: https://github.com/ztlevi
[laserwave]: https://github.com/Jaredk3nt/laserwave
[hyakt]: https://github.com/hyakt
[rouge theme]: https://github.com/josefaidt/rouge-theme
[JordanFaust]: https://github.com/JordanFaust
[vscode-1337]: https://github.com/microsoft/vscode-themes/tree/main/1337
[Zenburn]: https://github.com/bbatsov/zenburn-emacs
[eziam]: https://github.com/thblt/eziam-theme-emacs
[jbeans]: https://github.com/synic/jbeans-emacs
[tao]: https://github.com/11111000000/tao-theme-emacs
[FosterHangdaan]: https://github.com/FosterHangdaan
