![Release tag](https://img.shields.io/github/tag/hlissner/emacs-doom-themes.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/doom-themes-badge.svg?style=flat-square)](http://melpa.org/#/doom-themes)
![Build status](https://img.shields.io/github/workflow/status/hlissner/emacs-doom-themes/CI/master?style=flat-square)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

# doom-themes

A theme megapack for GNU Emacs, inspired by community favorites. Special
attention is given for [Doom Emacs](https://doomemacs.org) and [solaire-mode]
support, but will work fine anywhere else. 

[See the screenshots.][screenshots]

**Table of Contents**

- [Install](#install)
    - [Doom Emacs](#doom-emacs)
    - [Manually](#manually)
- [Theme list](#theme-list)
    - [Flagship themes](#flagship-themes)
    - [Additional themes](#additional-themes)
    - [Planned themes](#planned-themes)
- [Extensions](#extensions)
- [Complimentary plugins](#complimentary-plugins)
- [FAQ](#faq)
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

### Flagship themes
These themes were written by the author of this package and are most maintained:

| Name             | Description                            |
|------------------|----------------------------------------|
| `doom-one`       | Flagship theme based on atom One Dark  |
| `doom-one-light` | Flagship theme based on atom One Light |
| `doom-vibrant`   | A more vibrant version of `doom-one`   |

### Additional themes
These themes were submitted to us by the community. We welcome PRs to help us
maintain them and address inconsistencies:

| Name                                | Description                                                                                                  |
|-------------------------------------|--------------------------------------------------------------------------------------------------------------|
| `doom-1337`                         | ported from [VSCode's 1337 Theme][vscode-1337]                                                               |
| `doom-acario-dark`                  | an original dark theme (thanks to [gagbo])                                                                   |
| `doom-acario-light`                 | an original light theme (thanks to [gagbo])                                                                  |
| `doom-ayu-mirage`                   | Dark variant from [Ayu] themes (thanks to [LoveSponge])                                                      |
| `doom-ayu-light`                    | Light variant from [Ayu] themes(thanks to [LoveSponge])                                                      |
| `doom-badger`                       | Based on [original Badger theme](https://github.com/ccann/badger-theme)                                      |
| `doom-challenger-deep`              | based on Vim's [Challenger deep][challenger-deep] theme (thanks to [fuxialexander])                          |
| `doom-city-lights`                  | based on Atom's [City lights][city-lights] (thanks to [fuxialexander])                                       |
| `doom-dark+`                        | ported from VS Code's [Dark+][dark+] theme (thanks to [ema2159])                                             |
| `doom-dracula`                      | an implementation of [Dracula] theme (thanks to [fuxialexander])                                             |
| `doom-ephemeral`                    | inspired in the Ephemeral Theme from [elenapan's dotfiles] (thanks to [karetsu])                             |
| `doom-fairy-floss`                  | a candy colored Sublime theme by [sailorhg] (thanks to [ema2159])                                            |
| `doom-flatwhite`                    | a unique light theme ported from [Flatwhite Syntax][flatwhite] (thanks to [ShaneKilkelly])                   |
| `doom-gruvbox-light`                | adapted from Morhetz's [Gruvbox] light variant (thanks for [jsoa])                                           |
| `doom-gruvbox`                      | adapted from Morhetz's [Gruvbox] (thanks to [JongW])                                                         |
| `doom-henna`                        | based on VS Code's [Henna] (thanks to [jsoa])                                                                |
| `doom-homage-black`                 | dark variant of doom-homage white. (thanks to [mskorzhinskiy])                                               |
| `doom-homage-white`                 | a minimalistic, colorless theme, inspired by [eziam], [tao] and [jbeans] themes. (thanks to [mskorzhinskiy]) |
| `doom-horizon`                      | ported from VS Code's [Horizon] (thanks to [karetsu])                                                        |
| `doom-Iosvkem`                      | adapted from [Iosvkem] (thanks to [neutaaaaan])                                                              |
| `doom-laserwave`                    | a clean 80's synthwave / outrun theme inspired by VS Code's [laserwave][laserwave] (thanks to [hyakt])       |
| `doom-manegarm`                     | an original autumn-inspired dark theme (thanks to [kenranunderscore])                                        |
| `doom-material`                     | adapted from [Material Themes] (thanks to [tam5])                                                            |
| `doom-miramare`                     | a port of [Franbach's][franbach] [Miramare], a variant of gruvbox theme (thanks to [sagittaros])             |
| `doom-molokai`                      | a theme based on Texmate's Monokai                                                                           |
| `doom-monokai-classic`              | port of [Monokai]'s Classic variant (thanks to [ema2159])                                                    |
| `doom-monokai-pro`                  | port of [Monokai]'s Pro variant (thanks to [kadenbarlow])                                                    |
| `doom-monokai-machine`              | port of [Monokai]'s Pro (Machine) variant (thanks to [minikN])                                               |
| `doom-monokai-octagon`              | port of [Monokai]'s Pro (Octagon) variant (thanks to [minikN])                                               |
| `doom-monokai-ristretto`            | port of [Monokai]'s Pro (Ristretto) variant (thanks to [minikN])                                             |
| `doom-monokai-spectrum`             | port of [Monokai]'s Pro (Spectrum) variant (thanks to [minikN])                                              |
| `doom-moonlight`                    | ported from VS Code's [Moonlight Theme] (thanks to [Brettm12345])                                            |
| `doom-nord-light`                   | light variant of [Nord][nord] (thanks to [fuxialexander])                                                    |
| `doom-nord`                         | dark variant of [Nord][nord] (thanks to [fuxialexander])                                                     |
| `doom-nova`                         | adapted from [Nova] (thanks to [bigardone])                                                                  |
| `doom-oceanic-next`                 | adapted from [Oceanic Next] theme (thanks to [juanwolf])                                                     |
| `doom-old-hope`                     | based on [An Old Hope] theme (thanks to [teesloane])                                                         |
| `doom-opera-light`                  | an original light theme (thanks to [jwintz])                                                                 |
| `doom-opera`                        | an original dark theme (thanks to [jwintz])                                                                  |
| `doom-outrun-electric`              | a neon colored theme inspired in VS Code's [Outrun Electric][outrun] (thanks to [ema2159])                   |
| `doom-palenight`                    | adapted from [Material Themes] (thanks to [Brettm12345])                                                     |
| `doom-peacock`                      | based on Peacock from [daylerees' themes][daylerees] (thanks to [teesloane])                                 |
| `doom-plain-dark`                   | based on [plain] (thanks to [das-s])                                                                         |
| `doom-plain`                        | based on [plain] (thanks to [mateossh])                                                                      |
| `doom-rouge`                        | ported from [VSCode's Rouge Theme][rouge theme]  (thanks to [JordanFaust])                                   |
| `doom-shades-of-purple`             | a purple and vibrant theme inspired by VSCode's [Shades of Purple][shades-of-purple] (thanks to [jwbaldwin]) |
| `doom-snazzy`                       | a dark theme inspired in Atom's [Hyper Snazzy][snazzy] (thanks to [ar1a])                                    |
| `doom-solarized-dark`               | dark variant of [Solarized][solarized] (thanks to [ema2159])                                                 |
| `doom-solarized-dark-high-contrast` | high contrast dark variant of [Solarized][solarized] (thanks to [jmorag])                                    |
| `doom-solarized-light`              | light variant of [Solarized][solarized] (thanks to [fuxialexander])                                          |
| `doom-sourcerer`                    | based on [Sourcerer][sourcerer] (thanks to [defphil])                                                        |
| `doom-spacegrey`                    | [I'm sure you've heard of it][spacegrey] (thanks to [teesloane])                                             |
| `doom-tomorrow-day`                 | [Tomorrow][tomorrow]'s light variant (thanks to [emacswatcher])                                              |
| `doom-tomorrow-night`               | one of the dark variants of [Tomorrow][tomorrow] (thanks to [emacswatcher])                                  |
| `doom-wilmersdorf`                  | port of Ian Pan's [Wilmersdorf] (thanks to [ema2159])                                                        |
| `doom-xcode`                        | Based off of Apple's Xcode Dark theme (thanks to [kadenbarlow])                                              |
| `doom-zenburn`                      | port of the popular [Zenburn] theme (thanks to [jsoa])                                                       |

### Planned themes

| Name                                 | Description                                              |
|--------------------------------------|----------------------------------------------------------|
| `doom-mono-dark` / `doom-mono-light` | a minimalistic, monochromatic theme                      |
| `doom-tron`                          | based on Tron Legacy from [daylerees' themes][daylerees] |


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
     
## Complimentary plugins
The following plugins compliment our themes:

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
[issues]: https://github.com/hlissner/emacs-doom-themes/issues
[Iosvkem]: https://github.com/neutaaaaan/iosvkem
[juanwolf]: https://github.com/juanwolf
[jmorag]: https://github.com/jmorag
[JongW]: https://github.com/JongW
[jsoa]: https://github.com/jsoa
[jwintz]: https://github.com/jwintz
[kadenbarlow]: https://github.com/kadenbarlow
[karetsu]: https://github.com/karetsu
[kenranunderscore]: https://github.com/kenranunderscore
[LoveSponge]: https://github.com/LoveSponge
[mateossh]: https://github.com/mateossh
[Material Themes]: https://github.com/equinusocio/vsc-material-theme
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
[plain]: https://github.com/gko/plain
[sagittaros]: https://github.com/sagittaros/
[sailorhg]: https://sailorhg.github.io/fairyfloss/
[screenshots]: https://github.com/hlissner/emacs-doom-themes/tree/screenshots
[ShaneKilkelly]: https://github.com/ShaneKilkelly
[shades-of-purple]: https://github.com/ahmadawais/shades-of-purple-vscode
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
[vscode-1337]: https://github.com/microsoft/vscode-themes/tree/main/1337
[Zenburn]: https://github.com/bbatsov/zenburn-emacs
[eziam]: https://github.com/thblt/eziam-theme-emacs
[jbeans]: https://github.com/synic/jbeans-emacs
[tao]: https://github.com/11111000000/tao-theme-emacs
