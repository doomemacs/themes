[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/doom-themes-badge.svg)](http://melpa.org/#/doom-themes)
[![MELPA Stable](http://stable.melpa.org/packages/doom-themes-badge.svg)](http://stable.melpa.org/#/doom-themes)

# doom-themes

DOOM Themes is an opinionated UI plugin and pack of themes extracted from my
[emacs.d], inspired by the One Dark/Light UI and syntax themes
in [Atom](http://atom.io).

[See the screenshots.][screenshots]

**Notes:**

+ Uses `face-remapping-alist`, which won't work in terminal emacs (but fails
  gracefully).
+ Tested mainly on Emacs 25.1+

## Features

+ An assortment of color schemes (feel free to request or contribute more)

Optional features:
+ Dimming of non-file buffers to visually distinguish file buffers from
  temporary or special buffers.
+ A [neotree] icon theme that follows after Atom's file drawer. This requires
  the fonts in [all-the-icons] to be installed.
+ Highlighting of the current line number (requires `nlinum` and
  `hl-line-mode`).
+ _(soon)_ A mode-line config. Until this is added, check
  out [my mode-line configuration][mode-line] in my [emacs.d].

Currently available colorschemes:
+ **doom-one**: inspired by Atom One Dark
+ **doom-vibrant**: a more vibrant take on doom-one
+ **doom-molokai**: based on molokai
+ **doom-tomorrow-night**: [Chris Kempson's Tomorrow Night][tomorrow] (dark)

Soon to come:
+ **doom-tomorrow-day**: [Chris Kempson's Tomorrow Day][tomorrow] (light)
+ **doom-one-light**: inspired by Atom One Light
+ **doom-tron**: daylerees' [Tron Legacy][daylerees] colorscheme
+ **doom-peacock**: daylerees' [Peacock][daylerees] colorscheme
+ **doom-spacegrey**: [I'm sure you've heard of it][spacegrey]
+ **doom-mono-dark**: A minimalistic, custom colorscheme
+ **doom-mono-light**: A minimalistic, custom colorscheme

## Installation

`M-x package-install RET doom-themes`

A comprehensive configuration example:

```emacs-lisp
(require 'doom-themes)

;;; Settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t  ; if nil, italics is universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil)

;; Load the theme (doom-one, doom-dark, etc.)
(load-theme 'doom-one t)

;;; OPTIONAL
;; brighter source buffers (that represent files)
(add-hook 'find-file-hook #'doom-buffer-mode-maybe)
;; ...if you use auto-revert-mode
(add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
;; And you can brighten other buffers (unconditionally) with:
(add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)

;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Enable nlinum line highlighting
(doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode
```

[Check the wiki for details on customizing doom-themes][wiki].

## Contributing

Contributions in the way of bug fixes, additional themes, plugin support
requests or code reviews are welcome and encouraged.

[Don't hesitate to report bugs and request features][issues]!


[all-the-icons]: https://github.com/domtronn/all-the-icons.el
[config]: https://github.com/hlissner/.emacs.d/tree/master/modules/ui/doom-modeline
[daylerees]: http://daylerees.github.io/
[emacs.d]: https://github.com/hlissner/.emacs.d
[issues]: https://github.com/hlissner/emacs-doom-theme/issues
[mode-line]: https://github.com/hlissner/.emacs.d/blob/master/modules/ui/doom-modeline/config.el
[neotree]: https://github.com/jaypei/emacs-neotree
[screenshots]: https://github.com/hlissner/emacs-doom-theme/tree/screenshots
[spacegrey]: http://kkga.github.io/spacegray/
[tomorrow]: https://github.com/ChrisKempson/Tomorrow-Theme
[wiki]: https://github.com/hlissner/emacs-doom-theme/wiki
