[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/doom-themes-badge.svg)](http://melpa.org/#/doom-themes)
[![MELPA Stable](http://stable.melpa.org/packages/doom-themes-badge.svg)](http://stable.melpa.org/#/doom-themes)

# doom-themes

An opinionated UI plugin/pack of themes extracted from my [emacs.d],
inspired by the One Dark/Light UI and syntax themes
in [Atom](http://atom.io).

Includes optional dimming of non-source buffers (and minibuffer), a
[neotree] theme with font icons, and (soon) a mode-line config.

Currently available colorschemes:
+ **doom-one**: inspired by Atom One Dark
+ **doom-dark**: based on Molokai

Soon to come:
+ **doom-one-light**: inspired by Atom One Light
+ **doom-tron**: doom-one, but with
  daylerees' [Tron Legacy][daylerees] colorscheme
+ **doom-peacock**: doom-one, but with daylerees' [Peacock][daylerees]
  colorscheme

**Notes:**

+ Uses `face-remapping-alist`, which won't work in terminal emacs (but
  degrades gracefully).
+ Tested mainly on Emacs 24.5+

## Screenshots

Find them [in the screenshots branch][screenshots]

## Installation

1. Install from MELPA `M-x package-install RET doom-themes`, or clone
   the repo somewhere in your `load-path`.

2. If you want the neotree theme, download the [all-the-icons] ZIP file
   and install the fonts in the `fonts/` folder.

3. `(require 'doom-themes)` and then load the theme you want.

Example configuration:

``` emacs-lisp
(require 'doom-themes)
(load-theme 'doom-one t) ;; or doom-dark, etc.

;;; OPTIONAL
;; brighter source buffers
(add-hook 'find-file-hook 'doom-buffer-mode)
;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-buffer-mode)
;; Custom neotree theme
(require 'doom-neotree)
```

## Configuration

+ `doom-enable-bold` (default: `t`): if nil, bolding will be disabled
  across all faces.
+ `doom-enable-italic` (default: `t`): if nil, italicization will be
  disabled across all faces.

## Enabling other features

### Dimmed non-source buffers/windows

`(add-hook 'find-file-hook 'doom-buffer-mode)`

Enable `doom-buffer-mode` in buffers where you want a slightly
brighter background. I use it to visually set apart source buffers
from popups, the minibuffer, or temporary buffers.

This works by remapping the `default`, `hl-line` and `linum` faces to
`doom-default`, `doom-hl-line` and `doom-linum`.

### Neotree integration

`(require 'doom-neotree)`

Modifies [neotree] to use file icons (as shown in the [screenshots]).

Note:
+ This disables `neo-vc-integration`, because the two are
  incompatible.
+ This can be customized by changing these variables:
  + `doom-neotree-folder-size` (default: `1.0`) The `:height` to
    display folder icons at.
  + `doom-neotree-chevron-size` (default: `0.8`) The `:height` to
    display chevron icons at.
  + `doom-neotree-line-spacing` (default: `2`): line-spacing to use in
    the neotree buffer.
  + `doom-neotree-enable-file-icons` (default: `nil`) If non-nil,
    display filetype icons next to each file.
  + `doom-neotree-enable-folder-icons` (default: `t`)
  + `doom-neotree-enable-chevron-icons` (default: `t`)
+ These faces can be customized:
  + `doom-neotree-folder-face`: face for folder icons
  + `doom-neotree-chevron-face`: face for chevron icons

### Mode-line config

The custom mode-line isn't part of doom-themes yet, but will be soon.

In the meantime, check out [my mode-line configuration][mode-line] in
my [emacs.d].

### Brighter minibuffer

`(add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)`

This highlights the minibuffer while its active by remapping the
`default` face to `doom-minibuffer-active`.

Note: there is no way to reliably change the minibuffer's background
permanently.


[all-the-icons]: https://github.com/domtronn/all-the-icons.el
[daylerees]: http://daylerees.github.io/
[emacs.d]: https://github.com/hlissner/.emacs.d
[mode-line]: https://github.com/hlissner/.emacs.d/blob/master/core/core-modeline.el
[neotree]: https://github.com/jaypei/emacs-neotree
[screenshots]: https://github.com/hlissner/emacs-doom-theme/tree/screenshots
[config]: https://github.com/hlissner/.emacs.d/blob/master/core/core-ui.el#L91
