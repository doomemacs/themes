[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/doom-themes-badge.svg)](http://melpa.org/#/doom-themes)
[![MELPA Stable](http://stable.melpa.org/packages/doom-themes-badge.svg)](http://stable.melpa.org/#/doom-themes)

# doom-themes

An opinionated UI plugin/pack of themes extracted from my [emacs.d],
inspired by the One Dark/Light UI and syntax themes
in [Atom](http://atom.io).

[See the screenshots.][screenshots]

**Notes:**

+ Uses `face-remapping-alist`, which won't work in terminal emacs (but
  fails gracefully).
+ Tested mainly on Emacs 25.1+

## Features

+ An assortment of color schemes (feel free to request or contribute more)

Optional features:
+ Dimming of non-source buffers (and minibuffer) to visually
  distinguish file buffers from temporary or special buffers.
+ A [neotree] theme with configurable font icons (requires the fonts
  in [all-the-icons] to be installed).
+ Highlighting of the current line number (requires `nlinum` and
  `hl-line-mode`).
+ _(soon)_ A mode-line config.

Currently available colorschemes:
+ **doom-one**: inspired by Atom One Dark
+ **doom-molokai**: based on molokai

Soon to come:
+ **doom-one-classic**: a more vibrant version of doom-one
+ **doom-one-light**: inspired by Atom One Light
+ **doom-tron**: daylerees' [Tron Legacy][daylerees] colorscheme
+ **doom-peacock**: daylerees' [Peacock][daylerees] colorscheme
+ **doom-spacegrey**: [I'm sure you've heard of it][spacegrey]
+ **doom-mono-dark**: A minimalistic, custom colorscheme
+ **doom-mono-light**: A minimalistic, custom colorscheme

## Installation

`M-x package-install RET doom-themes`, or clone the repo somewhere in
your `load-path`.

A comprehensive configuration example:

```emacs-lisp
(require 'doom-themes)
(load-theme 'doom-one t) ;; or doom-dark, etc.

;;; Settings (defaults)
(setq doom-enable-bold t    ; if nil, bolding are universally disabled
      doom-enable-italic t  ; if nil, italics are universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil
      )

;;; OPTIONAL
;; brighter source buffers
(add-hook 'find-file-hook 'doom-buffer-mode)
;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
;; Enable custom neotree theme
(require 'doom-neotree)    ; all-the-icons fonts must be installed!
;; Enable nlinum line highlighting
(require 'doom-nlinum)     ; requires nlinum and hl-line-mode
```

## Integrations
### Org-mode

To get the most out of these themes in org-mode, you need:

``` emacs-lisp
(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)
```

### ediff and doom-buffer-mode

The temporary buffers ediff spins up aren't dimmed. You can fix this
with:

`(add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)`

### nlinum

To highlight the current line number, you need `hl-line-mode` and
`nlinum` installed. Then put this into your emacs.d:

`(require 'doom-nlinum)`

It will take effect as soon as nlinum-mode is activated.

### neotree

`(require 'doom-neotree)`

Modifies [neotree] to use icons for folders and files (as shown in the
[screenshots]).

Note:
+ This disables `neo-vc-integration`, because the two are
  incompatible.
+ `doom-neotree-enable-file-icons` (default: `t`)
+ `doom-neotree-enable-folder-icons` (default: `t`)
+ `doom-neotree-enable-chevron-icons` (default: `t`)
+ `doom-neotree-file-icons` determines what style of icons to use:
  + `t`: use the wide range of [all-the-icons] file type icons.
  + `'simple`: use a minimialistic set of file icons (most akin to
    Atom's default iconset).
  + `nil`: only use the folder icon for directories. No icons for files.
+ Customize the icons with:
  + `doom-neotree-enable-type-colors` (default: `t`): if non-nil, and
    `doom-neotree-file-icons` is `simple`, then color files/folders by
    category (hidden, media, documentation, data or build file). See
    `doom-neotree-file-face-re-alist` to configure this, and what face
    to assign each file type.
  + `doom-neotree-project-size` (default: `1.4`) The `:height` to
    display the project icons (at the top) at.
  + `doom-neotree-folder-size` (default: `1.05`) The `:height` to
    display folder icons at.
  + `doom-neotree-chevron-size` (default: `0.8`) The `:height` to
    display chevron icons at.
  + `doom-neotree-line-spacing` (default: `2`): line-spacing to use in
    the neotree buffer.
  + `doom-neotree-enable-variable-pitch` (default: `t`): if non-nil,
    file/folder labels will have the `variable-pitch` face applied to
    them.
+ These faces can be customized:
  + `doom-neotree-dir-face`: face for folders
  + `doom-neotree-file-face`: face for files
+ If `doom-neotree-enable-type-colors` is non-nil, file and folder
  entries will be colored with these faces, depending on their "type":
  + `doom-neotree-hidden-file-face` (dotfiles, *.o, *.pyc, *.elc, etc)
  + `doom-neotree-text-file-face` (READMEs, LICENSEs, org, md, etc.)
  + `doom-neotree-media-file-face` (images, video, audio, archives, etc.)
  + `doom-neotree-data-file-face` (json, xml, toml, yaml, etc.)

### mode-line config

The custom mode-line isn't part of doom-themes yet, but will be soon.

In the meantime, check out [my mode-line configuration][mode-line] in
my [emacs.d].


[all-the-icons]: https://github.com/domtronn/all-the-icons.el
[spacegrey]: http://kkga.github.io/spacegray/
[daylerees]: http://daylerees.github.io/
[emacs.d]: https://github.com/hlissner/.emacs.d
[mode-line]: https://github.com/hlissner/.emacs.d/blob/master/core/core-modeline.el
[neotree]: https://github.com/jaypei/emacs-neotree
[screenshots]: https://github.com/hlissner/emacs-doom-theme/tree/screenshots
[config]: https://github.com/hlissner/.emacs.d/blob/master/core/core-ui.el#L91
