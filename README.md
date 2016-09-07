[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)

# DOOM for Emacs

A pack of themes made for my [emacs.d](https://github.com/hlissner/emacs.d) and
inspired by the One Dark/Light UI and syntax themes for [Atom](http://atom.io).

+ **doom-one**: inspired by Atom One Dark
+ **doom-dark**: based on Molokai

Soon to come:
+ **doom-one-light**: inspired by Atom One Light
+ **doom-tron**: doom-one, but with daylerees' [Tron Legacy][daylerees] colorscheme
+ **doom-peacock**: doom-one, but with daylerees' [Peacock][daylerees] colorscheme

**Notes:**

+ Makes use of `face-remapping-alist`. See
  `doom-enable-bright-minibuffer` and `doom-enable-bright-buffers` --
  this does not work in the terminal but will degrade gracefully.
+ Designed for Emacs 24.5+

## Screenshots

Find them [in the screenshots branch](https://github.com/hlissner/emacs-doom-theme/tree/screenshots)

## Installation

doom-theme isn't on MELPA yet, but it will be soon.

Until it is, you have to:

1. Download this repo
2. Add the repo's path to `load-path`.
3. `(require 'doom-theme)`

Then you use `load-theme` to load the theme of your choice: e.g.
`(load-theme 'doom-one-theme)`

## Configuration

+ `doom-enable-bright-minibuffer` (default: `t`): if non-nil, the minibuffer's background
  will be slightly brighter when in use (see `doom-minibuffer-active` face)
+ `doom-enable-bright-buffers` (default: `t`): if non-nil, source buffers' backgrounds
  will be slightly brighter than special buffers. This looks great for
  distinguishing sidebars and popups from source code buffers (See
  `doom-default` face)
+ `doom-enable-bold` (default: `t`): if nil, bolding will be disabled
  across all faces.
+ `doom-enable-italic` (default: `t`): if nil, italicization will be
  disabled across all faces.

## Neotree integration

To get unicode icons in [neotree]:

```emacs-lisp
(defun doom*neo-insert-root-entry (node)
  "Pretty-print pwd in neotree"
  (list (concat "  " (projectile-project-name))))

(defun doom*neo-insert-fold-symbol (name)
  "Custom hybrid unicode theme with leading whitespace."
  (or (and (eq name 'open)  (neo-buffer--insert-with-face " -  " 'neo-expand-btn-face))
      (and (eq name 'close) (neo-buffer--insert-with-face " +  " 'neo-expand-btn-face))
      (and (eq name 'leaf)  (neo-buffer--insert-with-face "   " 'neo-expand-btn-face))))

(advice-add 'neo-buffer--insert-fold-symbol :override 'doom*neo-insert-fold-symbol)
(advice-add 'neo-buffer--insert-root-entry :filter-args 'doom*neo-insert-root-entry)
```

NOTE: Doesn't work if neo-vc-integration is on.

## Mode-line

My mode-line configuration can be found at the bottom of [core-ui in my emacs.d][mode-line-cfg].


[mode-line-cfg]: https://github.com/hlissner/.emacs.d/blob/master/core/core-ui.el
[neotree]: https://github.com/jaypei/emacs-neotree
[daylerees]: http://daylerees.github.io/
