[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)

# DOOM for Emacs

A pack of themes made for [DOOM Emacs](https://github.com/hlissner/emacs.d) and
inspired by the One Dark/Light UI and syntax themes for [Atom](http://atom.io).

+ **doom-one**: inspired by Atom One Dark
+ **doom-one-light** (WIP): inspired by Atom One Light
+ **doom-dark**: inspired by Molokai, used for terminal emacs

NOTE: Makes use of `face-remapping-alist`. See `doom-enable-bright-minibuffer`
and `doom-enable-bright-buffers`.

## Configuration

+ `doom-enable-bright-minibuffer` (default: `t`): if non-nil, the minibuffer's background
  will be slightly brighter when in use (see `doom-minibuffer-active` face)
+ `doom-enable-bright-buffers` (default: `t`): if non-nil, source buffers' backgrounds
  will be slightly brighter than special buffers. This looks great for
  distinguishing sidebars and popups from source code buffers (See
  `doom-default` face)
+ `doom-enable-bold` (default: `t`)
+ `doom-enable-italic` (default: `t`)

## Screenshots

### doom-one (dark)

![](../screenshots/one-dark/01.png)

(more to come)

## Neotree integration

I use the following to get an Atom-esque neotree display:

``` emacs-lisp
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
