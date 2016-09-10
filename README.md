[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)

# DOOM for Emacs

A pack of themes made for my [emacs.d] and inspired by the One
Dark/Light UI and syntax themes for [Atom](http://atom.io).

+ **doom-one**: inspired by Atom One Dark
+ **doom-dark**: based on Molokai

Soon to come:
+ **doom-one-light**: inspired by Atom One Light
+ **doom-tron**: doom-one, but with daylerees' [Tron Legacy][daylerees] colorscheme
+ **doom-peacock**: doom-one, but with daylerees' [Peacock][daylerees] colorscheme

**Notes:**

+ Uses `face-remapping-alist`, which won't work in terminal emacs (but
  degrades gracefully).
+ Tested mainly on Emacs 24.5+

## Screenshots

Find them [in the screenshots branch][screenshots]

## Installation

doom-theme isn't on MELPA yet, but it will be soon.

Until it is:

1. Download this repo:
   `git clone https://github.com/hlissner/emacs-doom-theme ~/.emacs.d/themes/doom-theme`
2. Add the repo's path to `load-path`:
   `(add-to-list 'load-path "~/.emacs.d/themes/doom-theme")`
3. Load doom-theme:
   `(require 'doom-theme)`
4. Load the theme you want, e.g.:
   `(load-theme 'doom-one t)`

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

To make [neotree] match the screenshots, use `(doom-init-neotree)`.

Note:
+ doom-theme disables `neo-vc-integration`, because the two are
  incompatible.

## Mode-line config

You can find [my mode-line configuration][mode-line] in my [emacs.d].
I've documented it to make it easier to grok.

I will include a mode-line component for doom-theme as soon as I can
decide the best way to do so.


[all-the-icons]: https://github.com/domtronn/all-the-icons.el
[daylerees]: http://daylerees.github.io/
[emacs.d]: https://github.com/hlissner/.emacs.d
[mode-line]: https://github.com/hlissner/.emacs.d/blob/master/core/core-modeline.el
[neotree]: https://github.com/jaypei/emacs-neotree
[screenshots]: https://github.com/hlissner/emacs-doom-theme/tree/screenshots
