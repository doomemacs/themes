[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)

# DOOM for Emacs

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
+ **doom-tron**: doom-one, but with daylerees' [Tron Legacy][daylerees] colorscheme
+ **doom-peacock**: doom-one, but with daylerees' [Peacock][daylerees] colorscheme

**Notes:**

+ Uses `face-remapping-alist`, which won't work in terminal emacs (but
  degrades gracefully).
+ Tested mainly on Emacs 24.5+

## Screenshots

Find them [in the screenshots branch][screenshots]

## Installation

1. Clone the repo somewhere in your `load-path`.
2. If you want the neotree theme, install the fonts in the `fonts/` folder in
   [all-the-icons].
3. `(require 'doom-theme)`
5. **Optional:** `(doom-init-neotree)`
6. Load the theme you want: `(load-theme 'doom-one t)`

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
