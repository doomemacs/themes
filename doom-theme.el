;;; doom-theme.el --- a pack of themes inspired by Atom One
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: May 22, 2016
;; Modified: September 7, 2016
;; Version: 1.0.5
;; Keywords: dark, blue, atom, one, seek
;; Homepage: https://github.com/hlissner/emacs-doom-theme
;; Package-Requires: ((emacs "24.4") (dash "2.12.0") (all-the-icons "1.0.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; doom-theme is a pack of themes extracted from doom-emacs, and is inspired by
;; the One Dark/Light UI and syntax themes for Atom. It provides brighter source
;; buffers and dimmed side-windows/popups.
;;
;; See the README.md for tips on customizing neotree or adding nlinum current
;; line highlights.
;;
;; It comes with two built-in colorschemes:
;;
;;   + doom-one: inspired by Atom One Dark
;;   + doom-dark: based on Molokai
;;
;; Others to come:
;;
;;   + doom-one-light: inspired by Atom One Light
;;   + doom-tron: doom-one with daylerees' Tron Legacy colorscheme
;;   + doom-peacock: doom-one with daylerees' Peacock Legacy colorscheme
;;
;; Note: doom-theme makes use of face-remapping-alist. See
;; doom-enable-bright-minibuffer and doom-enable-bright-buffer.
;;
;; ## Configuration
;;
;; + `doom-enable-bright-minibuffer` (default: `t`): if non-nil, the
;;   minibuffer's background will be slightly brighter when in use (see
;;   `doom-minibuffer-active` face)
;; + `doom-enable-bright-buffers` (default: `t`): if non-nil, source buffers'
;;   backgrounds will be slightly brighter than special buffers. This looks
;;   great for distinguishing sidebars and popups from source code buffers (See
;;   `doom-default` face)
;; + `doom-enable-bold` (default: `t`)
;; + `doom-enable-italic` (default: `t`)
;;
;;; Code:

(require 'doom)
(provide 'doom-theme)
;;; doom-theme.el ends here
