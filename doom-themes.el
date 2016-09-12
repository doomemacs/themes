;;; doom-themes.el --- a pack of themes inspired by Atom One
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: May 22, 2016
;; Modified: September 12, 2016
;; Version: 1.0.7
;; Keywords: dark blue atom one seek
;; Homepage: https://github.com/hlissner/emacs-doom-theme
;; Package-Requires: ((emacs "24.4") (dash "2.12.0") (all-the-icons "1.0.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; An opinionated UI plugin/pack of themes extracted from my emacs.d, inspired
;; by the One Dark/Light UI and syntax themes in Atom.
;;
;; Includes optional dimming of non-source buffers, a neotree theme with font
;; icons, and (soon) a mode-line config.
;;
;; Currently available colorschemes:
;; + doom-one: inspired by Atom One Dark
;; + doom-dark: based on Molokai
;;
;; Soon to come:
;; + doom-one-light**: inspired by Atom One Light
;; + doom-tron**: doom-one, but with daylerees' Tron Legacy colorscheme
;; + doom-peacock**: doom-one, but with daylerees' Peacock colorscheme
;;
;; Notes:
;; + Uses `face-remapping-alist`, which won't work in terminal emacs (but
;;   degrades gracefully).
;; + Tested mainly on Emacs 24.5+
;;
;;
;; ## Configuration
;;
;; + `doom-enable-bold` (default: `t`)
;; + `doom-enable-italic` (default: `t`)
;;
;;
;; ## Installation
;;
;; Clone the repo somewhere in your `load-path'.
;;
;; If you want the neotree theme, install the fonts in the fonts/ folder of
;; all-the-icons.
;;
;;   (require 'doom-themes)
;;   (load-theme 'doom-one t) ;; or doom-dark, etc.
;;
;;   ;; OPTIONAL (more info below)
;;   (add-hook 'find-file-hook 'doom-buffer-mode) ;; brighter source buffers
;;   (require 'doom-neotree)                      ;; neotree theme
;;
;;; Code:

(require 'doom)
(provide 'doom-themes)
;;; doom-themes.el ends here
