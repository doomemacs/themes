;;; doom-sonokai-theme.el --- Port of Sonokai's default style -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Mihir-Null <https://github.com/Mihir-Null>
;; Source: https://github.com/sainnhe/sonokai
;;
;;; Commentary:
;;
;; Sonokai's default palette and highlight mappings, adapted for Emacs.
;; Source revision: b023c5280b16fe2366f5e779d8d2756b3e5ee9c3
;; Other Sonokai styles are not included.
;;
;; MIT License
;;
;; Copyright (c) 2020 sainnhe
;; Copyright (c) 2026 Mihir-Null
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Code:

(require 'doom-themes)

(def-doom-theme doom-sonokai
  "A port of Sonokai's default dark style."
  :family 'doom-sonokai
  :background-mode 'dark

  ;; The 256-color entries match Sonokai's cterm palette.
  ;; name        GUI       256       16
  ((bg         '("#2c2e34" "#262626" "black"))
   (bg-alt     '("#222327" "#080808" "black"))
   (fg         '("#e2e2e3" "#bcbcbc" "white"))
   (fg-alt     '("#7f8490" "#949494" "brightblack"))

   (base0      '("#181819" "#080808" "black"))
   (base1      bg-alt)
   (base2      '("#33353f" "#303030" "brightblack"))
   (base3      '("#363944" "#303030" "brightblack"))
   (base4      '("#414550" "#3a3a3a" "brightblack"))
   (base5      '("#595f6f" "#585858" "brightblack"))
   (base6      fg-alt)
   (base7      fg)
   (base8      fg)

   (grey       fg-alt)
   (red        '("#fc5d7c" "#ff5f5f" "red"))
   (orange     '("#f39660" "#ffaf5f" "brightred"))
   (green      '("#9ed072" "#87af5f" "green"))
   (teal       green)
   (yellow     '("#e7c664" "#d7af5f" "yellow"))
   (blue       '("#76cce0" "#87afd7" "blue"))
   (dark-blue  '("#354157" "#00005f" "black"))
   (magenta    '("#b39df3" "#d787d7" "magenta"))
   (violet     magenta)
   (cyan       blue)
   (dark-cyan  blue)

   ;; Universal syntax categories.  Variables and properties follow Sonokai's
   ;; Tree-sitter mappings; constants use its traditional Constant group.
   (highlight      blue)
   (vertical-bar   base0)
   (selection      base4)
   (builtin        green)
   (comments       grey)
   (doc-comments   grey)
   (constants      orange)
   (functions      green)
   (keywords       red)
   (methods        green)
   (operators      red)
   (type           blue)
   (strings        yellow)
   (variables      fg)
   (numbers        violet)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; Sonokai's UI surfaces and filled search/completion accents.
   (modeline-bg    '("#3b3e48" "#3a3a3a" "brightblack"))
   (diff-added-bg  '("#394634" "#005f00" "black"))
   (diff-removed-bg '("#55393d" "#5f0000" "black"))
   (filled-red    '("#ff6077" "#ff5f5f" "red"))
   (filled-green  '("#a7df78" "#87af5f" "green"))
   (filled-blue   '("#85d3f2" "#87afd7" "blue"))
   (-modeline-pad
    (when doom-themes-padded-modeline
      (if (integerp doom-themes-padded-modeline)
          doom-themes-padded-modeline
        4))))

  ;; Keep Doom's package coverage; override the distinctive Sonokai faces.
  (((font-lock-comment-face &override) :slant (if italic 'italic 'normal))
   ((font-lock-function-call-face &override) :foreground functions :slant 'normal)
   ((font-lock-variable-use-face &override) :foreground variables)
   ((font-lock-property-name-face &override) :foreground orange :weight 'normal)
   ((font-lock-punctuation-face &override) :foreground grey)
   ((font-lock-regexp-grouping-backslash &override) :foreground green)
   ((font-lock-regexp-grouping-construct &override) :foreground green)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground fg)
   (hl-line :background base2 :extend t)
   (show-paren-match :background base4 :foreground fg)
   (isearch :background filled-red :foreground bg :weight 'bold)
   (lazy-highlight :background filled-green :foreground bg)
   (match :background filled-green :foreground bg)
   (tooltip :background base3 :foreground fg)

   (mode-line :background modeline-bg :foreground fg
              :box (when -modeline-pad
                     `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive :background base2 :foreground grey
                       :box (when -modeline-pad
                              `(:line-width ,-modeline-pad :color ,base2)))
   (tab-bar :background base2 :foreground grey)
   (tab-bar-tab :background filled-red :foreground bg :weight 'bold)
   (tab-bar-tab-inactive :background base4 :foreground fg)

   ;;;; completion
   (corfu-default :background base3 :foreground fg)
   (corfu-current :background filled-blue :foreground bg)
   (company-tooltip :background base3 :foreground fg)
   (company-tooltip-selection :background filled-blue :foreground bg)
   (vertico-current :background base3 :extend t)

   ;;;; diff <built-in>
   ((diff-added &override) :background diff-added-bg :foreground green)
   ((diff-removed &override) :background diff-removed-bg :foreground red)
   ((diff-changed &override) :background dark-blue :foreground blue)

   ;;;; doom-modeline
   (doom-modeline-bar :background filled-red)
   (doom-modeline-buffer-path :foreground blue :weight 'bold)

   ;;;; terminal colors (Sonokai deliberately uses orange for ANSI cyan)
   (ansi-color-black :foreground base0 :background base0)
   (ansi-color-bright-black :foreground grey :background grey)
   (ansi-color-cyan :foreground orange :background orange)
   (ansi-color-bright-red :foreground red :background red)
   (ansi-color-bright-green :foreground green :background green)
   (ansi-color-bright-yellow :foreground yellow :background yellow)
   (ansi-color-bright-blue :foreground blue :background blue)
   (ansi-color-bright-magenta :foreground magenta :background magenta)
   (ansi-color-bright-cyan :foreground orange :background orange)
   (ansi-color-bright-white :foreground fg :background fg)
   (term-color-black :foreground base0 :background base0)
   (term-color-cyan :foreground orange :background orange)
   ;; vterm uses the background attributes for its bright ANSI colors.
   (vterm-color-black :foreground base0 :background grey)
   (vterm-color-red :foreground red :background red)
   (vterm-color-green :foreground green :background green)
   (vterm-color-yellow :foreground yellow :background yellow)
   (vterm-color-blue :foreground blue :background blue)
   (vterm-color-magenta :foreground magenta :background magenta)
   (vterm-color-cyan :foreground orange :background orange)
   (vterm-color-white :foreground fg :background fg))

  ;; Older ansi-color consumers and rustic use vectors instead of ANSI faces.
  ((ansi-color-names-vector
    (vconcat (mapcar #'doom-color '(base0 red green yellow blue magenta orange fg))))
   (rustic-ansi-faces
    (vconcat (mapcar #'doom-color '(base0 red green yellow blue magenta orange fg))))))

(provide 'doom-sonokai-theme)
;;; doom-sonokai-theme.el ends here
