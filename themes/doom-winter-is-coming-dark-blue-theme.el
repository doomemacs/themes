;;; doom-winter-is-coming-dark-blue-theme.el --- Winter is Coming -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: Sep 18, 2022
;; Author: Philip Arvidsson <https://github.com/philiparvidsson>
;; Maintainer: Philip Arvidsson <https://github.com/philiparvidsson>
;; Source: https://github.com/johnpapa/vscode-winteriscoming
;;
;;; Commentary:
;;
;; Everyone loves this theme, or at least Philip and a few other people.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-winter-is-coming-dark-blue-theme nil
  "Options for the `doom-winter-is-coming-dark-blue' theme."
  :group 'doom-themes)

(defcustom doom-winter-is-coming-no-italics nil
  "If non-nil, no italics will be used."
  :group 'doom-winter-is-coming-dark-blue-theme
  :type 'boolean)

(defcustom doom-winter-is-coming-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-winter-is-coming-dark-blue-theme
  :type 'boolean)

(defcustom doom-winter-is-coming-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-winter-is-coming-dark-blue-theme
  :type 'boolean)

(defcustom doom-winter-is-coming-comment-bg doom-winter-is-coming-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their
legibility."
  :group 'doom-winter-is-coming-dark-blue-theme
  :type 'boolean)

(defcustom doom-winter-is-coming-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-winter-is-coming-dark-blue-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-winter-is-coming-dark-blue
                "Rip-off of Winter is Coming theme for VSCode."

                ;; name        default   256           16
                ((bg         '("#011627" "black"       "black"  ))
                 (fg         '("#a7dbf7" "#bfbfbf"     "brightwhite"  ))

                 ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
                 ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
                 ;; or region), especially when paired with the `doom-darken', `doom-lighten',
                 ;; and `doom-blend' helper functions.
                 (bg-alt     '("#062e5a" "black"       "black"        ))
                 (fg-alt     '("#7799bb" "#2d2d2d"     "white"        ))

                 ;; These should represent a spectrum from bg to fg, where base0 is a starker
                 ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
                 ;; dark grey, base0 should be white and base8 should be black.
                 (base0      '("#00101d" "black"       "black"        ))
                 (base1      '("#182735" "#1e1e1e"     "brightblack"  ))
                 (base2      '("#2e3f4e" "#2e2e2e"     "brightblack"  ))
                 (base3      '("#445969" "#262626"     "brightblack"  ))
                 (base4      '("#5c7485" "#3f3f3f"     "brightblack"  ))
                 (base5      '("#7590a3" "#525252"     "brightblack"  ))
                 (base6      '("#8fadc1" "#6b6b6b"     "brightblack"  ))
                 (base7      '("#aaccdf" "#979797"     "brightblack"  ))
                 (base8      '("#c5ebff" "#dfdfdf"     "white"        ))

                 (grey       base4)
                 (red        '("#ef5350" "#ff6655" "red"          ))
                 (orange     '("#ffca28" "#dd8844" "brightred"    ))
                 (green      '("#6bff81" "#ff0000" "green"        ))
                 (light-green '("#8dec95" "#ff0000" "green"        ))
                 (teal       '("#5abeb0" "#44b9b1" "brightgreen"  ))
                 (yellow     '("#f7ecb5" "#ECBE7B" "yellow"       ))
                 (light-blue '("#82aaff" "#51afef" "brightblue"   ))
                 (blue       '("#4373c2" "#51afef" "brightblue"   ))
                 (dark-blue  '("#0c4994" "#2257A0" "blue"         ))
                 (magenta    '("#d29ffc" "#c678dd" "brightmagenta"))
                 (violet     '("#7e57c2" "#a9a1e1" "magenta"      ))
                 (cyan       '("#00bff9" "#46D9FF" "brightcyan"   ))
                 (dark-cyan  '("#03648a" "#5699AF" "cyan"         ))

                 ;; These are the "universal syntax classes" that doom-themes establishes.
                 ;; These *must* be included in every doom themes, or your theme will throw an
                 ;; error, as they are used in the base theme defined in doom-themes-base.
                 (highlight      blue)
                 (vertical-bar   (doom-darken base1 0.1))
                 (selection      dark-blue)
                 (builtin        cyan)
                 (comments       (if doom-winter-is-coming-brighter-comments dark-cyan dark-blue))
                 (doc-comments   (doom-lighten (if doom-winter-is-coming-brighter-comments dark-cyan dark-blue) 0.25))
                 (constants      light-blue)
                 (functions      light-blue)
                 (keywords       cyan)
                 (methods        light-blue)
                 (operators      cyan)
                 (type           magenta)
                 (strings        light-green)
                 (variables      fg)
                 (numbers        yellow)
                 (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
                 (error          red)
                 (warning        orange)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; These are extra color variables used only in this theme; i.e. they aren't
                 ;; mandatory for derived themes.
                 (modeline-fg              fg)
                 (modeline-fg-alt          base5)
                 (modeline-bg              (if doom-winter-is-coming-brighter-modeline
                                               (doom-darken blue 0.45)
                                             (doom-darken bg-alt 0.1)))
                 (modeline-bg-alt          (if doom-winter-is-coming-brighter-modeline
                                               (doom-darken blue 0.475)
                                             `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
                 (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
                 (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

                 (-modeline-pad
                  (when doom-winter-is-coming-padded-modeline
                    (if (integerp doom-winter-is-coming-padded-modeline) doom-winter-is-coming-padded-modeline 4))))


  ;;;; Base theme face overrides
                (((line-number &override) :foreground dark-blue)
                 ((line-number-current-line &override) :foreground dark-blue)
                 ((font-lock-comment-face &override)
                  :background (if doom-winter-is-coming-comment-bg (doom-lighten bg 0.05) 'unspecified))
                 (mode-line
                  :background modeline-bg :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
                 (mode-line-inactive
                  :background modeline-bg-inactive :foreground modeline-fg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
                 (mode-line-emphasis :foreground (if doom-winter-is-coming-brighter-modeline base8 highlight))

                 (font-lock-comment-face
                  :foreground (if doom-winter-is-coming-brighter-comments dark-cyan dark-blue)
                  :inherit (if doom-winter-is-coming-no-italics nil 'italic))

                 (font-lock-keyword-face
                  :foreground cyan
                  :inherit (if doom-winter-is-coming-no-italics nil 'italic))

   ;;;; css-mode <built-in> / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)
   ;;;; doom-modeline
                 (doom-modeline-bar :background (if doom-winter-is-coming-brighter-modeline modeline-bg highlight))
                 (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
                 (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
                 (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
                 (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
                 (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
                 (font-latex-math-face :foreground green)
   ;;;; markdown-mode
                 (markdown-markup-face :foreground base5)
                 (markdown-header-face :inherit 'bold :foreground red)
                 ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
                 (rjsx-tag :foreground red)
                 (rjsx-attr :foreground orange)
   ;;;; solaire-mode
                 (solaire-mode-line-face
                  :inherit 'mode-line
                  :background modeline-bg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
                 (solaire-mode-line-inactive-face
                  :inherit 'mode-line-inactive
                  :background modeline-bg-inactive-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
                ())

;;; doom-winter-is-coming-dark-blue-theme.el ends here
