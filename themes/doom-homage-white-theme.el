;;; doom-homage-white-theme.el --- minimal white theme inspired by editors from 2000s -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: February 4, 2021 (#497)
;; Author: mskorzhinskiy <https://github.com/mskorzhinskiy>
;; Maintainer:
;; Source: original
;;
;;; Commentary:
;;
;; Theme is using palette inspired by various editors from 2000s, with a lot of
;; inspiration from eziam theme (thblt/eziam-theme-emacs) and tao themes
;; (11111000000/tao-theme-emacs).
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-homage-white-theme nil
  "Options for the `doom-homage-white' theme."
  :group 'doom-themes)

(defcustom doom-homage-white-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-homage-white-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-homage-white
  "A light theme inspired by editors from 2000s"
  :family 'doom-homage
  :background-mode 'light

  ;; name        default   256       16
  ((bg         '("#fafafa" nil       nil            ))
   (bg-alt     '("#f0f0f0" nil       nil            ))
   (base0      '("#f0f0f0" "#f0f0f0" "white"        ))
   (base1      '("#e7e7e7" "#e7e7e7" "brightblack"  ))
   (base2      '("#dfdfdf" "#dfdfdf" "brightblack"  ))
   (base3      '("#c6c7c7" "#c6c7c7" "brightblack"  ))
   (base4      '("#9ca0a4" "#9ca0a4" "brightblack"  ))
   (base5      '("#383a42" "#424242" "brightblack"  ))
   (base6      '("#202328" "#2e2e2e" "brightblack"  ))
   (base7      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base8      '("#1b2229" "black"   "black"        ))
   (fg         '("#383a42" "#424242" "black"        ))
   (fg-alt     '("#c6c7c7" "#c7c7c7" "brightblack"  ))

   (grey       base5)
   (red        '("#e45649" "#e45649" "red"          ))
   (orange     '("#8a3b3c" "#dd8844" "brightred"    ))
   (green      '("#556b2f" "#556b2f" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#986801" "#986801" "yellow"       ))
   (yellow-alt '("#fafadd" "#fafadd" "yellow"       ))
   (blue       '("#014980" "#014980" "brightblue"   ))
   (dark-blue  '("#030f64" "#030f64" "blue"         ))
   (magenta    '("#a626a4" "#a626a4" "magenta"      ))
   (violet     '("#b751b6" "#b751b6" "brightmagenta"))
   (cyan       '("#0184bc" "#0184bc" "brightcyan"   ))
   (dark-cyan  '("#005478" "#005478" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      base3)
   (builtin        fg)
   (comments       green)
   (doc-comments   (doom-darken comments 0.15))
   (constants      fg)
   (functions      blue)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        orange)
   (variables      fg)
   (numbers        orange)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright t)
   (-modeline-pad
    (when doom-homage-white-padded-modeline
      (if (integerp doom-homage-white-padded-modeline) doom-homage-white-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken base2 0.05)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken base2 0.1)
      base2))
   (modeline-bg-inactive (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))

  ;;;; Base theme face overrides
  ((font-lock-builtin-face       :inherit 'bold)
   ((font-lock-doc-face &override) :slant 'italic)
   (font-lock-function-name-face :inherit 'bold)
   (font-lock-keyword-face       :inherit 'bold)
   (font-lock-type-face          :inherit 'bold)
   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground base8)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (tooltip :background base1 :foreground fg)

   ;; Override secondary selection
   ((secondary-selection &override) :background base0)

   ;;;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))
   ;;;; magit
   ((magit-diff-hunk-heading           &override) :foreground base4 :background bg :bold bold)
   ((magit-diff-hunk-heading-highlight &override) :foreground fg    :background bg :bold bold)
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)
   ;;;; helm
   (helm-candidate-number :background blue :foreground bg)
   ;;;; highlight-indent-guides
   (highlight-indent-guides-character-face :foreground base2)
   ;;;; ivy
   ((ivy-minibuffer-match-face-1 &override) :foreground (doom-darken grey 0.70))
   ;;;; ivy-posframe
   (ivy-posframe               :background base0)
   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)
   ;;;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)
   ;;;; mu4e
   (mu4e-highlight-face         :background bg :inherit 'bold)
   (mu4e-header-highlight-face :foreground dark-blue :inherit 'bold)
   (mu4e-unread-face :foreground blue)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground fg)
   ((outline-2 &override) :foreground fg)
   ((outline-3 &override) :foreground fg)
   ((outline-4 &override) :foreground fg)
   ((outline-5 &override) :foreground fg)
   ((outline-6 &override) :foreground fg)
   ((outline-7 &override) :foreground fg)
   ((outline-8 &override) :foreground fg)
   ;;;; org <built-in>
   ;; Make unfinished cookie & todo keywords bright to grab attention
   ((org-todo &override) :foreground red)
   ;; Make tags and dates to have pretty box around them
   ((org-tag &override)   :foreground fg :background yellow-alt
    :box `(:line-width -1 :color ,base5 :style released-button))
   ((org-date &override)  :foreground fg :background base1
    :box `(:line-width -1 :color ,base5 :style released-button))
   ;; Make drawers and special keywords (like scheduled) to be very bleak
   ((org-special-keyword &override)  :foreground grey)
   ((org-drawer          &override)  :foreground grey)
   ;; Make ellipsis as bleak as possible and reset underline/boxing
   (org-ellipsis :underline nil :box nil :foreground fg :background bg)
   ;; Make blocks have a slightly different background
   ((org-block &override) :background base1)
   ((org-block-begin-line &override) :foreground fg :slant 'italic)
   ((org-quote &override) :background base1)
   ((org-table &override) :foreground fg)
   ;; Make "unimportant" things like distant deadlines and things scheduled for
   ;; today to be bleak.
   (org-upcoming-deadline         :foreground base8)
   (org-upcoming-distant-deadline :foreground fg)
   (org-scheduled                 :foreground fg)
   (org-scheduled-today           :foreground fg)
   (org-scheduled-previously      :foreground base8)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; swiper
   ((swiper-match-face-1 &override) :foreground bg :background fg)
   ((swiper-line-face    &override) :background (doom-lighten blue 0.70) :foreground fg)
   ;;;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1))

  ;;;; Base theme variable overrides-
  ())

;;; doom-homage-white-theme.el ends here
