;;; doom-tokyo-night-moon-theme.el --- inspired by Folke's Tokyo Night port for Neovim -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: August 19, 2024
;; Author: Foster Hangdaan <https://code.fosterhangdaan.com/foster>
;; Maintainer:
;; Source: https://github.com/folke/tokyonight.nvim
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-tokyo-night-moon-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom doom-tokyo-night-moon-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-tokyo-night-moon-theme
  :type 'boolean)

(defcustom doom-tokyo-night-moon-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-tokyo-night-moon-theme
  :type 'boolean)

(defcustom doom-tokyo-night-moon-comment-bg doom-tokyo-night-moon-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their legibility."
  :group 'doom-tokyo-night-moon-theme
  :type 'boolean)

(defcustom doom-tokyo-night-moon-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-tokyo-night-moon-theme
  :type '(or integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-tokyo-night-moon
  "A variant of Tokyo Night based on Folke's port for Neovim."

  ;; name        default   256       16
  ((bg         '("#222436" nil       nil            ))
   (bg-alt     '("#1e2030" nil       nil            ))
   (base0      '("#444a73" "#444a73" "black"        ))
   (base1      '("#4d5482" "#4d5482" "brightblack"  ))
   (base2      '("#555d91" "#555d91" "brightblack"  ))
   (base3      '("#5e67a0" "#5e67a0" "brightblack"  ))
   (base4      '("#6c74a9" "#6c74a9" "brightblack"  ))
   (base5      '("#7b82b1" "#7b82b1" "brightblack"  ))
   (base6      '("#8990ba" "#8990ba" "brightblack"  ))
   (base7      '("#989ec3" "#989ec3" "brightblack"  ))
   (base8      '("#a7accb" "#a7accb" "brightblack"  ))
   (fg-alt     '("#828bb8" "#828bb8" "brightwhite"  ))
   (fg         '("#c8d3f5" "#c8d3f5" "white"        ))

   (grey       base4)
   (red        '("#ff757f" "#ff757f" "red"          ))
   (orange     '("#ff966c" "#ff966c" "brightred"    ))
   (green      '("#c3e88d" "#c3e88d" "green"        ))
   (teal       '("#4fd6be" "#4fd6be" "brightgreen"  ))
   (yellow     '("#ffc777" "#ffc777" "yellow"       ))
   (blue       '("#82aaff" "#82aaff" "brightblue"   ))
   (dark-blue  '("#3e68d7" "#3e68d7" "blue"         ))
   (magenta    '("#c099ff" "#c099ff" "magenta"      ))
   (violet     '("#fca7ea" "#fca7ea" "brightmagenta"))
   (cyan       '("#86e1fc" "#86e1fc" "brightcyan"   ))
   (dark-cyan  '("#86e1fc" "#86e1fc" "cyan"         ))
   ; Additional custom colors
   (dark-green '("#4fd6be" "#4fd6be" "green"        ))

   ;; face categories -- required for all themes
   (highlight      fg)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      base0)
   (builtin        red)
   (comments       (if doom-tokyo-night-moon-brighter-comments (doom-lighten "#636da6" 0.25) "#636da6"))
   (doc-comments   (doom-lighten (if doom-tokyo-night-moon-brighter-comments (doom-lighten "#636da6" 0.25) "#636da6") 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        dark-cyan)
   (operators      cyan)
   (type           base8)
   (strings        green)
   (variables      fg)
   (numbers        orange)
   (region         (doom-lighten "#2f334d" 0.15))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    "#7ca1f2")
   (vc-added       "#b8db87")
   (vc-deleted     "#e26a75")

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-tokyo-night-moon-brighter-modeline)
   (-modeline-pad
    (when doom-tokyo-night-moon-padded-modeline
      (if (integerp doom-tokyo-night-moon-padded-modeline) doom-tokyo-night-moon-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- Extra Faces ------------------------
  (
   (hl-line :background "#2f334d")

   ((line-number-current-line &override) :foreground base8)
   ((line-number &override) :foreground base0 :background (doom-darken bg 0.025))

   (font-lock-comment-face
    :foreground comments
    :background (if doom-tokyo-night-moon-comment-bg (doom-lighten bg 0.05) 'unspecified)
    :slant 'italic)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (font-lock-keyword-face :foreground keywords :slant 'italic)

   ;;; Doom Modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-path :foreground fg :weight 'normal)
   (doom-modeline-buffer-file :foreground yellow :weight 'normal)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))
   (mode-line-buffer-id
    :foreground highlight)

   ;;; Doom Dashboard
   (doom-dashboard-banner :foreground comments :slant 'normal)
   (doom-dashboard-loaded :foreground comments :slant 'normal)
   (doom-dashboard-menu-title :foreground magenta :slant 'normal)

   ;;; Indentation
   (whitespace-indentation :background bg)
   (whitespace-tab :background bg)

   ;;; Ivy
   (ivy-subdir :foreground blue)
   (ivy-minibuffer-match-face-1 :foreground green :background bg-alt)
   (ivy-minibuffer-match-face-2 :foreground orange :background bg-alt)
   (ivy-minibuffer-match-face-3 :foreground red :background bg-alt)
   (ivy-minibuffer-match-face-4 :foreground yellow :background bg-alt)

   ;;; Elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;;; Solaire
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;;; Telephone
   (telephone-line-accent-active
    :inherit 'mode-line
    :background (doom-lighten bg 0.2))
   (telephone-line-accent-inactive
    :inherit 'mode-line
    :background (doom-lighten bg 0.05))
   (telephone-line-evil-emacs
    :inherit 'mode-line
    :background dark-blue)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground fg)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground cyan)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)

   ;;; Treemacs
   (treemacs-root-face :foreground magenta :weight 'bold :height 1.2)
   (doom-themes-treemacs-root-face :foreground magenta :weight 'ultra-bold :height 1.2)
   (doom-themes-treemacs-file-face :foreground fg-alt)
   (treemacs-directory-face :foreground base8)
   (treemacs-file-face :foreground fg)
   (treemacs-git-modified-face :foreground green)
   (treemacs-git-renamed-face :foreground yellow)

   ;;; Magit
   (magit-section-heading :foreground blue)
   (magit-branch-remote   :foreground orange)
   (magit-diff-our :foreground (doom-darken "#e26a75" 0.2) :background (doom-darken "#e26a75" 0.7))
   (magit-diff-our-highlight :foreground "#e26a75" :background (doom-darken "#e26a75" 0.5) :weight 'bold)
   (magit-diff-removed :foreground (doom-darken "#e26a75" 0.2) :background (doom-darken "#e26a75" 0.7))
   (magit-diff-removed-highlight :foreground "#e26a75" :background (doom-darken "#e26a75" 0.5) :weight 'bold)
   (magit-diff-hunk-heading :foreground bg :background base1 :weight 'bold)
   (magit-diff-hunk-heading-highlight :foreground bg :background base5 :weight 'bold)

   ;; --- Major-Mode Faces -------------------
   ;;; elisp
   (highlight-quoted-symbol :foreground yellow)

   ;;; js2-mode
   (js2-function-param :foreground yellow)
   (js2-object-property :foreground dark-green)

   ;;; typescript-mode
   (typescript-this-face :foreground red)
   (typescript-access-modifier-face :foreground yellow)

   ;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-text :foreground violet)
   (rjsx-attr :foreground magenta :slant 'italic :weight 'medium)
   (rjsx-tag-bracket-face :foreground (doom-darken red 0.3))

   ;;; css-mode / scss-mode
   (css-property             :foreground blue)
   (css-selector             :foreground teal)
   (css-pseudo-class         :foreground orange)

   ;;; markdown-mode
   (markdown-markup-face :foreground violet)
   (markdown-header-face :inherit 'bold :foreground dark-cyan)
   (markdown-blockquote-face :foreground violet :background (doom-lighten bg 0.04))
   (markdown-table-face :foreground violet :background (doom-lighten bg 0.04))
   ((markdown-code-face &override) :foreground dark-cyan :background (doom-lighten bg 0.04))

   ;;; org-mode
   (org-hide :foreground hidden)
   (org-block :background (doom-darken bg 0.25))
   (org-block-begin-line :background (doom-darken bg 0.25) :foreground comments :extend t)
   (solaire-org-hide-face :foreground hidden)

   ;;; web-mode
   (web-mode-json-context-face :foreground yellow)
   (web-mode-json-key-face :foreground teal)
   (web-mode-keyword-face :inherit 'font-lock-keyword-face)
   ;;;; Block
   (web-mode-block-delimiter-face :foreground yellow)
   ;;;; Code
   (web-mode-constant-face :foreground constants)
   (web-mode-variable-name-face :foreground variables)
   ;;;; CSS
   (web-mode-css-pseudo-class-face :foreground orange)
   (web-mode-css-property-name-face :foreground blue)
   (web-mode-css-selector-face :foreground teal)
   (web-mode-css-selector-class-face :foreground keywords :slant 'nil)
   (web-mode-css-selector-tag-face :inherit 'web-mode-css-selector-class-face)
   (web-mode-css-function-face :foreground yellow)
   ;;;; HTML
   (web-mode-html-attr-engine-face :foreground yellow)
   (web-mode-html-attr-equal-face :foreground operators)
   (web-mode-html-attr-name-face :foreground magenta)
   (web-mode-html-tag-bracket-face :foreground (doom-darken red 0.3))
   (web-mode-html-tag-face :foreground red))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-tokyo-night-moon-theme.el ends here
