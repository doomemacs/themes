;;; doom-tokyo-night-light-theme.el --- inspired by VSCode's Tokyo Night theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: August 17, 2024
;; Author: Foster Hangdaan <https://code.fosterhangdaan.com/foster>
;; Maintainer:
;; Source: https://github.com/enkia/tokyo-night-vscode-theme
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-tokyo-night-light-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom doom-tokyo-night-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-tokyo-night-light-theme
  :type 'boolean)

(defcustom doom-tokyo-night-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-tokyo-night-light-theme
  :type 'boolean)

(defcustom doom-tokyo-night-light-comment-bg doom-tokyo-night-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their legibility."
  :group 'doom-tokyo-night-light-theme
  :type 'boolean)

(defcustom doom-tokyo-night-light-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-tokyo-night-light-theme
  :type '(or integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-tokyo-night-light
  "A light variant of Tokyo Night."

  ; Color Scheme
  ; gui       256
  ; "#8c4351" "#8c4351" => This keyword, HTML elements, Regex group symbol, CSS units, Terminal Red
  ; "#965027" "#965027" => Number and Boolean constants, Language support constants
  ; "#8f5e15" "#8f5e15" => Function parameters, Regex character sets, Terminal Yellow
  ; "#634f30" "#634f30" => Parameters inside functions (semantic highlighting only)
  ; "#385f0d" "#385f0d" => Strings, CSS class names
  ; "#33635c" "#33635c" => Object literal keys, Markdown links, Regex literal strings, Terminal Green
  ; "#006c86" "#006c86" => Language support functions, CSS HTML elements
  ; "#0f4b6e" "#0f4b6e" => Object properties, Regex quantifiers and flags, Markdown headings, Terminal Cyan, Markdown code, Import/export keywords
  ; "#2959aa" "#2959aa" => Function names, CSS property names, Markdown Headings, Terminal Blue
  ; "#5a3e8e" "#5a3e8e" => Control Keywords, Storage Types, Regex symbols and operators, HTML Attributes, Terminal Magenta
  ; "#707280" "#707280" => Terminal White
  ; "#40434f" "#40434f" => Markdown Text, HTML Text
  ; "#343b58" "#343b58" => Editor foreground, Variables, Class names, Terminal black
  ; "#6c6e75" "#6c6e75" => Comments
  ; "#e6e7ed" "#e6e7ed" => Editor background

  ;; name        default   256       16
  ((bg         '("#e6e7ed" nil       nil            ))
   (bg-alt     '("#d4d6e0" nil       nil            ))
   (base0      '("#707280" "#707280" "white"        ))
   (base1      '("#797b8a" "#797b8a" "brightwhite"  ))
   (base2      '("#838593" "#838593" "brightwhite"  ))
   (base3      '("#8d8f9c" "#8d8f9c" "brightwhite"  ))
   (base4      '("#989aa5" "#989aa5" "brightwhite"  ))
   (base5      '("#a2a4ae" "#a2a4ae" "brightwhite"  ))
   (base6      '("#acaeb7" "#acaeb7" "brightwhite"  ))
   (base7      '("#b7b8c0" "#b7b8c0" "brightwhite"  ))
   (base8      '("#c1c2c9" "#c1c2c9" "brightwhite"  ))
   (fg-alt     '("#475178" "#475178" "brightblack"  ))
   (fg         '("#343b58" "#343b58" "black"        ))

   (grey       base4)
   (red        '("#8c4351" "#8c4351" "red"          ))
   (orange     '("#965027" "#965027" "brightred"    ))
   (green      '("#33635c" "#33635c" "green"        ))
   (teal       '("#006c86" "#006c86" "brightgreen"  ))
   (yellow     '("#8f5e15" "#8f5e15" "yellow"       ))
   (blue       '("#2959aa" "#2959aa" "brightblue"   ))
   (dark-blue  '("#2959aa" "#2959aa" "blue"         ))
   (magenta    '("#5a3e8e" "#5a3e8e" "magenta"      ))
   (violet     '("#40434f" "#40434f" "brightmagenta"))
   (cyan       '("#0f4b6e" "#0f4b6e" "brightcyan"   ))
   (dark-cyan  '("#0f4b6e" "#0f4b6e" "cyan"         ))
   ; Additional custom colors
   (dark-green '("#385f0d" "#385f0d" "green"        ))
   (brown      '("#634f30" "#634f30" "yellow"       ))

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      base5)
   (builtin        red)
   (comments       (if doom-tokyo-night-light-brighter-comments (doom-lighten "#6c6e75" 0.25) "#6c6e75"))
   (doc-comments   (doom-lighten (if doom-tokyo-night-light-brighter-comments (doom-lighten "#6c6e75" 0.25) "#6c6e75") 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        blue)
   (operators      teal)
   (type           base0)
   (strings        dark-green)
   (variables      fg)
   (numbers        orange)
   (region         (doom-lighten base8 0.15))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-tokyo-night-light-brighter-modeline)
   (-modeline-pad
    (when doom-tokyo-night-light-padded-modeline
      (if (integerp doom-tokyo-night-light-padded-modeline) doom-tokyo-night-light-padded-modeline 4)))

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
   ((line-number-current-line &override) :foreground fg)
   ((line-number &override) :foreground comments :background (doom-darken bg 0.025))

   (font-lock-comment-face
    :foreground comments
    :background (if doom-tokyo-night-light-comment-bg (doom-lighten bg 0.05) 'unspecified)
    :slant 'italic)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (font-lock-keyword-face :foreground keywords :slant 'italic)

   ;;; Doom Modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-path :foreground fg :weight 'normal)
   (doom-modeline-buffer-file :foreground brown :weight 'normal)

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
   (magit-diff-our :foreground (doom-darken red 0.2) :background (doom-darken red 0.7))
   (magit-diff-our-highlight :foreground red :background (doom-darken red 0.5) :weight 'bold)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-darken red 0.7))
   (magit-diff-removed-highlight :foreground red :background (doom-darken red 0.5) :weight 'bold)

   ;; --- Major-Mode Faces -------------------
   ;;; elisp
   (highlight-quoted-symbol :foreground yellow)

   ;;; js2-mode
   (js2-function-param :foreground yellow)
   (js2-object-property :foreground green)

   ;;; typescript-mode
   (typescript-this-face :foreground red)
   (typescript-access-modifier-face :foreground brown)

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
   (org-block :background (doom-darken bg 0.05))
   (org-block-begin-line :background (doom-darken bg 0.05) :foreground comments :extend t)
   (solaire-org-hide-face :foreground hidden)

   ;;; web-mode
   (web-mode-json-context-face :foreground brown)
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

;;; doom-tokyo-night-light-theme.el ends here
