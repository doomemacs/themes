;;; doom-tokyo-night-theme.el - based on https://github.com/enkia/tokyo-night-vscode-theme -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-tokyo-night-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-tokyo-night-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-tokyo-night-theme
  :type 'boolean)

(defcustom doom-tokyo-night-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-tokyo-night-theme
  :type 'boolean)

(defcustom doom-tokyo-night-comment-bg doom-tokyo-night-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their legibility."
  :group 'doom-tokyo-night-theme
  :type 'boolean)

(defcustom doom-tokyo-night-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-tokyo-night-theme
  :type '(or integer boolean))

(def-doom-theme doom-tokyo-night
  "A clean, dark theme that celebrates the lights of downtown Tokyo at night."

  ; Color Scheme
  ; gui       256
  ; "#f7768e" "#f7768e" => This keyword, HTML elements, Regex group symbol, CSS units, Terminal Red
  ; "#ff9e64" "#ff9e64" => Number and Boolean constants, Language support constants
  ; "#e0af68" "#e0af68" => Function parameters, Regex character sets, Terminal Yellow
  ; "#9ece6a" "#9ece6a" => Strings, CSS class names
  ; "#73daca" "#73daca" => Object literal keys, Markdown links, Terminal Green
  ; "#b4f9f8" "#b4f9f8" => Regex literal strings
  ; "#2ac3de" "#2ac3de" => Language support functions, CSS HTML elements
  ; "#7dcfff" "#7dcfff" => Object properties, Regex quantifiers and flags, Markdown headings, Terminal Cyan, Markdown code, Import/export keywords
  ; "#7aa2f7" "#7aa2f7" => Function names, CSS property names, Terminal Blue
  ; "#bb9af7" "#bb9af7" => Control Keywords, Storage Types, Regex symbols and operators, HTML Attributes, Terminal Magenta
  ; "#c0caf5" "#c0caf5" => Variables, Class names, Terminal White
  ; "#a9b1d6" "#a9b1d6" => Editor foreground
  ; "#9aa5ce" "#9aa5ce" => Markdown Text, HTML Text
  ; "#cfc9c2" "#cfc9c2" => Parameters inside functions (semantic highlighting only)
  ; "#565f89" "#565f89" => Comments
  ; "#414868" "#414868" => Terminal black
  ; "#24283b" "#24283b" => Editor background (Storm)
  ; "#1a1b26" "#1a1b26" => Editor background (Night)

  ;; name        default   256       16
  ((bg         '("#1a1b26" nil       nil            ))
   (bg-alt     '("#13141c" nil       nil            ))
   (base0      '("#414868" "#414868" "black"        ))
   (base1      '("#51587a" "#51587a" "brightblack"  ))
   (base2      '("#61698b" "#61698b" "brightblack"  ))
   (base3      '("#71799d" "#71799d" "brightblack"  ))
   (base4      '("#8189af" "#8189af" "brightblack"  ))
   (base5      '("#9099c0" "#9099c0" "brightblack"  ))
   (base6      '("#a0aad2" "#a0aad2" "brightblack"  ))
   (base7      '("#b0bae3" "#b0bae3" "brightblack"  ))
   (base8      '("#c0caf5" "#c0caf5" "white"        ))
   (fg-alt     '("#c0caf5" "#c0caf5" "brightwhite"  ))
   (fg         '("#a9b1d6" "#a9b1d6" "white"        ))

   (grey       base4)
   (red        '("#f7768e" "#f7768e" "red"          ))
   (orange     '("#ff9e64" "#ff9e64" "brightred"    ))
   (green      '("#73daca" "#73daca" "green"        ))
   (teal       '("#2ac3de" "#2ac3de" "brightgreen"  ))
   (yellow     '("#e0af68" "#e0af68" "yellow"       ))
   (blue       '("#7aa2f7" "#7aa2f7" "brightblue"   ))
   (dark-blue  '("#565f89" "#565f89" "blue"         ))
   (magenta    '("#bb9af7" "#bb9af7" "magenta"      ))
   (violet     '("#9aa5ce" "#9aa5ce" "brightmagenta"))
   (cyan       '("#b4f9f8" "#b4f9f8" "brightcyan"   ))
   (dark-cyan  '("#7dcfff" "#7dcfff" "cyan"         ))
   ; Additional custom colors
   (dark-green '("#9ece6a" "#9ece6a" "green"        ))
   (brown      '("#cfc9c2" "#cfc9c2" "yellow"       ))

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      base0)
   (builtin        red)
   (comments       (if doom-tokyo-night-brighter-comments base5 base1))
   (doc-comments   (doom-lighten (if doom-tokyo-night-brighter-comments base5 base1) 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        blue)
   (operators      dark-cyan)
   (type           base8)
   (strings        dark-green)
   (variables      base8)
   (numbers        orange)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-tokyo-night-brighter-modeline)
   (-modeline-pad
    (when doom-tokyo-night-padded-modeline
      (if (integerp doom-tokyo-night-padded-modeline) doom-tokyo-night-padded-modeline 4)))

   (modeline-fg     nil)
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
   ((line-number-current-line &override) :foreground base8)
   ((line-number &override) :foreground comments :background (doom-darken bg 0.025))

   (font-lock-comment-face
    :foreground comments
    :background (if doom-tokyo-night-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   ;;; Doom Modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-path :foreground base8 :weight 'normal)
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
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden)

   ;;; web-mode
   (web-mode-json-context-face :foreground brown)
   (web-mode-json-key-face :foreground teal)
   ;;;; Block
   (web-mode-block-delimiter-face :foreground yellow)
   ;;;; Code
   (web-mode-constant-face :foreground constants)
   (web-mode-variable-name-face :foreground variables)
   ;;;; CSS
   (web-mode-css-pseudo-class-face :foreground orange)
   (web-mode-css-property-name-face :foreground blue)
   (web-mode-css-selector-face :foreground teal)
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

;;; doom-tokyo-night-theme.el ends here
