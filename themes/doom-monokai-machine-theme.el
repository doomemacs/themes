;;; doom-monokai-machine-theme.el --- inspired by Monokai Pro (Filter Machine) -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-monokai-machine-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-monokai-machine-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-monokai-machine-theme
  :type 'boolean)

(defcustom doom-monokai-machine-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-monokai-machine-theme
  :type 'boolean)

(defcustom doom-monokai-machine-comment-bg doom-monokai-machine-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-monokai-machine-theme
  :type 'boolean)

(defcustom doom-monokai-machine-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-monokai-machine-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-monokai-machine
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#273136" nil       nil            ))
   (bg-alt     '("#1d2528" nil       nil            ))
   (base0      '("#1a2023" "#121212" "black"        ))
   (base1      '("#1d2528" "#1c1c1c" "brightblack"  ))
   (base2      '("#222f2c" "#262626" "brightblack"  ))
   (base3      '("#374c4b" "#3a3a3a" "brightblack"  ))
   (base4      '("#545f62" "#585858" "brightblack"  ))
   (base5      '("#6b7678" "#585858" "brightblack"  ))
   (base6      '("#929f9c" "#6c6c6c" "brightblack"  ))
   (base7      '("#b2bfbc" "#8a8a8a" "brightblack"  ))
   (base8      '("#d2dfdc" "#bcbcbc" "white"        ))
   (fg         '("#f2fffc" "#ffffff" "brightwhite"  ))
   (fg-alt     '("#d2dfdc" "#c6c6c6" "white"        ))

   (grey       base4)
   (red        '("#ff6d7e" "#ff0000" "red"          ))
   (orange     '("#ffb270" "#ff7f50" "brightred"    ))
   (green      '("#a2e578" "#90ee90" "green"        ))
   (teal       '("#7cd5f1" "#40e0d0" "brightgreen"  ))
   (yellow     '("#ffed72" "#f0e68c" "yellow"       ))
   (violet     '("#baa0f8" "#9370db" "magenta"      ))
   (blue       '("#7cd5f1" "#51afef" "brightblue"   ))
   (dark-blue  blue)
   (magenta    violet)
   (cyan       blue)
   (dark-cyan  blue)

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      base3)
   (builtin        magenta)
   (comments       (if doom-monokai-machine-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-monokai-machine-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      green)
   (keywords       red)
   (methods        green)
   (operators      red)
   (type           cyan)
   (strings        yellow)
   (variables      fg)
   (numbers        violet)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-monokai-machine-brighter-modeline)
   (-modeline-pad
    (when doom-monokai-machine-padded-modeline
      (if (integerp doom-monokai-machine-padded-modeline) doom-monokai-machine-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((cursor :background fg)
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-monokai-machine-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background base3 :distant-foreground base7 :weight 'normal)

   ;; isearch
   (match          :foreground yellow :background base5)
   (isearch        :inherit 'match)
   (lazy-highlight :inherit 'match)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;; lsp-mode
   (lsp-headerline-breadcrumb-separator-face :foreground green)

   ;; tree-sitter
   (tree-sitter-hl-face:method.call        :foreground green)
   (tree-sitter-hl-face:function.call      :foreground green)
   (tree-sitter-hl-face:type.builtin       :foreground blue)
   (tree-sitter-hl-face:variable.parameter :foreground orange)
   (tree-sitter-hl-face:variable.special   :foreground blue)
   (tree-sitter-hl-face:punctuation        :foreground base6)
   (tree-sitter-hl-face:tag        :foreground red)
   (tree-sitter-hl-face:attribute        :foreground blue)

   ;; highlight escape sequence
   (hes-escape-backslash-face              :foreground violet)
   (hes-escape-sequence-face               :foreground violet)

   ;; rjsx
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange))

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-monokai-machine-theme.el ends here
