;;; doom-henna-theme.el --- inspired by vscode henna theme -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;; Code:
(defgroup doom-henna-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-henna-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-henna-theme
  :type 'boolean)

(defcustom doom-henna-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-henna-theme
  :type 'boolean)

(defcustom doom-henna-comment-bg doom-henna-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-henna-theme
  :type 'boolean)

(defcustom doom-henna-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-henna-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-henna
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#21272e" nil       nil            ))
   (bg-alt     '("#21242b" nil       nil            ))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#181A1F" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#606F73" "#3f3f3f" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#f8f8f8" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#737c8c" "#979797" "white"        ))
   (grey       base4)
   (red        '("#e74c3c" "#ff6655" "red"          ))
   (green      '("#53df83" "#99bb66" "green"        ))
   (teal       '("#1abc9c" "#44b9b1" "brightgreen"))
   (blue       '("#56b5c2" "#51afef" "brightblue"   ))
   (cyan       '("#56b6c2" "#46D9FF" "brightcyan"   ))

   ;; Not used
   (orange     '("#da8548" "#dd8844" "brightred"    ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (magenta    '("#c678dd" "#c678dd" "brightmagenta"))
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; custom
   (green-alt  '("#9cd230"                          ))

   ;; face categories -- required for all themes
   (highlight      teal)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      cyan)
   (builtin        teal)
   (comments       base4) ;;(if doom-henna-brighter-comments dark-cyan base5))
   (doc-comments   green);; (doom-lighten (if doom-henna-brighter-comments dark-cyan base5) 0.25))
   (constants      teal)
   (functions      red)
   (keywords       teal)
   (methods        red)
   (operators      red)
   (type           red)
   (strings        green)
   (variables      fg)
   (numbers        teal)
   (region         (doom-darken dark-cyan 0.5)) ;;`(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green-alt)
   (vc-deleted     (doom-darken red 0.2))

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-henna-brighter-modeline)
   (-modeline-pad
    (when doom-henna-padded-modeline
      (if (integerp doom-henna-padded-modeline) doom-henna-padded-modeline 4)))


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

  ;; Operator Fonts
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground fg)

   (hl-line :background "black")

   (font-lock-operator-face
    :foreground operators)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-henna-comment-bg (doom-lighten bg 0.05)))
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
   (solaire-default-face  :inherit 'default :background base1)

   ;; hl-todo
   (hl-todo :foreground red :weight 'bold)

   ;; iedit
   (iedit-occurrence :foreground blue :weight 'bold :inverse-video t)
   (iedit-read-only-occurrence :inherit 'region)

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; Doom dashboard
   (doom-dashboard-banner      :foreground red)
   (doom-dashboard-footer-icon :foreground green-alt)
   (doom-dashboard-loaded      :foreground green-alt)

   ;; which-key
   (which-key-key-face                   :foreground red)
   (which-key-group-description-face     :foreground green)
   (which-key-command-description-face   :foreground teal)
   (which-key-local-map-description-face :foreground green)

   ;; ivy-mode
   (ivy-minibuffer-match-highlight :foreground red)
   (ivy-highlight-face :foreground red)
   (ivy-minibuffer-match-face-2
     :inherit 'ivy-minibuffer-match-face-1
     :foreground red :background base1 :weight 'semi-bold)
   (ivy-minibuffer-match-face-4
     :inherit 'ivy-minibuffer-match-face-2
     :foreground red :weight 'semi-bold)
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; treemacs
   (treemacs-directory-face    :foreground base8)
   (treemacs-git-modified-face :foreground yellow)
   (treemacs-file-face         :foreground base8)
   (treemacs-root-face         :foreground red :weight 'bold)

   ;; magit
   (magit-section-heading :foreground red :weight 'bold)
   (magit-tag             :foreground yellow)
   (magit-filename        :foreground teal)

   ;; rainbow delimiters
   (rainbow-delimiters-depth-1-face :foreground red)
   (rainbow-delimiters-depth-2-face :foreground green)
   (rainbow-delimiters-depth-3-face :foreground teal)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground blue)
   (rainbow-delimiters-depth-6-face :foreground green-alt)
   (rainbow-delimiters-depth-7-face :foreground cyan)

   ;; Dired
   (diredfl-date-time :foreground teal)
   (diredfl-number    :foreground green)
   (diredfl-dir-heading :foreground teal :weight 'bold)
   
   ;; --- major-mode faces -------------------

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground cyan)
   (css-property             :foreground teal)
   (css-selector             :foreground red)

   ;; markdown-mode
   (markdown-markup-face           :foreground base5)
   (markdown-header-face           :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   (markdown-bold-face             :foreground blue :weight 'bold)

   ;; org-mode
   (org-hide              :foreground hidden)
   (solaire-org-hide-face :foreground hidden)
   (org-code              :foreground blue)

   ;; outline
   (outline-1 :foreground red                         :weight 'bold :extend t)
   (outline-2 :foreground teal                        :weight 'bold :extend t)
   (outline-3 :foreground green                       :weight 'bold :extend t)
   (outline-4 :foreground (doom-lighten red 0.25)     :weight 'bold :extend t)
   (outline-5 :foreground (doom-lighten green 0.25)   :weight 'bold :extend t)
   (outline-6 :foreground (doom-lighten blue 0.5)     :weight 'bold :extend t)
   (outline-7 :foreground (doom-lighten red 0.5)      :weight 'bold :extend t)
   (outline-8 :foreground (doom-lighten blue 0.8)     :weight 'bold :extend t)

   ;; web-mode
   (web-mode-html-attr-equal-face  :foreground teal)
   (web-mode-html-tag-face         :foreground green-alt)
   (web-mode-html-tag-bracket-face :foreground teal)
   (web-mode-keyword-face          :foreground teal)
   (web-mode-block-control-face    :foreground red)
   (web-mode-block-delimiter-face  :foreground teal)
   (web-mode-variable-name-face    :foreground green)

   ;; typescript
   (typescript-access-modifier-face :foreground green-alt)
   (typescript-this-face            :foreground green-alt)

   ;; LSP
   (lsp-face-highlight-textual :background "black")
   (lsp-face-highlight-read    :background (doom-darken dark-blue 0.3))

   ;; js
   (js2-object-property :foreground fg)
   (js2-object-property-access :foreground green)


   )

  ;; --- extra variables ---------------------
  ()

  )

;;; doom-henna-theme.el ends here
