;;; doom-badger-theme.el --- inspired by original badger theme -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-badger-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-badger-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-badger-theme
  :type 'boolean)

(defcustom doom-badger-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-badger-theme
  :type 'boolean)

(defcustom doom-badger-comment-bg doom-badger-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-badger-theme
  :type 'boolean)

(defcustom doom-badger-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-badger-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-badger
  "A dark theme inspired by original badger theme"

  ;; name        default   256       16
  ((bg         '("#171717" nil       nil            ))
   (bg-alt     '("#1c1c1c" nil       nil            ))
   (base0      '("#1D1D1D" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#433F4f" "#3f3f3f" "brightblack"  ))
   (base5      '("#635770" "#525252" "brightblack"  ))
   (base6      '("#656868" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#F6F3E8" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#FBF9F3" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#E2434C" "#ff6655" "red"          ))
   (orange     '("#EA9847" "#dd8844" "brightred"    ))
   (green      '("#86B187" "#99bb66" "green"        ))
   (teal       '("#65A399" "#44b9b1" "brightgreen"  ))
   (yellow     '("#E0D063" "#ECBE7B" "yellow"       ))
   (blue       '("#8AC6F2" "#51afef" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#E18Cbb" "#c678dd" "brightmagenta"))
   (violet     '("#BF93C3" "#a9a1e1" "magenta"      ))
   (cyan       '("cyan"    "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; Custom
   (olive      '("#9AA68E"))
   (lime       '("#84C452"))
   (link       '("#8ACDAA"))
   (dull-red   '("#A55662"))
   (brown      '("#AC8952"))
   (sand       '("#C7B299"))
   (salmon     '("#F28B86"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base4 0.1))
   (selection      dark-blue)
   (builtin        salmon)
   (comments       (if doom-badger-brighter-comments dark-cyan base6))
   (doc-comments   (doom-lighten (if doom-badger-brighter-comments dark-cyan base6) 0.25))
   (constants      base5)
   (functions      orange)
   (keywords       blue)
   (methods        cyan)
   (operators      sand)
   (type           sand)
   (strings        green)
   (variables      magenta)
   (numbers        fg)
   (region         `(,(doom-lighten (car bg-alt) 0.1) ,@(doom-lighten (cdr base0) 0.1)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-badger-brighter-modeline)
   (-modeline-pad
    (when doom-badger-padded-modeline
      (if (integerp doom-badger-padded-modeline) doom-badger-padded-modeline 4)))

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
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-badger-comment-bg (doom-lighten bg 0.05)))
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
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; treemacs
   (treemacs-directory-face    :foreground base6)
   (treemacs-git-modified-face :foreground yellow)
   (treemacs-file-face         :foreground base8)
   (treemacs-root-face         :foreground blue :weight 'bold)
   (doom-themes-treemacs-file-face :foreground blue)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   (org-document-info-keyword :foreground olive)
   (org-document-title :foreground salmon :height 1.50)

   (org-archived :foreground fg :weight 'bold)
   ;; (org-checkbox :foreground fg+alt :foreground olive
   ;;                                 :box (:line-width 1 :style released-button))
   (org-done :foreground lime :strike-through t)
   (org-todo :foreground red)
   (org-formula :foreground violet)
   (org-headline-done :strike-through t :foreground base6)
   (org-hide :foreground bg)
   (org-level-1 :foreground blue)
   (org-level-2 :foreground violet)
   (org-level-3 :foreground orange)
   (org-level-4 :foreground yellow)
   (org-level-5 :foreground salmon)
   (org-level-6 :foreground green)
   (org-level-7 :foreground brown)
   (org-level-8 :foreground teal)
   (org-link :foreground link :underline t)

   (org-agenda-date :foreground blue)
   (org-deadline-announce :foreground dull-red)
   (org-date :foreground link :underline t)
   (org-agenda-date-today  :foreground salmon :weight 'light :slant 'italic)
   (org-agenda-structure  :inherit font-lock-comment-face)
   ;; `(org-scheduled ((t (:foreground ,zenburn-green+4))))x
   ;; `(org-scheduled-previously ((t (:foreground ,zenburn-red-4))))
   ;; `(org-scheduled-today ((t (:foreground ,zenburn-blue+1))))
   ;; `(org-sexp-date ((t (:foreground ,zenburn-blue+1 :underline t))))
   ;; `(org-time-grid ((t (:foreground ,zenburn-orange))))
   ;; `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))

   (org-special-keyword :foreground olive :weight 'normal)
   (org-table :foreground olive)
   (org-tag :bold t :foreground orange :strike-through nil)
   (org-warning :bold t :foreground magenta :weight 'bold)
   (org-column :background "black")
   (org-column-title :background "black" :foreground lime :underline t)
   (org-mode-line-clock :foreground yellow)
   (org-footnote :foreground link :underline t)
   (org-code :foreground olive)
   (org-verbatim :inherit 'org-code)

   ;; web-mode
   (web-mode-html-attr-equal-face  :foreground fg)
   (web-mode-html-attr-name-face   :foreground base8)
   (web-mode-html-tag-face         :foreground blue)
   (web-mode-html-tag-bracket-face :foreground sand)
   (web-mode-keyword-face          :foreground blue)
   (web-mode-block-control-face    :foreground orange)
   (web-mode-block-delimiter-face  :foreground sand)
   (web-mode-variable-name-face    :foreground (doom-lighten constants 0.3))
   )
  ;; --- extra variables ---------------------
  ()
  )

;;; doom-badger-theme.el ends here
