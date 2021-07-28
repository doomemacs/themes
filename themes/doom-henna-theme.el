;;; doom-henna-theme.el --- inspired by vscode henna theme -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;; Code:
(defgroup doom-henna-theme nil
  "Options for the `doom-henna' theme."
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
   (bg-alt     '("#1B1F23" nil       nil            ))
   (base0      '("#10151a" "black"   "black"        ))
   (base1      '("#181A1F" "#2e2e2e" "brightblack"  ))
   (base2      '("#1B1F23" "#262626" "brightblack"  ))
   (base3      '("#262D35" "#3f3f3f" "brightblack"  ))
   (base4      '("#282C34" "#525252" "brightblack"  ))
   (base5      '("#2c313a" "#6b6b6b" "brightblack"  ))
   (base6      '("#3B4048" "#979797" "brightblack"  ))
   (base7      '("#495162" "#dfdfdf" "white"        ))
   (base8      '("#606F73" "#1e1e1e" "brightblack"  ))
   (fg         '("#f8f8f0" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#6B717D" "#979797" "white"        ))
   (grey       '("#737c8c"))
   (red        '("#e74c3c" "#ff6655" "red"          ))
   (green      '("#53df83" "#99bb66" "green"        ))
   (teal       '("#1abc9c" "#44b9b1" "brightgreen"  ))
   (blue       '("#56b5c2" "#51afef" "brightblue"   ))
   (cyan       '("#56b6c2" "#46D9FF" "brightcyan"   ))

   ;; Not used, so remap to other (henna) colors
   (orange     red)
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (magenta    '("#FFB8D1" "#FFB8D1" "magenta"      ))
   (violet     '("#C5A3FF" "#C5A3FF" "brightmagenta"))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (dark-cyan  '("#2e4a54" "#204052" "cyan"         ))

   ;; custom
   (green-alt  '("#9cd230"                          ))
   (green-dark '("#30c965"                          ))

   ;; face categories -- required for all themes
   (highlight      red)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      cyan)
   (builtin        teal)
   (comments       base8)
   (doc-comments   base8)
   (constants      teal)
   (functions      red)
   (keywords       teal)
   (methods        red)
   (operators      red)
   (type           red)
   (strings        green)
   (variables      fg)
   (numbers        teal)
   (region         dark-cyan)
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
   (modeline-fg-alt base7)
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


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-henna-comment-bg (doom-lighten bg 0.05)))
   ((line-number &override) :foreground base7)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   (solaire-default-face  :inherit 'default :background base1)
   ;;;; hl-todo
   (hl-todo :foreground red :weight 'bold)
   ;;;; iedit
   (iedit-occurrence            :foreground blue :weight 'bold :inverse-video t)
   (iedit-read-only-occurrence  :inherit 'region)
   ;;;; doom-modeline
   (doom-modeline-bar                 :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file         :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path         :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; centaur-tabs
   (centaur-tabs-selected                   :background base3 :foreground fg)
   (centaur-tabs-unselected                 :background base2 :foreground grey)
   (centaur-tabs-selected-modified          :background bg :foreground green-alt)
   (centaur-tabs-unselected-modified        :background base1 :foreground magenta)
   (centaur-tabs-active-bar-face            :background green)
   (centaur-tabs-modified-marker-selected   :inherit 'centaur-tabs-selected-modified :foreground green)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected-modified :foreground green)
   ;;;; doom-emacs
   (doom-dashboard-banner      :foreground red)
   (doom-dashboard-footer-icon :foreground green-alt)
   (doom-dashboard-loaded      :foreground green-alt)
   ;;;; which-key
   (which-key-key-face                   :foreground red)
   (which-key-group-description-face     :foreground green)
   (which-key-command-description-face   :foreground teal)
   (which-key-local-map-description-face :foreground green)
   ;;;; highlight-numbers
   (highlight-numbers-number :foreground blue)
   ;;;; ivy
   (ivy-minibuffer-match-highlight  :foreground red)
   (ivy-highlight-face              :foreground red)
   (ivy-minibuffer-match-face-2
    :inherit 'ivy-minibuffer-match-face-1
    :foreground red :background base1 :weight 'semi-bold)
   (ivy-minibuffer-match-face-4
    :inherit 'ivy-minibuffer-match-face-2
    :foreground red :weight 'semi-bold)
   (ivy-current-match :background red :distant-foreground base0 :weight 'normal)
   ;;;; treemacs
   (treemacs-directory-face     :foreground base8)
   (treemacs-git-modified-face  :foreground yellow)
   (treemacs-git-added-face     :foreground green)
   (treemacs-git-untracked-face :foreground green-alt)
   (treemacs-file-face          :foreground fg)
   (treemacs-root-face          :foreground red :weight 'bold)
   ;;;; magit
   (magit-blame-headling              :foreground magenta :background base3)
   (magit-cherry-equvalent            :foreground red)
   (magit-log-author                  :foreground magenta)
   (magit-section-heading             :foreground red :weight 'bold)
   (magit-tag                         :foreground (doom-lighten green-alt 0.5))
   (magit-filename                    :foreground teal)
   (magit-diff-hunk-heading           :background (doom-darken teal 0.5))
   (magit-diff-hunk-heading-highlight :background (doom-darken teal 0.2))
   (magit-branch-current              :foreground green-alt)
   ;;;; popup
   (popup-tip-face        :background base8 :foreground fg)
   (popup-menu-mouse-face :background base8 :foreground fg)
   (popup-summary-face    :background base7 :foreground fg)
   ;;;; rainbow delimiters
   (rainbow-delimiters-depth-1-face :foreground red)
   (rainbow-delimiters-depth-2-face :foreground green)
   (rainbow-delimiters-depth-3-face :foreground teal)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground blue)
   (rainbow-delimiters-depth-6-face :foreground green-alt)
   (rainbow-delimiters-depth-7-face :foreground cyan)
   ;;;; Dired
   (diredfl-date-time    :foreground teal)
   (diredfl-number       :foreground green)
   (diredfl-dir-heading  :foreground teal :weight 'bold)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground cyan)
   (css-property             :foreground teal)
   (css-selector             :foreground red)
   ;;;; markdown-mode
   (markdown-markup-face           :foreground grey)
   (markdown-header-face           :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   (markdown-bold-face             :foreground green :weight 'bold)
   (markdown-url-face              :foreground fg :underline t)
   (markdown-link-face             :foreground green)
   (markdown-list-face             :foregroung fg)
   (markdown-header-face-1         :foreground fg)
   (markdown-header-face-2         :foreground fg)
   (markdown-header-face-3         :foreground fg)
   (markdown-header-face-4         :foreground fg)
   (markdown-header-face-5         :foreground fg)
   (markdown-header-face-6         :foreground fg)
   (markdown-header-delimiter-face :foreground fg)
   (markdown-inline-code-face      :foreground teal)
   ;;;; outline <built-in>
   (outline-1 :foreground red                         :weight 'bold :extend t)
   (outline-2 :foreground teal                        :weight 'bold :extend t)
   (outline-3 :foreground green                       :weight 'bold :extend t)
   (outline-4 :foreground (doom-lighten red 0.25)     :weight 'bold :extend t)
   (outline-5 :foreground (doom-lighten green 0.25)   :weight 'bold :extend t)
   (outline-6 :foreground (doom-lighten blue 0.5)     :weight 'bold :extend t)
   (outline-7 :foreground (doom-lighten red 0.5)      :weight 'bold :extend t)
   (outline-8 :foreground (doom-lighten blue 0.8)     :weight 'bold :extend t)
   ;;;; org <built-in>
   (org-hide              :foreground hidden)
   ((org-code &override)  :foreground blue)
   (org-table             :foreground fg-alt)
   ;;;; web-mode
   (web-mode-html-attr-equal-face  :foreground teal)
   (web-mode-html-tag-face         :foreground green-alt)
   (web-mode-html-tag-bracket-face :foreground teal)
   (web-mode-keyword-face          :foreground teal)
   (web-mode-block-control-face    :foreground red)
   (web-mode-variable-name-face    :foreground (doom-lighten green 0.5))
   ;;;; typescript
   (typescript-access-modifier-face :foreground green-alt)
   (typescript-this-face            :foreground green-alt)
   ;;;; LSP
   (lsp-face-highlight-textual :background "black")
   (lsp-face-highlight-read    :background (doom-darken dark-blue 0.3))
   ;;;; js
   (js2-object-property          :foreground fg)
   (js2-object-property-access   :foreground green)
   (js2-jsdoc-value              :foreground red)
   (js2-jsdoc-tag                :foreground teal)
   (js2-jsdoc-html-tag-delimiter :foreground base8)
   (js2-jsdoc-html-tag-name      :foreground base8)
   ;;;; rjsx
   (rjsx-attr :foreground blue))

  ;;;; Base theme variable overrides-
  ())

;;; doom-henna-theme.el ends here
