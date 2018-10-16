;; doom-molokai-theme.el --- inspired by Textmate's monokai
(require 'doom-themes)

;;
(defgroup doom-molokai-theme nil
  "Options for doom-molokai."
  :group 'doom-themes)

(defcustom doom-one-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-molokai-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-molokai-theme
  :type 'boolean)

(defcustom doom-one-comment-bg doom-one-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-molokai-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-molokai-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-molokai
  "A dark, vibrant theme inspired by Textmate's monokai."

  ;; name        gui       256       16
  ((bg         '("#1c1e1f" nil       nil))
   (bg-alt     '("#222323" nil       nil))
   (base0      '("#000000" "black"   "black"      ))
   (base1      '("#151617" "#101010" "brightblack"))
   (base2      '("#1d1f20" "#191919" "brightblack"))
   (base3      '("#2d2e2e" "#252525" "brightblack"))
   (base4      '("#4e4e4e" "#454545" "brightblack"))
   (base5      '("#555556" "#6b6b6b" "brightblack"))
   (base6      '("#767679" "#7b7b7b" "brightblack"))
   (base7      '("#cfc0c5" "#c1c1c1" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "brightwhite"))
   (fg         '("#d6d6d4" "#dfdfdf" "brightwhite"))
   (fg-alt     '("#556172" "#4d4d4d" "white"))

   (grey       '("#525254" "#515154" "brightblack"))
   (red        '("#e74c3c" "#e74c3c" "red"))
   (orange     '("#fd971f" "#fd971f" "brightred"))
   (green      '("#b6e63e" "#b6e63e" "green"))
   (teal       green)
   (yellow     '("#e2c770" "#e2c770" "yellow"))
   (blue       '("#268bd2" "#2686D6" "brightblue"))
   (dark-blue  '("#727280" "#727280" "blue"))
   (magenta    '("#fb2874" "#fb2874" "magenta"))
   (violet     '("#9c91e4" "#9c91e4" "brightmagenta"))
   (cyan       '("#66d9ef" "#66d9ef" "brightcyan"))
   (dark-cyan  '("#8fa1b3" "#8FA1B3" "cyan"))

   ;; face categories
   (highlight      orange)
   (vertical-bar   (doom-darken base4 0.1))
   (selection      base5)
   (builtin        orange)
   (comments       (if doom-molokai-brighter-comments violet base5))
   (doc-comments   (if doom-molokai-brighter-comments (doom-lighten violet 0.1) (doom-lighten base5 0.25)))
   (constants      orange)
   (functions      green)
   (keywords       magenta)
   (methods        cyan)
   (operators      violet)
   (type           cyan)
   (strings        yellow)
   (variables      orange)
   (numbers        violet)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    cyan)
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-one-brighter-modeline)
   (-modeline-pad
    (when doom-molokai-padded-modeline
      (if (integerp doom-molokai-padded-modeline)
          doom-molokai-padded-modeline
        4)))

   (modeline-fg     nil)
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

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))


  ;; --- extra faces ------------------------
  ((lazy-highlight :background violet :foreground base0 :distant-foreground base0 :bold bold)
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   (cursor :background magenta)
   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-one-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background base3 :foreground base8
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base3)))
   (mode-line-inactive
    :background (doom-darken base2 0.2) :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base2)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (doom-modeline-bar :background green)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-modified :inherit 'bold :foreground orange)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ((line-number &override) :foreground base5 :distant-foreground nil)
   ((line-number-current-line &override) :foreground base7 :distant-foreground nil)

   (isearch :foreground base0 :background green)

   ;; ediff
   (ediff-fine-diff-A :background (doom-blend magenta bg 0.3) :weight 'bold)

   ;; evil-mode
   (evil-search-highlight-persist-highlight-face :background violet)

   ;; evil-snipe
   (evil-snipe-first-match-face :foreground base0 :background green)
   (evil-snipe-matches-face     :foreground green :underline t)

   ;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,green)  :background base3)

   ;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)

   ;; ivy
   (ivy-current-match :background base3)
   (ivy-minibuffer-match-face-1 :background base1 :foreground base4)

   ;; neotree
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground magenta)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground orange)
   (rainbow-delimiters-depth-7-face :foreground green)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground keywords)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-blockquote-face :inherit 'italic :foreground dark-blue)
   (markdown-list-face :foreground magenta)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'bold :foreground blue)
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base2 0.045))

   ;; org-mode
   (org-level-1 :background base2 :foreground magenta :bold bold :height 1.2)
   (org-level-2 :inherit 'org-level-1 :foreground orange)
   (org-level-3 :bold bold :foreground violet)
   (org-level-4 :inherit 'org-level-3)
   (org-level-5 :inherit 'org-level-3)
   (org-level-6 :inherit 'org-level-3)
   (org-ellipsis :underline nil :background base2 :foreground orange)
   (org-tag :foreground yellow :bold nil)
   (org-quote :inherit 'italic :foreground base7 :background org-quote)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow))
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-molokai-theme.el ends here
