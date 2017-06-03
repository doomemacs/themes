;; DOOM Molokai (inspired by Textmate's monokai)
(require 'doom-themes)

;;
(defgroup doom-molokai-theme nil
  "Options for doom-molokai."
  :group 'doom-themes)

(defcustom doom-molokai-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-molokai-theme
  :type 'boolean)

;;
(def-doom-theme doom-molokai
  "A dark, vibrant theme inspired by Textmate's monokai."

  ;; name        gui       256       16
  ((bg         '("#1c1e1f" nil       nil))
   (bg-alt     '("#222323" nil       nil))
   (base0      '("#000000"))
   (base1      '("#0f1e20" "#0f1f1f" "brightblack"))
   (base2      `("#1d1f20" "#141414" "brightblack"))
   (base3      `(,(doom-lighten (car bg-alt) 0.05) "#252525" "brightblack"))
   (base4      `(,(doom-lighten (car bg-alt) 0.2) "#353535" "brightblack"))
   (base5      '("#4a5050" "#4d5d5d" "brightblack"))
   (base6      '("#767679" "#6b6b6b" "brightblack"))
   (base7      '("#cfc0c5" "#c1c1c1" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "brightwhite"))
   (fg         '("#d6d6d4" "#dfdfdf" "brightwhite"))
   (fg-alt     '("#556172" "#4d4d4d" "white"))

   (grey       '("#525254" "#515154" "brightblack"))
   (red        '("#e74c3c" "red"))
   (orange     '("#fd971f" "#F99919" "brightred"))
   (green      '("#b6e63e" "green"))
   (teal       green)
   (yellow     '("#e2c770" "yellow"))
   (blue       '("#268bd2" "#2686D6" "brightblue"))
   (dark-blue  '("#727280" "blue"))
   (magenta    '("#fb2874" "magenta"))
   (violet     '("#9c91e4" "brightmagenta"))
   (cyan       '("#66d9ef" "cyan"    "brightcyan"))
   (dark-cyan  '("#8fa1b3" "#8FA1B3" "cyan"))

   ;; face categories
   (highlight      orange)
   (vertical-bar   base2)
   (current-line   `(,(doom-lighten (car bg) 0.05) "#1f1f1f"))
   (selection      base0)
   (builtin        orange)
   (comments       (if doom-molokai-brighter-comments violet base5))
   (doc-comments   (if doom-molokai-brighter-comments (doom-lighten violet 0.1) (doom-lighten base5 0.2)))
   (constants      orange)
   (functions      green)
   (keywords       magenta)
   (methods        cyan)
   (operators      violet)
   (type           cyan)
   (strings        yellow)
   (variables      orange)
   (numbers        blue)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    base4)
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     red)

   ;; custom categories
   ;; n/a
   )


  ;; --- extra faces ------------------------
  ((mode-line          :background base3 :foreground base8)
   (mode-line-inactive :background base2 :foreground base4)

   ;;(solaire-mode-line-face :inherit 'mode-line :background base3)
   ;;(solaire-mode-line-inactive-face :inherit 'mode-line-inactive :background base2)

   (doom-modeline-buffer-modified :foreground orange :bold bold)
   (doom-modeline-buffer-path :foreground (if bold base8 orange))

   (linum :foreground base5 :distant-foreground nil :bold nil)
   (nlinum-hl-face :foreground orange :distant-foreground nil :bold bold)

   (isearch :foreground base0 :background green)
   (isearch-lazy-highlight-face :foreground base0 :background violet)

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
   (helm-match :foreground keywords)
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)

   ;; ivy
   (ivy-current-match :background base3)

   ;; neotree
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground magenta)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground yellow)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground cyan)
   (rainbow-delimiters-depth-6-face :foreground magenta)
   (rainbow-delimiters-depth-7-face :foreground orange)


   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground keywords)

   ;; markdown-mode
   (markdown-header-face     :foreground orange)
   (markdown-blockquote-face :foreground dark-blue)
   (markdown-markup-face     :foreground cyan)
   (markdown-list-face       :foreground magenta)
   (markdown-pre-face        :foreground cyan)

   ;; org-mode
   (org-level-1 :background base2 :foreground magenta :bold bold :height 1.2)
   (org-level-2 :inherit 'org-level-1 :foreground orange)
   (org-level-3 :bold bold :foreground violet)
   (org-level-4 :inherit 'org-level-3)
   (org-level-5 :inherit 'org-level-3)
   (org-level-6 :inherit 'org-level-3)
   (org-ellipsis :underline nil :background base2 :foreground orange)
   (org-tag :foreground yellow :bold nil)
   (org-quote :inherit 'italic :foreground base7 :background current-line)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow))


  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-molokai-theme.el ends here
