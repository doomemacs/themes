;;; doom-moonlight-theme.el --- inspired by VS code's Moonlight -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-moonlight-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-moonlight-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-moonlight-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-moonlight
  "A dark theme inspired by VS code's Moonlight"

  ;; name        default   256       16
  ((bg         '("#212337" "#212337"  "black"))
   (bg-alt     '("#191a2a" "#191a2a" "black"))
   (base0      '("#161a2a" "#161a2a" "black"))
   (base1      '("#191a2a" "#191a2a" "brightblack"))
   (base2      '("#1e2030" "#1e2030" "brightblack"))
   (base3      '("#222436" "#222436" "brightblack"))
   (base4      '("#2f334d" "#2f334d" "brightblack"))
   (base5      '("#444a73" "#444a73" "brightblack"))
   (base6      '("#828bb8" "#828bb8" "brightblack"))
   (base7      '("#a9b8e8" "#a9b8e8" "brightblack"))
   (base8      '("#b4c2f0" "#b4c2f0" "white"))
   (indigo     '("#7a88cf" "#7a88cf" "brightblack"))
   (region     '("#383e5c" "#383e5c" "brightblack"))
   (fg         '("#c8d3f5" "#c8d3f5" "brightwhite"))
   (fg-alt     '("#b4c2f0" "#b4c2f0" "white"))

   (grey base5)

   (dark-red      '("#ff5370" "#ff5370" "red"))
   (red           '("#ff757f" "#ff757f" "red"))
   (light-red     '("#ff98a4" "#ff98a4" "brightred"))
   (orange        '("#ff995e" "#ff995e" "brightred"))
   (green         '("#c3e88d" "#c3e88d" "green"))
   (dark-teal     '("#4fd6be" "#4fd6be" "green"))
   (teal          '("#77e0c6" "#77e0c6" "brightgreen"))
   (light-teal    '("#7af8ca" "#7af8ca" "brightgreen"))
   (yellow        '("#ffc777" "#ffc777" "brightyellow"))
   (blue          '("#82aaff" "#82aaff" "brightblue"))
   (dark-blue     '("#4976eb" "#4976eb" "brightblue"))
   (light-blue    '("#50c4fa" "#50c4fa" "blue"))
   (light-magenta '("#baacff" "#baacff" "brightmagenta"))
   (magenta       '("#c099ff" "#c099ff" "brightmagenta"))
   (violet        '("#f989d3" "#f989d3" "magenta"))
   (light-pink    '("#fca7ea" "#fca7ea" "magenta"))
   (pink          '("#f3c1ff" "#f3c1ff" "magenta"))
   (cyan          '("#b4f9f8" "#b4f9f8" "brightcyan"))
   (dark-cyan     '("#86e1fc" "#86e1fc" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base0)
   (line-highlight base4)
   (selection      region)
   (builtin        magenta)
   (comments       indigo)
   (doc-comments   (doom-lighten comments 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        red)
   (operators      dark-cyan)
   (type           yellow)
   (strings        green)
   (variables      light-red)
   (numbers        orange)
   (region         region)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       teal)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (doom-darken base2 0.1))
   (modeline-bg-alt (doom-darken bg 0.1))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-moonlight-padded-modeline
      (if (integerp doom-moonlight-padded-modeline) doom-moonlight-padded-modeline 4))))

  ;; --- base faces ------------------------
  (((lazy-highlight &override) :background base4 :foreground fg :distant-foreground fg)

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   (doom-modeline-buffer-file       :foreground base7)
   (doom-modeline-icon-inactive     :foreground indigo)
   (doom-modeline-evil-normal-state :foreground dark-cyan)
   (doom-modeline-evil-insert-state :foreground blue)
   (doom-modeline-project-dir       :foreground light-teal)
   (doom-modeline-buffer-path       :foreground blue)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   (hl-line :background line-highlight)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))

   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (solaire-mode-line-face
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (tooltip :background base0 :foreground fg)

   (fringe :background base2)

   ((line-number &override) :foreground base5 :background (doom-darken bg 0.06))
   ((line-number-current-line &override) :foreground fg :background line-highlight)
   ((linum &inherit line-number))

   (font-lock-keyword-face :foreground keywords)
   (font-lock-comment-face :foreground comments)
   (font-lock-doc-face :foreground doc-comments)

   ;; message
   (message-header-name       :foreground green)
   (message-header-subject    :foreground highlight :weight 'bold)
   (message-header-to         :foreground highlight :weight 'bold)
   (message-header-cc         :inherit 'message-header-to :foreground (doom-darken highlight 0.15))
   (message-header-other      :foreground violet)
   (message-header-newsgroups :foreground yellow)
   (message-header-xheader    :foreground doc-comments)
   (message-separator         :foreground comments)
   (message-mml               :foreground comments :slant 'italic)
   (message-cited-text        :foreground magenta)

   ;; --- major-mode faces ------------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; nix-mode
   (nix-attribute-face :foreground blue)
   (nix-builtin-face :foreground dark-teal)

   ;; man-mode
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)

   ;; lsp-mode
   (lsp-face-highlight-read :background region)
   (lsp-face-highlight-textual :background region)
   (lsp-face-highlight-write :background region)
   (lsp-face-semhl-type-primative :foreground orange)
   (lsp-face-semhl-method :foreground magenta)

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground dark-teal)
   (js2-object-property-access :foreground fg-alt)
   (js2-function-param         :foreground pink)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; org-mode
   ((outline-1 &override) :foreground light-blue)
   ((outline-2 &override) :foreground dark-cyan)
   ((outline-3 &override) :foreground light-red)
   ((outline-4 &override) :foreground blue)
   ((outline-5 &override) :foreground magenta)
   ((outline-6 &override) :foreground red)
   ((outline-7 &override) :foreground violet)
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)

   ;; rjsx-mode
   (rjsx-tag :foreground violet)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)

   ;; --- plugin faces -------------------
   ;; all-the-icons
   (all-the-icons-red        :foreground red)
   (all-the-icons-red-alt     :foreground red)
   (all-the-icons-lred       :foreground light-red)
   (all-the-icons-dred       :foreground dark-red)
   (all-the-icons-green      :foreground teal)
   (all-the-icons-green-alt  :foreground teal)
   (all-the-icons-lgreen     :foreground green)
   (all-the-icons-dgreen     :foreground dark-teal)
   (all-the-icons-yellow     :foreground yellow)
   (all-the-icons-yellow-alt :foreground yellow)
   (all-the-icons-lyellow    :foreground (doom-lighten yellow 0.3))
   (all-the-icons-dyellow    :foreground orange)
   (all-the-icons-orange     :foreground orange)
   (all-the-icons-orange-alt :foreground orange)
   (all-the-icons-lorange    :foreground orange)
   (all-the-icons-dorange    :foreground orange)
   (all-the-icons-blue       :foreground blue)
   (all-the-icons-blue-alt   :foreground teal)
   (all-the-icons-lblue      :foreground (doom-lighten blue 0.3))
   (all-the-icons-dblue      :foreground (doom-darken blue 0.1))
   (all-the-icons-maroon     :foreground magenta)
   (all-the-icons-maroon-alt :foreground magenta)
   (all-the-icons-lmaroon    :foreground light-magenta)
   (all-the-icons-dmaroon    :foreground magenta)
   (all-the-icons-purple     :foreground magenta)
   (all-the-icons-purple-alt :foreground magenta)
   (all-the-icons-lpurple    :foreground light-magenta)
   (all-the-icons-dpurple    :foreground magenta)
   (all-the-icons-cyan       :foreground dark-cyan)
   (all-the-icons-cyan-alt   :foreground dark-cyan)
   (all-the-icons-lcyan      :foreground (doom-lighten dark-cyan 0.3))
   (all-the-icons-dcyan      :foreground dark-cyan)
   (all-the-icons-pink       :foreground pink)
   (all-the-icons-pink-alt   :foreground pink)
   (all-the-icons-lpink      :foreground light-pink)
   (all-the-icons-dpink      :foreground pink)
   (all-the-icons-silver     :foreground (doom-lighten grey 0.2))
   (all-the-icons-silver-alt :foreground (doom-lighten grey 0.2))
   (all-the-icons-lsilver    :foreground (doom-lighten grey 0.4))
   (all-the-icons-dsilver    :foreground grey)

   ;; all-the-icons-dired
   (all-the-icons-dired-dir-face :foreground indigo)

   ;; company
   (company-tooltip :inherit 'tooltip)
   (company-tooltip-common :foreground highlight)

   ;; company-box
   (company-box-annotation :foreground base7)

   ;; doom-dashboard
   (doom-dashboard-menu-desc :foreground dark-cyan)
   (doom-dashboard-menu-tile :foreground dark-teal)
  
   ;; diredfl
   (diredfl-date-time    :foreground blue)
   (diredfl-file-name    :foreground base7)
   (diredfl-file-suffix  :foreground base6)
   (diredfl-symlink      :foreground dark-cyan)

   ;; dired+
   (diredp-number :foreground orange)

   ;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored  :foreground cyan)
   (dired-k-added    :foreground vc-added)

   ;; magit
   (magit-filename :foreground teal)

   ;; markdown-mode
   (markdown-header-face           :inherit 'bold :foreground yellow)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground magenta :inherit 'italic)
   (markdown-list-face             :foreground red)
   (markdown-url-face              :inherit 'underline :foreground orange)
   (markdown-gfm-checkbox-face     :foreground blue)
   (markdown-blockquote-face       :inherit 'italic :foreground fg)
   (mmm-default-submode-face       :background base1)

   ;; nav-flash
   (nav-flash-face :background region)

   ;; ivy-posframe
   (ivy-posframe :background base0)
   (ivy-posframe-border :background base0)

   ;; popup
   (popup-face :inherit 'tooltip)
   (popup-selection-face :inherit 'tooltip)

   ;; pos-tip
   (popup          :inherit 'tooltip)
   (popup-tip-face :inherit 'tooltip)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground light-red)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)

   ;; treemacs
   (treemacs-directory-face :foreground blue)
   (treemacs-git-modified-face :foreground blue)

   ;; workspaces
   (+workspace-tab-selected-face :background region :foreground blue)

   ;; which-key
   (which-func :foreground blue)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground magenta)
   (which-key-local-map-description-face :foreground cyan)))


;;; doom-moonlight-theme.el ends here
