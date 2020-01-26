;;; doom-dark+-theme.el --- inspired by dark+ Theme by equinusocio -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-dark+-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-dark+-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-dark+-theme
  :type '(choice integer boolean))

(defcustom doom-dark+-blue-modeline nil
  "If non-nil, mode-line's color will be blue instead of the default purple."
  :group 'doom-dark+-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-dark+
  "A dark theme inspired by dark+ Theme by equinusocio"

  ;; name        default   256       16
  ((bg         '("#1e1e1e" nil       nil))
   (bg-alt     '("#252526" nil       nil))
   (base0      '("#171F24" "black"   "black"))
   (base1      '("#1C1C1C" "#1C1C1C" "brightblack"))
   (base2      '("#121212" "#626262" "brightblack"))
   (base3      '("#313131" "#3a3a3a" "brightblack"))
   (base4      '("#4b474c" "#444444" "brightblack"))
   (base5      '("#37474F" "#585858" "brightblack"))
   (base6      '("#237AD3" "#2277DD" "brightblack"))
   (base7      '("#777778" "#767676" "brightblack"))
   (base8      '("#f4f4f4" "#a8a8a8" "white"))
   (fg         '("#d4d4d4" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#AEAFAD" "#bcbcbc" "white"))

   (grey base7)
   (red          '("#D16969" "#DD6666" "red"))
   (orange       '("#DB8E73" "#DD8877" "brightred"))
   (green        '("#579C4C" "#559944" "green"))
   (light-green  '("#B5CEA8" "#BBCCAA" "green"))
   (teal         '("#35CDAF" "#33CCAA" "brightgreen"))
   (yellow       '("#D7BA7D" "#DDBB77" "brightyellow"))
   (light-yellow '("#D9DAA2" "#DDDDAA" "brightyellow"))
   (blue         '("#339CDB" "#3399DD" "brightblue"))
   (dark-blue    '("#124F7B" "#114477" "blue"))
   (magenta      '("#C586C0" "#CC88CC" "brightmagenta"))
   (violet       '("#BB80B3" "#BB88BB" "magenta"))
   (dark-violet  '("#68217A" "#662277" "magenta"))
   (cyan         '("#85DDFF" "#5FD7FF" "brightcyan"))
   (dark-cyan    '("#207FA1" "#2277AA" "cyan"))

   ;; face categories -- required for all themes
   (highlight      base6)
   (vertical-bar   base2)
   (selection      base4)
   (builtin        magenta)
   (comments       green)
   (doc-comments   base7)
   (constants      blue)
   (functions      light-yellow)
   (keywords       blue)
   (methods        light-yellow)
   (operators      cyan)
   (type           teal)
   (strings        orange)
   (variables      cyan)
   (numbers        light-green)
   (region         (doom-darken base6 0.5))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (if doom-dark+-blue-modeline base6 dark-violet))
   (modeline-bg-alt (doom-darken bg 0.01))
   (modeline-fg     base8)
   (modeline-fg-alt blue)

   (-modeline-pad
    (when doom-dark+-padded-modeline
      (if (integerp doom-dark+-padded-modeline) doom-dark+-padded-modeline 4))))

  ;; --- base faces ------------------------
  ((highlight            :background highlight  :foreground base8 :distant-foreground base8)
   ((lazy-highlight &override) :background base4 :foreground fg :distant-foreground fg :bold bold)
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))

   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (mode-line-emphasis
    :foreground fg
    :weight 'bold)

   (solaire-mode-line-face
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (fringe :background base2)

   ;; --- major-mode faces ------------------------
   ;; all-the-icons
   (all-the-icons-dblue    :foreground blue)

   ;; man-mode
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)

   ;; centaur-tabs
   (centaur-tabs-active-bar-face :background base6)
   (centaur-tabs-selected-modified :inherit 'centaur-tabs-selected
				   :foreground fg
				   :weight 'bold)
   (centaur-tabs-unselected-modified :inherit 'centaur-tabs-unselected
				     :foreground fg
				     :weight 'bold)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected
					  :foreground fg)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected
					    :foreground fg)
   ;; dashboard
   (dashboard-heading :foreground green :weight 'bold)
   
   ;; doom-modeline
   (doom-modeline-bar :background (if doom-dark+-blue-modeline base6 dark-violet))
   (doom-modeline-info :inherit 'mode-line-emphasis)
   (doom-modeline-urgent :inherit 'mode-line-emphasis)
   (doom-modeline-warning :inherit 'mode-line-emphasis)
   (doom-modeline-debug :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-minor-mode :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-project-dir :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-project-parent-dir :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-persp-name :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-file :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-modified :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-lsp-success :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :inherit 'mode-line-emphasis)
   (doom-modeline-evil-insert-state :foreground cyan)
   (doom-modeline-evil-visual-state :foreground yellow)
   
   ;; org-mode
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)

   ;; --- plugin faces -------------------
   ;; company
   (company-tooltip-selection     :background region)
   
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)

   ;; ivy
   (counsel-active-mode :foreground (doom-lighten base6 0.1))
   (ivy-minibuffer-match-face-2 :foreground (doom-lighten base6 0.1) :weight 'extra-bold)

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground cyan)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; lsp-mode
   (lsp-lens-face              :foreground base7 :height 0.8)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)

   ;; org-pomodoro
   (org-pomodoro-mode-line :inherit 'mode-line-emphasis) ; unreadable otherwise
   (org-pomodoro-mode-line-overtime :inherit 'org-pomodoro-mode-line)
   (org-pomodoro-mode-line-break :inherit 'org-pomodoro-mode-line)

   ;; rjsx-mode
   (rjsx-tag :foreground blue)
   (rjsx-attr :foreground cyan :slant 'italic :weight 'medium)

   ;; treemacs
   (treemacs-root-face :foreground fg :weight 'ultra-bold :height 1.2)
   (doom-themes-treemacs-root-face :foreground fg :weight 'ultra-bold :height 1.2)
   (doom-themes-treemacs-file-face :foreground fg)
   (treemacs-directory-face :foreground fg)
   (treemacs-git-modified-face :foreground blue)

   ;; tooltip
   (tooltip :background base2 :foreground fg)))

(provide 'doom-dark+-theme)
