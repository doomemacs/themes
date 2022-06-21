;;; doom-shades-of-purple-theme.el --- a port of VSCode's Shades of Purple -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 22, 2021 (#521)
;; Author: jwbaldwin <https://github.com/jwbaldwin>
;; Maintainer:
;; Source: https://github.com/jwbaldwin/spacemacs-shades-of-purple
;; Source: https://github.com/ahmadawais/shades-of-purple-vscode
;;
;;; Commentary:
;;
;; Background            #2D2B55   #2D2B55
;; Background Dark       #1E1E3F   #1E1E3F
;; Foreground            #A599E9   #A599E9
;; Hover Background      #4D21FC   #4D21FC
;; Contrast              #FAD000   #FAD000
;; Contrast Lite         #FFEE80   #FFEE80
;; Contrast Lite II      #FAEFA5   #FAEFA5
;; Highlight             #FF7200   #FF7200
;; Comment               #B362FF   #B362FF
;; Constants             #FF628C   #FF628C
;; Keywords              #FF9D00   #FF9D00
;; Other                 #9EFFFF   #9EFFFF
;; Strings               #A5FF90   #A5FF90
;; Templates             #3AD900   #3AD900
;; Definitions           #FB94FF   #FB94FF
;; Invalid               #EC3A37F5 #EC3A37F5
;; Diff Added            #00FF009A #00FF009A
;; Diff Removed          #FF000D81 #FF000D81
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-shades-of-purple-theme nil
  "Options for the `doom-shades-of-purple' theme."
  :group 'doom-themes)

(defcustom doom-shades-of-purple-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-shades-of-purple-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-shades-of-purple
  "A dark theme inspired by VS code's shades-of-purple"

  ;; name        default   256       16
  ((bg         '("#2d2b55" "#2d2b55"  "black"))
   (bg-alt     '("#1e1e3f" "#1e1e3f" "black"))
   (base0      '("#161a2a" "#161a2a" "black"))
   (base1      '("#191a2a" "#191a2a" "brightblack"))
   (base2      '("#1e2030" "#1e2030" "brightblack"))
   (base3      '("#222436" "#222436" "brightblack"))
   (base4      '("#a599e9" "#a599e9" "brightblack"))
   (base5      '("#444a73" "#444a73" "brightblack"))
   (base6      '("#828bb8" "#828bb8" "brightblack"))
   (base7      '("#a9b8e8" "#a9b8e8" "brightblack"))
   (base8      '("#b4c2f0" "#b4c2f0" "white"))
   (indigo     '("#7a88cf" "#7a88cf" "brightblack"))
   (region     '("#b362ff" "#b362ff" "brightblack"))
   (selection  '("#b362ff" "#b362ff" "brightblack"))
   (fg         '("#e3e9fa" "#e3e9fa" "brightwhite"))
   (fg-alt     '("#b4c2f0" "#b4c2f0" "white"))

   (grey base5)

   (dark-red      '("#ff5370" "#ff5370" "red"))
   (red           '("#ff000d" "#ff000d" "red"))
   (light-red     '("#ff98a4" "#ff98a4" "brightred"))
   (orange        '("#ff9d00" "#ff9d00" "brightred"))
   (light-green   '("#a5ff90" "#a5ff90" "green"))
   (green         '("#3ad900" "#3ad900" "green"))
   (dark-green    '("#00ff00" "#00ff00" "green"))
   (dark-teal     '("#37fea1" "#37fea1" "green"))
   (teal          '("#ff628c" "#ff628c" "brightgreen"))
   (light-teal    '("#7af8ca" "#7af8ca" "brightgreen"))
   (yellow        '("#fad000" "#fad000" "brightyellow"))
   (blue          '("#82aaff" "#82aaff" "brightblue"))
   (dark-blue     '("#4976eb" "#4976eb" "brightblue"))
   (light-blue    '("#50c4fa" "#50c4fa" "blue"))
   (light-magenta '("#baacff" "#baacff" "brightmagenta"))
   (magenta       '("#ff9d00" "#ff9d00" "brightmagenta"))
   (violet        '("#f989d3" "#f989d3" "magenta"))
   (light-pink    '("#fb94ff" "#fb94ff" "magenta"))
   (pink          '("#ff628c" "#ff628c" "magenta"))
   (cyan          '("#ff628c" "#ff628c" "brightcyan"))
   (dark-cyan     '("#9effff" "#9effff" "cyan"))
   (purple        '("#b362ff" "#b362ff" "magenta"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   bg-alt)
   (line-highlight bg-alt)
   (selection      selection)
   (builtin        magenta)
   (comments       purple)
   (doc-comments   (doom-lighten comments 0.25))
   (constants      light-pink)
   (functions      (doom-lighten yellow 0.15))
   (keywords       orange)
   (methods        yellow)
   (operators      orange)
   (type           green)
   (strings        light-green)
   (variables      dark-teal)
   (numbers        orange)
   (region         region)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       dark-green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (doom-darken bg-alt 0.1))
   (modeline-bg-alt (doom-darken bg 0.1))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-shades-of-purple-padded-modeline
      (if (integerp doom-shades-of-purple-padded-modeline) doom-shades-of-purple-padded-modeline 4))))

  ;;;; Base theme face overrides
  (;;;; emacs
   (font-lock-keyword-face :foreground keywords)
   (font-lock-comment-face :foreground comments)
   (font-lock-doc-face :foreground doc-comments)
   (fringe :background bg)
   (lazy-highlight :background purple :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (tooltip :background base0 :foreground fg)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground fg :background line-highlight)

   ;;;; linum
   ((linum &inherit line-number))
   ;;;; doom-modeline
   (doom-modeline-buffer-file       :foreground base7)
   (doom-modeline-icon-inactive     :foreground indigo)
   (doom-modeline-evil-normal-state :foreground dark-cyan)
   (doom-modeline-evil-insert-state :foreground yellow)
   (doom-modeline-project-dir       :foreground light-teal)
   (doom-modeline-buffer-path       :foreground blue)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; hl-line <built-in>
   (hl-line :background line-highlight)
   ;;;; message <built-in>
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
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; nix-mode
   (nix-attribute-face :foreground blue)
   (nix-builtin-face :foreground dark-teal)
   ;;;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)
   ;;;; lsp-mode
   (lsp-face-highlight-read :background region)
   (lsp-face-highlight-textual :background region)
   (lsp-face-highlight-write :background region)
   (lsp-face-semhl-type-primative :foreground orange)
   (lsp-face-semhl-method :foreground magenta)
   ;;;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground dark-teal)
   (js2-object-property-access :foreground fg-alt)
   (js2-function-param         :foreground pink)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground light-blue)
   ((outline-2 &override) :foreground dark-cyan)
   ((outline-3 &override) :foreground light-red)
   ((outline-4 &override) :foreground blue)
   ((outline-5 &override) :foreground magenta)
   ((outline-6 &override) :foreground red)
   ((outline-7 &override) :foreground violet)
   ;;;; org-mode <built-in>
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   ;;;; rjsx-mode
   (rjsx-tag :foreground violet)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)
   ;;;; all-the-icons
   (all-the-icons-cyan       :foreground dark-cyan)
   (all-the-icons-cyan-alt   :foreground dark-cyan)
   (all-the-icons-dblue      :foreground (doom-darken blue 0.1))
   (all-the-icons-dcyan      :foreground dark-cyan)
   (all-the-icons-dgreen     :foreground dark-teal)
   (all-the-icons-dmaroon    :foreground magenta)
   (all-the-icons-dorange    :foreground orange)
   (all-the-icons-dpink      :foreground pink)
   (all-the-icons-dpurple    :foreground magenta)
   (all-the-icons-dred       :foreground dark-red)
   (all-the-icons-dsilver    :foreground grey)
   (all-the-icons-dyellow    :foreground orange)
   (all-the-icons-green      :foreground teal)
   (all-the-icons-lcyan      :foreground (doom-lighten dark-cyan 0.3))
   (all-the-icons-lgreen     :foreground green)
   (all-the-icons-lmaroon    :foreground light-magenta)
   (all-the-icons-lorange    :foreground orange)
   (all-the-icons-lpink      :foreground light-pink)
   (all-the-icons-lpurple    :foreground light-magenta)
   (all-the-icons-lred       :foreground light-red)
   (all-the-icons-lsilver    :foreground (doom-lighten grey 0.4))
   (all-the-icons-lyellow    :foreground (doom-lighten yellow 0.3))
   (all-the-icons-pink       :foreground pink)
   (all-the-icons-purple     :foreground magenta)
   (all-the-icons-purple-alt :foreground magenta)
   (all-the-icons-red-alt    :foreground red)
   (all-the-icons-silver     :foreground (doom-lighten grey 0.2))
   ;;;; all-the-icons-dired
   (all-the-icons-dired-dir-face :foreground indigo)
   ;;;; company
   (company-tooltip :inherit 'tooltip)
   (company-tooltip-common :foreground highlight)
   ;;;; company-box
   (company-box-annotation :foreground base7)
   ;;;; doom-emacs
   (doom-dashboard-menu-desc :foreground dark-cyan)
   (doom-dashboard-menu-tile :foreground dark-teal)
   ;;;; diredfl
   (diredfl-date-time    :foreground blue)
   (diredfl-file-name    :foreground base7)
   (diredfl-file-suffix  :foreground base6)
   (diredfl-symlink      :foreground dark-cyan)
   ;;;; dired+
   (diredp-number :foreground orange)
   ;;;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored  :foreground cyan)
   (dired-k-added    :foreground vc-added)
   ;;;; magit
   (magit-filename :foreground teal)
   ;;;; markdown-mode
   (markdown-header-face           :inherit 'bold :foreground yellow)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground magenta :inherit 'italic)
   (markdown-list-face             :foreground red)
   (markdown-url-face              :inherit 'underline :foreground orange)
   (markdown-gfm-checkbox-face     :foreground blue)
   (markdown-blockquote-face       :inherit 'italic :foreground fg)
   ;;;; nav-flash
   (nav-flash-face :background region)
   ;;;; ivy-posframe
   (ivy-posframe :background base0)
   (ivy-posframe-border :background base0)
   ;;;; popup
   (popup-face :inherit 'tooltip)
   (popup-selection-face :inherit 'tooltip)
   ;;;; pos-tip
   (popup          :inherit 'tooltip)
   (popup-tip-face :inherit 'tooltip)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground light-red)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)
   ;;;; treemacs
   (treemacs-directory-face :foreground highlight)
   (treemacs-git-modified-face :foreground highlight)
   ;;;; workspaces
   (+workspace-tab-selected-face :background region :foreground blue)
   ;;;; which-key
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground magenta)
   (which-key-local-map-description-face :foreground cyan)))

;;; doom-shades-of-purple-theme.el ends here
