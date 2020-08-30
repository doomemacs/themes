;;; doom-tomorrow-day-theme.el -- port of tomorrow theme -*- no-byte-compile: t; -*-
;;; Commentary:
;; This file is part of emacs-doom-themes, which provides license
;; information.
;;; Code:

(require 'doom-themes)

(defgroup doom-tomorrow-day-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-tomorrow-day-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-tomorrow-day-theme
  :type '(choice integer boolean))

(def-doom-theme doom-tomorrow-day
  "A light theme based off of Chris Kempson's Tomorrow Dark."

  ;; name        gui       256       16
  ((bg         '("#ffffff" "white"   "white" ))
   (bg-alt     '("#eaeaea" nil       nil     ))
   (base0      '("#f2f2f2" "white"   "white" ))
   (base1      '("#e4e4e4" "#e4e4e4"         ))
   (base2      '("#dedede" "#cccccc"         ))
   (base3      '("#d6d4d4" "#cccccc" "silver"))
   (base4      '("#C0bfbf" "#c0c0c0" "silver"))
   (base5      '("#a3a1a1" "#adadad" "silver"))
   (base6      '("#8a8787" "#949494" "silver"))
   (base7      '("#696769" "#6b6b6b" "silver"))
   (base8      '("#000000" "#000000" "black" ))
   (fg         '("#4d4d4c" "#3a3a3a" "black"))
   (fg-alt     (doom-darken fg 0.6))

   (grey       '("#a5a4a5" "#999999" "silver"))
   (red        '("#c82829" "#cc3333" "red"))
   (orange     '("#f5871f" "#ff9933" "brightred"))
   (yellow     '("#eab700" "#ffcc00" "yellow"))
   (green      '("#718c00" "#669900" "green"))
   (blue       '("#3e999f" "#339999" "brightblue"))
   (dark-blue  '("#4271ae" "#336699" "blue"))
   (teal       blue) ; FIXME replace with real teal
   (magenta    '("#c9b4cf" "#c9b4cf" "magenta"))
   (violet     '("#8959a8" "#996699" "brightmagenta"))
   (cyan       '("#8abeb7" "#8abeb7" "cyan"))
   (dark-cyan  (doom-lighten cyan 0.4))

   ;; face categories
   (highlight      dark-blue)
   (vertical-bar   base0)
   (selection      base3)
   (builtin        blue)
   (comments       grey)
   (doc-comments   (doom-darken grey 0.1))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg) 0.1) ,@(cdr base3)))
   (modeline-bg-alt `(,(doom-darken (car bg) 0.14) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when doom-tomorrow-day-padded-modeline
      (if (integerp doom-tomorrow-day-padded-modeline)
          doom-tomorrow-day-padded-modeline
        4))))

  ;; --- faces ------------------------------
  ((doom-modeline-buffer-path       :foreground violet :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground blue :bold bold)

   (ivy-current-match :background region :distant-foreground grey :weight 'ultra-bold)
   (ivy-minibuffer-match-face-1
    :foreground base5
    :weight 'light)
   (ivy-minibuffer-match-face-2 :inherit 'ivy-minibuffer-match-face-1 :foreground violet :weight 'ultra-bold)
   (ivy-minibuffer-match-face-3 :inherit 'ivy-minibuffer-match-face-2 :foreground blue)
   (ivy-minibuffer-match-face-4 :inherit 'ivy-minibuffer-match-face-2 :foreground red)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;; org
   (org-block-begin-line         :foreground base7 :background base3 :extend t)

   ;; treemacs
   (treemacs-git-untracked-face :foreground yellow)
   )

  ;; --- variables --------------------------
  ;; ()
  )

(provide 'doom-tomorrow-day-theme)
;;; doom-tomorrow-day-theme.el ends here
