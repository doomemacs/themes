;;; doom-windy-light-theme.el --- based on Tailwind colors -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Tasmo <https://github.com/tasmo>
;; Maintainer: Henrik Lissner <https://github.com/hlissner>
;; Source: https://github.com/tailwindlabs/tailwindcss
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-windy-light-theme nil
    "Options for the `doom-windy-light' theme."
    :group 'doom-themes)

(defcustom doom-windy-light-brighter nil
    "If non-nil, more vivid colors will be used."
    :group 'doom-windy-light-theme
    :type 'boolean)

(defcustom doom-windy-light-padded-modeline doom-themes-padded-modeline
    "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
    :group 'doom-windy-light-theme
    :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-windy-light
    "A light theme inspired by Tailwind."
    :family 'doom-windy
    :background-mode 'light

    (
        ;; name       default   256       16
        ;; Define theme colors
        (slate-800   '("#1d293d" "#1d293d" "black"))
        (slate-950   '("#020618" "#020618" "black"))
        (zinc-100    '("#f4f4f5" "#f4f4f5" "white"))
        (zinc-200    '("#e4e4e7" "#e4e4e7" "white"))
        (neutral-50  '("#fafafa" "#fafafa" "white"))
        (neutral-100 '("#f5f5f5" "#f5f5f5" "brightwhite"))
        (neutral-200 '("#e5e5e5" "#e5e5e5" "brightwhite"))
        (neutral-300 '("#d4d4d4" "#d4d4d4" "brightwhite"))
        (neutral-400 '("#a1a1a1" "#a1a1a1" "brightwhite"))
        (neutral-500 '("#737373" "#737373" "brightblack"))
        (neutral-600 '("#525252" "#525252" "brightblack"))
        (neutral-800 '("#262626" "#262626" "brightblack"))
        (neutral-900 '("#171717" "#171717" "brightblack"))
        (neutral-950 '("#0a0a0a" "#0a0a0a" "black"))

        ;; Set default colors
        (bg     zinc-100)
        (fg     slate-950)
        (bg-alt zinc-200)
        (fg-alt slate-800)

        ;; These should represent a spectrum from bg to fg, where base0 is a starker
        ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
        ;; dark grey, base0 should be white and base8 should be black.
        (base0  neutral-50)
        (base1  neutral-100)
        (base2  neutral-200)
        (base3  neutral-300)
        (base4  neutral-400)
        (base5  neutral-500)
        (base6  neutral-600)
        (base7  neutral-800)
        (base8  neutral-950)

        (bred        '("#fb2c36" "#ff0000" "brightred"))
        (red         '("#c10007" "#cd0000" "red"))
        (dred        '("#82181a" "#8b0000" "red"))
        (orange      '("#f54900" "#ff4500" "brightred"))
        (amber       '("#e17100" "#ff8c00" "yellow"))
        (damber      '("#973c00" "#8b5a00" "yellow"))
        (byellow     '("#fff085" "#fff68f" "brightyellow"))
        (yellow      '("#f0b100" "#eead0e" "brightyellow"))
        (lime        '("#5ea500" "#9acd32" "green"))
        (bgreen      '("#00c950" "#32cd32" "brightgreen"))
        (green       '("#008236" "#228b22" "green"))
        (dgreen      '("#0d542b" "#006400" "green"))
        (emerald     '("#007a55" "#2a8b57" "green"))
        (teal        '("#00786f" "#28b2aa" "cyan"))
        (bcyan       '("#00b8db" "#00ced1" "brightcyan"))
        (cyan        '("#007595" "#008b8b" "cyan"))
        (dcyan       '("#104e64" "#2f4f4f" "black"))
        (bsky        '("#74d4ff" "#00bfff" "brightblue"))
        (sky         '("#0069a8" "#009acd" "blue"))
        (dsky        '("#024a70" "#00688b" "blue"))
        (bblue       '("#2b7fff" "#1e98ff" "brightblue"))
        (blue        '("#1447e6" "#4169e1" "blue"))
        (dblue       '("#1c398e" "#27408b" "blue"))
        (indigo      '("#432dd7" "#6a5acd" "blue"))
        (violet      '("#5d0ec0" "#551a8b" "brightmagenta"))
        (purple      '("#8200db" "#8b008b" "magenta"))
        (fuchsia     '("#8a0194" "#8b0a50" "magenta"))
        (pink        '("#a3004c" "#af005f" "brightred"))
        (rose        '("#c70036" "#b22222" "brightred"))
        (stone       '("#79716b" "#8b7765" "brightblack"))
        ;; Color aliases
        (white       base0)
        (grey        base4)
        (black       base8)
        (dark-cyan   dcyan)
        (dark-blue   dblue)
        (light-blue  bsky)
        (magenta     pink)

        ;; These are the "universal syntax classes" that doom-themes establishes.
        ;; These *must* be included in every doom themes, or your theme will throw an
        ;; error, as they are used in the base theme defined in doom-themes-base.
        (highlight      indigo)
        (vertical-bar   (doom-darken bg-alt 0.1))
        (selection      base4)
        (builtin        sky)
        (comments       (if doom-windy-light-brighter (doom-blend purple base5 0.8) base5))
        (doc-comments   (doom-blend comments fg 0.5))
        (constants      pink)
        (functions      teal)
        (keywords       blue)
        (methods        cyan)
        (operators      fuchsia)
        (type           green)
        (strings        (doom-blend sky fg 0.5))
        (variables      emerald)
        (numbers        orange)
        (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
        (error          rose)
        (warning        amber)
        (success        green)
        (vc-modified    blue)
        (vc-added       green)
        (vc-deleted     red)

        ;; Extra color variables used only in this theme
        (modeline-fg-alt          (doom-blend
                                      fg bg-alt
                                      (if doom-windy-light-brighter 0.6 0.5)))
        (modeline-bg              (if doom-windy-light-brighter
                                      bg-alt
                                      (doom-darken bg 0.05)))
        (modeline-bg-alt          (doom-lighten modeline-bg 0.03))
        (modeline-bg-inactive     (doom-lighten modeline-bg 0.04))

        (-modeline-pad
            (when doom-windy-light-padded-modeline
                (if (integerp doom-windy-light-padded-modeline) doom-windy-light-padded-modeline 4))))

    ;;;; Base theme face overrides
    (
        ;;;; Emacs Lisp faces
        ((cursor &override) :background green)
        (help-key-binding
            :inherit 'fixed-pitch
            :background base0
            :foreground indigo
            :box `(:line-width -1 :color ,base3))
        (highlight
            :forground 'unspecified
            :background (doom-blend bg light-blue 0.4)
            :distant-foreground fg-alt)
        (lazy-highlight :inherit 'match)
        ((line-number &override) :foreground base4)
        ((line-number-current-line &override) :foreground base7)
        ((link &override) :foreground 'unspecified :weight 'unspecified)
        (link-visited :inherit 'link :foreground violet)
        (match
            :background (doom-blend bg bgreen 0.8)
            :foreground fg
            :weight 'bold)
        (minibuffer-prompt :foreground fg)
        (mode-line
            :background modeline-bg
            :foreground fg
            :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
        (mode-line-active :inherit 'mode-line)
        (mode-line-inactive
            :background modeline-bg-inactive
            :foreground base5
            :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive))
            :underline nil)
        (mode-line-emphasis
            :foreground (if doom-windy-light-brighter base8 builtin))
        (mode-line-buffer-id :foreground fg :weight 'normal)
        (mode-line-highlight
            :background modeline-bg
            :foreground fg
            :box nil
            :underline nil
            :weight 'bold)
        (shadow :foreground base4)
        (tab-line :inherit 'mode-line)
        (tool-bar :inherit 'mode-line)

        ;;;; font-lock-* faces
        ((font-lock-doc-face &override) :slant 'italic)
        (font-lock-doc-markup-face :foreground violet)
        (font-lock-preprocessor-face :foreground operators)
        ((font-lock-preprocessor-char-face &inherit font-lock-processor-face) :inherit 'bold)

        ;;;; Package faces
        ;;;; all-the-icons
        (all-the-icons-blue       :foreground blue)
        (all-the-icons-blue-alt   :foreground sky)
        (all-the-icons-cyan       :foreground cyan)
        (all-the-icons-cyan-alt   :foreground teal)
        (all-the-icons-dblue      :foreground dblue)
        (all-the-icons-dcyan      :foreground dcyan)
        (all-the-icons-dgreen     :foreground dgreen)
        (all-the-icons-dmaroon    :foreground (doom-darken stone 0.3))
        (all-the-icons-dorange    :foreground (doom-darken orange 0.3))
        (all-the-icons-dpink      :foreground (doom-darken pink 0.3))
        (all-the-icons-dpurple    :foreground (doom-darken purple 0.3))
        (all-the-icons-dred       :foreground dred)
        (all-the-icons-dsilver    :foreground (doom-lighten grey 0.1))
        (all-the-icons-dyellow    :foreground damber)
        (all-the-icons-green      :foreground green)
        (all-the-icons-lblue      :foreground bblue)
        (all-the-icons-lcyan      :foreground bcyan)
        (all-the-icons-lgreen     :foreground bgreen)
        (all-the-icons-lmaroon    :foreground (doom-lighten stone 0.3))
        (all-the-icons-lorange    :foreground (doom-lighten orange 0.3))
        (all-the-icons-lpink      :foreground (doom-lighten pink 0.3))
        (all-the-icons-lpurple    :foreground (doom-lighten purple 0.3))
        (all-the-icons-lred       :foreground bred)
        (all-the-icons-lsilver    :foreground (doom-lighten grey 0.7))
        (all-the-icons-lyellow    :foreground yellow)
        (all-the-icons-maroon     :foreground stone)
        (all-the-icons-orange     :foreground orange)
        (all-the-icons-pink       :foreground pink)
        (all-the-icons-purple     :foreground purple)
        (all-the-icons-purple-alt :foreground violet)
        (all-the-icons-red        :foreground red)
        (all-the-icons-red-alt    :foreground rose)
        (all-the-icons-silver     :foreground (doom-lighten grey 0.45))
        (all-the-icons-yellow     :foreground amber)
        ;;;; ansi-color
        (ansi-color-black          :foreground base8   :background base8)
        (ansi-color-red            :foreground red     :background red)
        (ansi-color-green          :foreground green   :background green)
        (ansi-color-yellow         :foreground yellow  :background yellow)
        (ansi-color-blue           :foreground blue    :background blue)
        (ansi-color-magenta        :foreground magenta :background magenta)
        (ansi-color-cyan           :foreground cyan    :background cyan)
        (ansi-color-white          :foreground base0   :background base0)
        (ansi-color-bright-black   :foreground base6   :background base6)
        (ansi-color-bright-red     :foreground bred    :background bred)
        (ansi-color-bright-green   :foreground bgreen  :background bgreen)
        (ansi-color-bright-yellow
            :foreground (doom-lighten yellow 0.15)
            :background (doom-lighten yellow 0.15))
        (ansi-color-bright-blue    :foreground bblue   :background bblue)
        (ansi-color-bright-magenta
            :foreground (doom-lighten magenta 0.15)
            :background (doom-lighten magenta 0.15))
        (ansi-color-bright-cyan    :foreground bcyan   :background bcyan)
        (ansi-color-bright-white   :foreground base3   :background base3)
        ;;;; anzu
        (anzu-match-1            :background bsky)
        (anzu-match-2            :background bgreen)
        (anzu-match-1            :background byellow)
        (anzu-mode-line          :foreground pink :weight bold)
        (anzu-mode-line-no-match :foreground fuchsia)
        ;;;; avy
        (avy-goto-char-timer-face
            :background (doom-lighten light-blue 0.5))
        (avy-lead-face :background light-blue :foreground fg-alt)
        (avy-lead-face-0
            (&all   :inherit 'avy-lead-face)
            (&light
                :background (doom-darken light-blue 0.25)
                :distant-foreground bg))
        (avy-lead-face-1
            (&all   :inherit 'avy-lead-face)
            (&light
                :background (doom-darken light-blue 0.5)
                :foreground bg))
        (avy-lead-face-2
            (&all   :inherit 'avy-lead-face)
            (&light
                :background (doom-darken light-blue 0.75)
                :foreground bg))
        ;;;; blink
        (blink-matching-paren-offscreen :foreground orange)
        ;;;; calendar
        (holiday :background (doom-blend bg fuchsia 0.7))
        ;;;; completions
        (completions-group-title :foreground fg)
        ;;;; css-mode / scss-mode
        (css-proprietary-property :foreground red)
        (css-property             :foreground green)
        (css-selector             :foreground blue)
        ;;;; diff-mode
        (diff-error :background error :foreground white)
        (diff-indicator-added   :foreground vc-added)
        (diff-added
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-blend vc-added bg 0.1))
        (diff-refine-added
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-lighten vc-added 0.7))
        (diff-indicator-changed :foreground vc-modified)
        (diff-changed
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-blend vc-modified bg 0.1))
        (diff-refine-changed
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-lighten vc-modified 0.7))
        (diff-indicator-removed :foreground vc-deleted)
        (diff-removed
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-blend vc-deleted bg 0.1))
        (diff-refine-removed
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-lighten vc-deleted 0.7))
        ;;;; dired
        (dired-broken-symlink :inherit 'error :background byellow)
        ;;; doom-modeline
        (doom-modeline-buffer-modified
            :inherit '(doom-modeline font-lock-constant-face)
            :weight 'bold)
        (doom-modeline-evil-emacs-state    :inherit 'doom-modeline :foreground builtin)
        (doom-modeline-evil-insert-state   :inherit 'doom-modeline :foreground lime)
        (doom-modeline-evil-motion-state   :inherit 'doom-modeline :foreground constants)
        (doom-modeline-evil-normal-state   :inherit 'doom-modeline :foreground green)
        (doom-modeline-evil-operator-state :inherit 'doom-modeline :foreground functions)
        (doom-modeline-evil-replace-state  :inherit 'doom-modeline :foreground warning)
        (doom-modeline-evil-user-state     :inherit 'doom-modeline :foreground numbers)
        (doom-modeline-evil-visual-state   :inherit 'doom-modeline :foreground operators)
        (doom-modeline-fly-insert-state    :inherit 'doom-modeline-evil-insert-state)
        (doom-modeline-fly-normal-state    :inherit 'doom-modeline-evil-normal-state)
        ;;;; doom-themes
        (doom-themes-visual-bell :background warning)
        ;;;; ediff
        (ediff-current-diff-A
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-blend purple bg 0.1))
        (ediff-fine-diff-A
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-lighten purple 0.7))
        (ediff-current-diff-B
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-blend cyan bg 0.1))
        (ediff-fine-diff-B
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-lighten cyan 0.7))
        (ediff-current-diff-C
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-blend orange bg 0.1))
        (ediff-fine-diff-C
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-lighten orange 0.7))
        (ediff-current-diff-Ancestor
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-blend vc-modified bg 0.1))
        (ediff-fine-diff-Ancestor
            :foreground 'unspecified
            :distant-foreground fg
            :background (doom-lighten vc-modified 0.7))
        ;;;; elisp
        (elisp-shorthand-font-lock-face :foreground sky)
        ;;;; eshell
        (eshell-prompt :foreground sky :bold bold)
        ;;;; evil-ex
        (evil-ex-highlight :inherit 'highlight)
        (evil-ex-search    :inherit 'isearch)
        ;;;; evil-snipe
        (evil-snipe-first-match-face :inherit 'highlight)
        (evil-snipe-matches-face     :inherit 'region)
        ;;;; gnus
        (gnus-cite-1  :foreground (doom-blend fg-alt blue 0.5))
        (gnus-cite-2  :foreground (doom-blend fg-alt green 0.5))
        (gnus-cite-3  :foreground (doom-blend fg-alt magenta 0.5))
        (gnus-cite-4  :foreground (doom-blend fg-alt teal 0.5))
        (gnus-cite-5  :foreground (doom-blend fg-alt orange 0.5))
        (gnus-cite-6  :foreground (doom-blend fg-alt purple 0.5))
        (gnus-cite-7  :foreground (doom-blend fg-alt cyan 0.5))
        (gnus-cite-8  :foreground (doom-blend fg-alt fuchsia 0.5))
        (gnus-cite-9  :foreground (doom-blend fg-alt indigo 0.5))
        (gnus-cite-10 :foreground (doom-blend fg-alt yellow 0.5))
        (gnus-cite-11 :foreground (doom-blend fg-alt violet 0.5))
        (gnus-group-mail-1
            :inherit 'gnus-group-mail-1-empty
            :weight 'bold)
        (gnus-group-mail-1-empty :foreground indigo)
        (gnus-group-mail-2
            :inherit 'gnus-group-mail-2-empty
            :weight 'bold)
        (gnus-group-mail-2-empty :foreground violet)
        (gnus-group-mail-3
            :inherit 'gnus-group-mail-3-empty
            :weight 'bold)
        (gnus-group-mail-3-empty :foreground purple)
        (gnus-group-mail-low
            :inherit 'gnus-group-mail-low-empty
            :weight 'bold)
        (gnus-group-mail-low-empty :foreground pink)
        (gnus-group-news-1
            :inherit 'gnus-group-news-1-empty
            :weight 'bold)
        (gnus-group-news-1-empty :foreground green)
        (gnus-group-news-2
            :inherit 'gnus-group-news-2-empty
            :weight 'bold)
        (gnus-group-news-2-empty :foreground emerald)
        (gnus-group-news-3
            :inherit 'gnus-group-news-3-empty
            :weight 'bold)
        (gnus-group-news-3-empty :foreground teal)
        (gnus-group-news-4
            :inherit 'gnus-group-news-4-empty
            :weight 'bold)
        (gnus-group-news-4-empty :foreground cyan)
        (gnus-group-news-5
            :inherit 'gnus-group-news-5-empty
            :weight 'bold)
        (gnus-group-news-5-empty :foreground sky)
        (gnus-group-news-6
            :inherit 'gnus-group-news-6-empty
            :weight 'bold)
        (gnus-group-news-6-empty :foreground blue)
        (gnus-group-news-low
            :inherit 'gnus-group-news-low-empty
            :weight 'bold)
        (gnus-group-news-low-empty :foreground lime)
        (gnus-emphasis-highlight-words :inherit 'lazy-highlight)
        ((gnus-header-subject &inherit message-header-subject))
        (gnus-signature :inherit 'font-lock-comment-face)
        ;;;; helm
        (helm-candidate-number :background blue :foreground bg)
        ;;;; highlight-numbers-mode
        (highlight-numbers-number :foreground numbers)
        ;;;; hl-line
        (hl-line :background base0 :distant-background bg-alt :extend t)
        ;;;; hl-todo
        (hl-todo :foreground purple :weight 'bold)
        ;;;; hyde
        (hyde-comitted-face :foreground sky)
        (hyde-header-face   :background modeline-bg :foreground sky)
        (hyde-modified-face :foreground vc-modified)
        (hyde-pushed-face   :foreground vc-added)
        (hyde-unsaved-face  :foreground vc-deleted)
        ;;;; icon
        (icon-button :background bg-alt
            :foreground fg
            :box '(:line-width (3 . -1)))
        ;;;; isearch
        (isearch         :inherit 'lazy-highlight)
        (isearch-fail    :background (doom-blend bg error 0.8) :weight 'bold)
        (isearch-group-1 :background (doom-blend bg fuchsia 0.9))
        (isearch-group-2 :background (doom-blend bg violet 0.9))
        ;;;; ivy-posframe
        (ivy-posframe :background base0)
        ;;;; magit
        (magit-blame-heading          :background bg-alt :foreground stone)
        (magit-diff-added             :background (doom-blend   vc-added bg 0.1))
        (magit-diff-added-highlight   :background (doom-lighten vc-added 0.85))
        (magit-diff-base              :background (doom-blend   vc-modified bg 0.1))
        (magit-diff-base-highlight    :background (doom-lighten vc-modified 0.85))
        (magit-diff-context-highlight :background base0 :distant-background bg-alt)
        (magit-diff-hunk-heading
            :foreground (doom-blend bg purple 0.4)
            :background bg-alt
            :extend t)
        (magit-diff-hunk-heading-highlight
            :foreground purple
            :background (doom-blend bg purple 0.8)
            :weight 'bold
            :extend t)
        (magit-diff-removed           :background (doom-blend   vc-deleted bg 0.1))
        (magit-diff-removed-highlight :background (doom-lighten vc-deleted 0.85))
        (magit-diff-file-heading-selection
            :inherit 'magit-diff-file-heading
            :background modeline-bg
            :foreground sky)
        (magit-diff-lines-heading
            :foreground byellow
            :background dred
            :extend t)
        (magit-header-line
            :foreground blue
            :background modeline-bg
            :bold bold)
        (magit-diff-hunk-heading-selection
            :inherit 'magit-diff-hunk-heading-highlight
            :foreground fg
            :bold bold)
        ;;;; markdown-mode
        ((markdown-code-face &override) :background base1)
        (markdown-header-face   :inherit 'bold :foreground dark-blue)
        (markdown-header-face-1 :inherit 'outline-1)
        (markdown-header-face-2 :inherit 'outline-2)
        (markdown-header-face-3 :inherit 'outline-3)
        (markdown-header-face-4 :inherit 'outline-4)
        (markdown-header-face-5 :inherit 'outline-5)
        (markdown-header-face-6 :inherit 'outline-6)
        (markdown-link-face     :foreground strings)
        (markdown-table-face    :inherit 'org-table)
        ((markdown-url-face &override) :foreground purple)
        ;;;; mm-decode
        (mm-command-output :foreground red)
        ;;;; mmm-mode
        (mmm-default-submode-face :background base1)
        ;;;; message
        ((message-header-name &override) :foreground sky)
        (message-header-other :foreground doc-comments)
        ((message-header-subject &override) :foreground 'unspecified)
        ((message-header-to &inherit message-header-subject))
        ;;;; mu4e
        (mu4e-flagged-face :foreground indigo)
        ((mu4e-header-highlight-face &override)
            :background base0
            :foreground 'unspecified
            (:underline (:style wave)))
        (mu4e-header-title-face :foregrmuound lime)
        (mu4e-header-title-value :foreground cyan)
        (mu4e-highlight-face
            :foreground indigo
            :background 'unspecified
            :weight 'bold)
        (mu4e-unread-face :foreground fuchsia :weight 'bold)
        ;;;; nerd-icons
        (nerd-icons-blue       :foreground blue)
        (nerd-icons-blue-alt   :foreground sky)
        (nerd-icons-cyan       :foreground cyan)
        (nerd-icons-cyan-alt   :foreground teal)
        (nerd-icons-dblue      :foreground dark-blue)
        (nerd-icons-dcyan      :foreground dark-cyan)
        (nerd-icons-dgreen     :foreground dgreen)
        (nerd-icons-dmaroon    :foreground (doom-darken stone 0.3))
        (nerd-icons-dorange    :foreground (doom-darken orange 0.3))
        (nerd-icons-dpink      :foreground (doom-darken pink 0.3))
        (nerd-icons-dpurple    :foreground (doom-darken purple 0.3))
        (nerd-icons-dred       :foreground dred)
        (nerd-icons-dsilver    :foreground (doom-lighten grey 0.1))
        (nerd-icons-dyellow    :foreground damber)
        (nerd-icons-green      :foreground green)
        (nerd-icons-lblue      :foreground bblue)
        (nerd-icons-lcyan      :foreground bcyan)
        (nerd-icons-lgreen     :foreground bgreen)
        (nerd-icons-lmaroon    :foreground (doom-lighten stone 0.3))
        (nerd-icons-lorange    :foreground (doom-lighten orange 0.3))
        (nerd-icons-lpink      :foreground (doom-lighten pink 0.3))
        (nerd-icons-lpurple    :foreground (doom-lighten purple 0.3))
        (nerd-icons-lred       :foreground bred)
        (nerd-icons-lsilver    :foreground (doom-lighten grey 0.7))
        (nerd-icons-lyellow    :foreground yellow)
        (nerd-icons-maroon     :foreground stone)
        (nerd-icons-orange     :foreground orange)
        (nerd-icons-pink       :foreground pink)
        (nerd-icons-purple     :foreground purple)
        (nerd-icons-purple-alt :foreground violet)
        (nerd-icons-red        :foreground red)
        (nerd-icons-red-alt    :foreground rose)
        (nerd-icons-silver     :foreground (doom-lighten grey 0.45))
        (nerd-icons-yellow     :foreground amber)
        ;;;; outline
        ;; NOTE org-mode's org-level-N faces inherit these outline-N faces.
        ((outline-1 &override)
            :foreground (doom-blend fg blue    (if doom-windy-light-brighter 0.25 0.33)))
        ((outline-2 &override)
            :foreground (doom-blend fg purple  (if doom-windy-light-brighter 0.25 0.33)))
        ((outline-3 &override)
            :foreground (doom-blend fg cyan    (if doom-windy-light-brighter 0.25 0.33)))
        ((outline-4 &override)
            :foreground (doom-blend fg rose    (if doom-windy-light-brighter 0.25 0.33)))
        ((outline-5 &override)
            :foreground (doom-blend fg indigo  (if doom-windy-light-brighter 0.25 0.33)))
        ((outline-6 &override)
            :foreground (doom-blend fg yellow  (if doom-windy-light-brighter 0.25 0.33)))
        ((outline-7 &override)
            :foreground (doom-blend fg violet  (if doom-windy-light-brighter 0.25 0.33)))
        ((outline-8 &override)
            :foreground (doom-blend fg fuchsia (if doom-windy-light-brighter 0.25 0.33)))
        ;;;; org
        ((org-block &override) :inherit 'fixed-pitch :background (doom-blend bg base0 0.25))
        (org-block-begin-line
            :foreground (doom-blend fg stone 0.3)
            :background (doom-blend bg stone 0.95)
            :slant 'italic
            :extend t)
        (org-code                  :inherit 'fixed-pitch)
        (org-date                  :foreground orange)
        (org-date-selected         :background (doom-blend bg orange 0.5) :foreground fg-alt)
        (org-document-info-keyword :inherit 'fixed-pitch)
        (org-ellipsis
            :underline nil
            :background 'unspecified
            :foreground comments
            :weight 'extra-light)
        ((org-link &override)      :foreground 'unspecified)
        (org-meta-line             :inherit 'fixed-pitch)
        (org-mode-line-clock-overrun
            :inherit 'mode-line
            :background (doom-blend bg-alt red 0.7))
        (org-property-value        :inherit 'fixed-pitch)
        ((org-quote &override)     :background base1)
        (org-special-keyword       :inherit 'fixed-pitch)
        (org-table                 :inherit 'fixed-pitch :foreground fg :background bg-alt)
        (org-tag                   :inherit 'fixed-pitch)
        (org-verbatim              :inherit 'fixed-pitch)
        ;;;; parenface
        (paren-face-match :background base0 :weight 'ultra-bold)
        ((paren-face-mismatch &inherit paren-face-match) :inverse-video t)
        ((paren-face-no-match &inherit paren-face-mismatch))
        ;;;; popup
        (popup-isearch-match              :inherit 'match)
        (popup-menu-mouse-face            :inherit 'highlight)
        (popup-menu-selection-face        :inherit 'region)
        (popup-scroll-bar-background-face :inherit 'hl-line)
        (popup-scroll-bar-foreground-face :inherit 'fringe)
        (popup-summery-face               :inherit 'popup-face :foreground indigo)
        ;;;; pulse
        (pulse-highlight-face :background byellow)
        (pulse-highlight-start-face :inherit 'pulse-highlight-face)
        ;;;; rainbow-delimiters
        (rainbow-delimiters-depth-1-face    :foreground blue)
        (rainbow-delimiters-depth-2-face    :foreground green)
        (rainbow-delimiters-depth-3-face    :foreground magenta)
        (rainbow-delimiters-depth-4-face    :foreground teal)
        (rainbow-delimiters-depth-5-face    :foreground orange)
        (rainbow-delimiters-depth-6-face    :foreground purple)
        (rainbow-delimiters-depth-7-face    :foreground cyan)
        (rainbow-delimiters-depth-8-face    :foreground amber)
        (rainbow-delimiters-depth-9-face    :foreground indigo)
        (rainbow-delimiters-depth-10-face   :foreground emerald)
        (rainbow-delimiters-depth-11-face   :foreground violet)
        (rainbow-delimiters-depth-12-face   :foreground lime)
        (rainbow-delimiters-unmatched-face  :weight 'bold :overline fg :inverse-video t)
        (rainbow-delimiters-mismatched-face :foreground warning :overline t)
        ;;;; shr
        (shr-mark :background byellow)
        (shr-selected-link :inherit 'link :background bsky)
        ;;;; solaire-mode
        (solaire-default-face  :inherit 'default :background (doom-darken bg 0.01))
        (solaire-hl-line-face  :inherit 'hl-line)
        (solaire-org-hide      :inherit 'org-hide)
        (solaire-mode-line-face
            :inherit 'mode-line
            :background modeline-bg-alt
            :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
        (solaire-mode-line-inactive-face
            :inherit 'mode-line-inactive
            :background modeline-bg-inactive
            :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
        ;;;; smartparens
        (sp-wrap-overlay-opening-pair :inherit 'sp-wrap-overlay-face :foreground green)
        ;;;; transient
        (transient-disabled-suffix :background dred   :foreground bg-alt)
        (transient-enabled-suffix  :background bgreen :foreground fg)
        (transient-wnabled-suffix  :background bgreen :foreground fg)
        (transient-higher-level    :box `(:line-width -1 :color ,grey))
        ;;;; tty-menu
        (tty-menu-disabled-face :background indigo :foreground base3)
        (tty-menu-enabled-face  :background blue   :foreground base1)
        (tty-menu-selected-face :background bblue  :foreground base0)
        ;;;; wgrep
        ((wgrep-face &override) :background base1)
        ;;;; whitespace
        (whitespace-big-indent :background dred :foreground bred)
        (whitespace-empty      :background base2)
        (whitespace-hspace     :background bg-alt :foreground white)
        (whitespace-line    :background base0 :foreground red :weight 'bold)
        (whitespace-missing-newline-at-eof
            :background (doom-blend bg stone 0.5)
            :foreground fg)
        (whitespace-newline    :foreground base3)
        (whitespace-space      :foreground base2)
        (whitespace-space-after-tab
            :background (doom-blend bg yellow 0.4)
            :foreground dred)
        (whitespace-space-before-tab
            :background (doom-blend bg orange 0.4)
            :foreground dred)
        (whitespace-tab
            :foreground base4
            :background (if (not (default-value 'indent-tabs-mode)) base0 'unspecified))
        (whitespace-indentation
            :foreground base3
            :background (if (default-value 'indent-tabs-mode) base0 'unspecified))
    )

    ;;;; Base theme variable overrides
    ())

;;; doom-windy-light-theme.el ends here
