;;; doom-feather-light-theme.el --- a light variable of feather-dark, inspired by doom-one -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Date: November 12, 2022
;; Author: Lena SAVY-LARIGALDIE <https://gitlab.com/Plunne>
;; Maintainer: Lena SAVY-LARIGALDIE <https://gitlab.com/Plunne>
;; Source: https://github.com/Plunne/doom-feather-theme
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-feather-light-theme nil
  "Options for the `doom-feather-light' theme."
  :group 'doom-themes)

(defcustom doom-feather-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-feather-light-theme
  :type 'boolean)

(defcustom doom-feather-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-feather-light-theme
  :type 'boolean)

(defcustom doom-feather-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-feather-light-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-feather-light
  "A light theme based on Doom One Light."

  ;; name        default   256       16
  ((bg         '("#F0EDF4" "#ffffff" "white"        ))
   (fg         '("#4A3B5E" "#424242" "black"        ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#E8E4EE" "#e4e4e4" "white"        ))
   (fg-alt     '("#9783B1" "#949494" "brightblack"  ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#E8E4EE" "#eeeeee" "white"        ))
   (base1      '("#E1DBE9" "#e4e4e4" "brightblack"  ))
   (base2      '("#D9D2E3" "#dadada" "brightblack"  ))
   (base3      '("#C3B7D2" "#c6c6c6" "brightblack"  ))
   (base4      '("#B4A5C7" "#b2b2b2" "brightblack"  ))
   (base5      '("#9783B1" "#949494" "brightblack"  ))
   (base6      '("#4A3B5E" "#424242" "brightblack"  ))
   (base7      '("#2A2236" "#262626" "brightblack"  ))
   (base8      '("#15111B" "#121212" "black"        ))

   (grey       base4)
   (red        '("#dc322f" "#dc322f" "red"          ))
   (orange     '("#d75f00" "#d75f00" "brightred"    ))
   (green      '("#5f8700" "#578700" "green"        ))
   (teal       '("#008070" "#008787" "brightgreen"  ))
   (yellow     '("#a07000" "#875f00" "yellow"       ))
   (blue       '("#007daf" "#0087d7" "brightblue"   ))
   (dark-blue  '("#2257A0" "#005faf" "blue"         ))
   (magenta    '("#f4649b" "#ff5f87" "magenta"      ))
   (violet     '("#875faf" "#875faf" "brightmagenta"))
   (cyan       '("#008ea1" "#0087af" "brightcyan"   ))
   (dark-cyan  '("#204052" "#005f5f" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      base5)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-feather-light-brighter-comments cyan base4))
   (doc-comments   (doom-darken comments 0.15))
   (constants      teal)
   (functions      blue)
   (keywords       violet)
   (methods        cyan)
   (operators      magenta)
   (type           magenta)
   (strings        green)
   (variables      (doom-darken violet 0.4))
   (numbers        teal)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          (doom-blend
                              violet base4
                              (if doom-feather-light-brighter-modeline 0.5 0.2)))
   (modeline-bg              (if doom-feather-light-brighter-modeline
                                 (doom-darken base2 0.05)
                               base1))
   (modeline-bg-alt          (if doom-feather-light-brighter-modeline
                                 (doom-darken base2 0.1)
                               base2))
   (modeline-bg-inactive     (doom-darken bg 0.1))
   (modeline-bg-alt-inactive `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1)))

   (-modeline-pad
    (when doom-feather-light-padded-modeline
      (if (integerp doom-feather-light-padded-modeline) doom-feather-light-padded-modeline 4))))

  ;;;; Base theme face overrides
   (((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :background bg :foreground base5)
   ((font-lock-comment-face &override) :background (if doom-feather-light-brighter-comments base0) :italic t)
   ((font-lock-doc-face &override) :slant 'italic)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if doom-feather-light-brighter-modeline base8 highlight))
   (shadow :foreground base4)
   (tooltip :background base1 :foreground fg)
   ;;;; button (#include "strings")
   (button :foreground strings)
   ;;;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; dashboard
   (dashboard-navigator :foreground magenta)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-feather-light-brighter-modeline modeline-bg highlight))
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))
   ;;;; helm
   (helm-candidate-number :background blue :foreground bg)
   ;;;; ivy
   (ivy-current-match :background base2 :distant-foreground fg :weight 'normal)
   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   ;;;; magit
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)
   ;;;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)
   ;;;; Outlines
   (outline-1 :height 1.8 :foreground magenta :weight 'bold)
   (outline-2 :height 1.2 :foreground violet :weight 'bold)
   (outline-3 :height 1.1 :foreground teal :weight 'bold)
   (outline-4 :height 1.0 :foreground (doom-darken violet 0.25) :weight 'bold)
   (outline-5 :height 1.0 :foreground (doom-darken teal 0.25) :weight 'bold)
   (outline-6 :height 1.0 :foreground (doom-darken violet 0.5) :weight 'bold)
   (outline-7 :height 1.0 :foreground (doom-darken teal 0.5) :weight 'bold)
   (outline-8 :height 1.0 :foreground (doom-darken violet 0.8) :weight 'bold)
   ;;;; org <built-in>
   ((org-block &override) :background base1)
   ((org-block-begin-line &override) :foreground comments)
   (org-ellipsis :underline nil :background bg     :foreground red)
   ((org-quote &override) :background base1)
   ;;;; posframe
   (ivy-posframe               :background base0)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground blue)
   (rainbow-delimiters-depth-2-face :foreground green)
   (rainbow-delimiters-depth-3-face :foreground magenta)
   (rainbow-delimiters-depth-4-face :foreground blue)
   (rainbow-delimiters-depth-5-face :foreground green)
   (rainbow-delimiters-depth-6-face :foreground orange)
   (rainbow-delimiters-depth-7-face :foreground red)
   (rainbow-delimiters-depth-8-face :foreground yellow)
   (rainbow-delimiters-depth-9-face :foreground violet)
   ;;;; selectrum
   (selectrum-current-candidate :background base2)
   ;;;; Treemacs
   (treemacs-root-face :foreground teal :weight 'bold :height 1.4)
   (doom-themes-treemacs-root-face :foreground teal :weight 'ultra-bold :height 1.2)
   ;;;; Tree-sitter
   (tree-sitter-hl-face:punctuation.bracket :foreground comments)
   (tree-sitter-hl-face:attribute :foreground violet)
   (tree-sitter-hl-face:function\.call :foreground blue)
   (tree-sitter-hl-face:function\.macro :foreground blue)
   (tree-sitter-hl-face:type\.builtin :foreground magenta :italic t)
   (tree-sitter-hl-face:variable\.special :foreground constants)
   (tree-sitter-hl-face:operator :foreground operators)
   ;;;; vertico
   (vertico-current :background base2)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-alt-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt-inactive)))
   ;;;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; whitespace
   ((whitespace-tab &override)         :background (unless (default-value 'indent-tabs-mode) base0))
   ((whitespace-indentation &override) :background (if (default-value 'indent-tabs-mode) base0)))

  ;;;; Base theme variable overrides-
  ()
  )

;;; doom-feather-light-theme.el ends here
