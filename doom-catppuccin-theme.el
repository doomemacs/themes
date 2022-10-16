;;; doom-catppuccin-theme.el --- inspired by catppuccin -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 23, 2016 (28620647f838)
;; Author: Afiq Nazrie <https://github.com/mangkoran>
;; Maintainer: Afiq Nazrie <https://github.com/mangkoran>
;; Source: https://github.com/catppuccin/catppuccin
;;
;;; Commentary:
;;
;; A dark theme based on catppuccin colorscheme.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-catppuccin-theme nil
  "Options for the `doom-catppuccin' theme."
  :group 'doom-themes)

(defcustom doom-catppuccin-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-catppuccin-theme
  :type 'boolean)

(defcustom doom-catppuccin-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-catppuccin-theme
  :type 'boolean)

(defcustom doom-catppuccin-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-catppuccin-theme
  :type '(choice integer boolean))

(defcustom doom-catppuccin-dark-variant nil
  "Color palette used for catppuccin theme.
A choice of \"frappe\" or \"mocha\" can be used to change the
background contrast. All other values default to \"macchiato\"."
  :group 'doom-catppuccin-theme
  :type  'string)

;;
;;; Theme definition

(def-doom-theme doom-catppuccin
  "A dark theme based on catppuccin colorscheme."

  ;; name                                                  default    256      16
  ((_crust
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#232634" "black" "black")) ; crust
          ((equal doom-catppuccin-dark-variant "mocha")  '("#11111b" "black" "black"))
          (t                                             '("#181926" "black" "black"))))
   (_mantle
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#292c3c" "#1e1e1e" "brightblack")) ; mantle
          ((equal doom-catppuccin-dark-variant "mocha")  '("#181825" "#1e1e1e" "brightblack"))
          (t                                             '("#1e2030" "#1e1e1e" "brightblack"))))
   (_base
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#303446" "#2e2e2e" "brightblack")) ; base
          ((equal doom-catppuccin-dark-variant "mocha")  '("#1e1e2e" "#2e2e2e" "brightblack"))
          (t                                             '("#24273a" "#2e2e2e" "brightblack"))))
   (_surface0
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#414559" "#262626" "brightblack")) ; surface0
          ((equal doom-catppuccin-dark-variant "mocha")  '("#313244" "#262626" "brightblack"))
          (t                                             '("#363a4f" "#262626" "brightblack"))))
   (_surface1
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#51576d" "#3f3f3f" "brightblack")) ; surface1
          ((equal doom-catppuccin-dark-variant "mocha")  '("#45475a" "#3f3f3f" "brightblack"))
          (t                                             '("#494d64" "#3f3f3f" "brightblack"))))
   (_surface2
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#628880" "#525252" "brightblack")) ; surface2
          ((equal doom-catppuccin-dark-variant "mocha")  '("#585b70" "#525252" "brightblack"))
          (t                                             '("#5b6078" "#525252" "brightblack"))))
   (_overlay0
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#737994" "#6b6b6b" "brightblack")) ; overlay0
          ((equal doom-catppuccin-dark-variant "mocha")  '("#6c7086" "#6b6b6b" "brightblack"))
          (t                                             '("#6e738d" "#6b6b6b" "brightblack"))))
   (_overlay1
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#838ba7" "#979797" "brightblack")) ; overlay1
          ((equal doom-catppuccin-dark-variant "mocha")  '("#7f849c" "#979797" "brightblack"))
          (t                                             '("#8087a2" "#979797" "brightblack"))))
   (_overlay2
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#949cbb" "#dfdfdf" "white")) ; overlay2
          ((equal doom-catppuccin-dark-variant "mocha")  '("#9399b2" "#dfdfdf" "white"))
          (t                                             '("#939ab7" "#dfdfdf" "white"))))
   (_subtext0
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#a5adce" "#dfdfdf" "white")) ; subtext0
          ((equal doom-catppuccin-dark-variant "mocha")  '("#a6adc8" "#dfdfdf" "white"))
          (t                                             '("#a5adcb" "#dfdfdf" "white"))))
   (_subtext1
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#b5bfe2" "#dfdfdf" "white")) ; subtext1
          ((equal doom-catppuccin-dark-variant "mocha")  '("#bac2de" "#dfdfdf" "white"))
          (t                                             '("#b8c0e0" "#dfdfdf" "white"))))
   (_text
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#c6d0f5" "#dfdfdf" "white")) ; text
          ((equal doom-catppuccin-dark-variant "mocha")  '("#cdd6f4" "#dfdfdf" "white"))
          (t                                             '("#cad3f5" "#dfdfdf" "white"))))
   (_rosewater
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#f2d5cf" "#c678dd" "brightmagenta")) ; pink
          ((equal doom-catppuccin-dark-variant "mocha")  '("#f5e0dc" "#c678dd" "brightmagenta"))
          (t                                             '("#f4dbd6" "#c678dd" "brightmagenta"))))
   (_flamingo
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#eebebe" "#c678dd" "brightmagenta"))
          ((equal doom-catppuccin-dark-variant "mocha")  '("#f2cdcd" "#c678dd" "brightmagenta"))
          (t                                             '("#f0c6c6" "#c678dd" "brightmagenta"))))
   (_pink
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#f4b8e4" "#c678dd" "brightmagenta"))
          ((equal doom-catppuccin-dark-variant "mocha")  '("#f5c2e7" "#c678dd" "brightmagenta"))
          (t                                             '("#f5bde6" "#c678dd" "brightmagenta"))))
   (_mauve
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#ca9ee6" "#a9a1e1" "magenta")) ; mauve
          ((equal doom-catppuccin-dark-variant "mocha")  '("#cba6f7" "#a9a1e1" "magenta"))
          (t                                             '("#c6a0f6" "#a9a1e1" "magenta"))))
   (_red
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#e78284" "#ff6655" "red")) ; red
          ((equal doom-catppuccin-dark-variant "mocha")  '("#f38ba8" "#ff6655" "red"))
          (t                                             '("#ed8796" "#ff6655" "red"))))
   (_maroon
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#ea999c" "#ff6655" "red"))
          ((equal doom-catppuccin-dark-variant "mocha")  '("#eba0ac" "#ff6655" "red"))
          (t                                             '("#ee99a0" "#ff6655" "red"))))
   (_peach
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#ef9f76" "#dd8844" "brightred")) ; peach
          ((equal doom-catppuccin-dark-variant "mocha")  '("#fab387" "#dd8844" "brightred"))
          (t                                             '("#f5a97f" "#dd8844" "brightred"))))
   (_yellow
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#e5c890" "#ecbe7b" "yellow")) ; yellow
          ((equal doom-catppuccin-dark-variant "mocha")  '("#f9e2af" "#ecbe7b" "yellow"))
          (t                                             '("#eed49f" "#ecbe7b" "yellow"))))
   (_green
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#a6d189" "#99bb66" "green")) ; green
          ((equal doom-catppuccin-dark-variant "mocha")  '("#a6e3a1" "#99bb66" "green"))
          (t                                             '("#a6da95" "#99bb66" "green"))))
   (_teal
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#81c8be" "#44b9b1" "brightgreen")) ; teal
          ((equal doom-catppuccin-dark-variant "mocha")  '("#94e2d5" "#44b9b1" "brightgreen"))
          (t                                             '("#8bd5ca" "#44b9b1" "brightgreen"))))
   (_sky
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#99d1db" "#46d9ff" "brightcyan")) ; sky
          ((equal doom-catppuccin-dark-variant "mocha")  '("#89dceb" "#46d9ff" "brightcyan"))
          (t                                             '("#91d7e3" "#46d9ff" "brightcyan"))))
   (_sapphire
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#85c1dc" "#5699af" "cyan")) ; sapphire
          ((equal doom-catppuccin-dark-variant "mocha")  '("#74c7ec" "#5699af" "cyan"))
          (t                                             '("#7dc4e4" "#5699af" "cyan"))))
   (_blue
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#8caaee" "#2257a0" "blue")) ; blue
          ((equal doom-catppuccin-dark-variant "mocha")  '("#89b4fa" "#2257a0" "blue"))
          (t                                             '("#8aadf4" "#2257a0" "blue"))))
   (_lavender
    (cond ((equal doom-catppuccin-dark-variant "frappe") '("#babbf1" "#51afef" "brightblue")) ; lavender
          ((equal doom-catppuccin-dark-variant "mocha")  '("#b4befe" "#51afef" "brightblue"))
          (t                                             '("#b7bdf8" "#51afef" "brightblue"))))

   (bg _base)
   (fg _text)

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt _crust)
   (fg-alt _subtext0)

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0       _crust)
   (base1       _mantle)
   (base2       _base)
   (base3       _surface0)
   (base4       _surface1)
   (base5       _surface2)
   (base6       _overlay0)
   (base7       _overlay1)
   (base8       _overlay2)

   (grey       base4)
   (red        _red)      ; red
   (orange     _peach)    ; peach
   (green      _green)    ; green
   (teal       _teal)     ; teal
   (yellow     _yellow)   ; yellow
   (blue       _lavender) ; lavender
   (dark-blue  _blue)     ; blue
   (magenta    _pink)     ; pink
   (violet     _mauve)    ; mauve
   (cyan       _sky)      ; sky
   (dark-cyan  _sapphire) ; sapphire

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      _lavender)
   (vertical-bar   _crust)
   (selection      _blue)
   (builtin        _peach)
   (comments       (if doom-catppuccin-brighter-comments _sapphire base5))
   (doc-comments   (if doom-catppuccin-brighter-comments _sky base6))
   (constants      _peach)
   (functions      _blue)
   (keywords       _mauve)
   (methods        _blue)
   (operators      _sky)
   (type           _yellow)
   (strings        _green)
   (variables      _text)
   (numbers        _peach)
   ;; (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   ;; (region         _surface2)
   (region         `(,(car _surface2) ,@(cdr _overlay0)))
   (error          _red)
   (warning        _yellow)
   (success        _green)
   (vc-modified    _peach)
   (vc-added       _green)
   (vc-deleted     _red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-catppuccin-brighter-modeline
                                 _blue
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-catppuccin-brighter-modeline
                                 _blue
                               `(,(doom-darken (car bg-alt) 0.2) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-catppuccin-padded-modeline
      (if (integerp doom-catppuccin-padded-modeline) doom-catppuccin-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-catppuccin-brighter-comments (doom-lighten bg 0.1)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-catppuccin-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground _peach)
   (css-property             :foreground _green)
   (css-selector             :foreground _lavender)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-catppuccin-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground _green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background _surface0 :foreground _mantle)
   ;;;; ivy
   (ivy-current-match :background _blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground _green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground _red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.1))
   ;;;; rjsx-mode
   (rjsx-tag :foreground _red)
   (rjsx-attr :foreground _peach)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
   (cursor :background _rosewater))

  ;;;; Base theme variable overrides-
  ())

;;; doom-catppuccin-theme.el ends here
