;;; doom-flate-theme.el --- inspired by VSCode's flate theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: November 18, 2022
;; Author: Arsenic <https://github.com/Arsenic-ATG>
;; Author: Arsenic <https://github.com/Arsenic-ATG>
;; Source: https://github.com/hiukky/flate
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-flate-theme nil
  "Options for the `doom-flate` theme."
  :group 'doom-themes)

(defcustom doom-flate-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-flate-theme
  :type 'boolean)

(defcustom doom-flate-comment-bg doom-flate-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their Legibility."
  :group 'doom-flate-theme
  :type 'boolean)

(defcustom doom-flate-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-flate-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-flate

                "dark vibrant theme inspired by VSCode's flate theme"

                ;; Name        gui       256       16
                ((bg         '("#0d1117" nil       nil          ))
                 (bg-alt     '("#040408" "color-232" "brightblack"  ))
                 (base0      '("#0F1019" "color-234" "black"        ))
                 (base1      '("#121212" "color-233" "brightblack"  ))
                 (base2      '("#1E1E33" "color-236" "brightblack"  ))
                 (base3      '("#464A56" "color-240" "brightblack"  ))
                 (base4      '("#585C6C" "color-60"  "brightblack"  ))
                 (base5      '("#767676" "color-243" "brightblack"  ))
                 (base6      '("#959EA5" "color-109" "white"        ))
                 (base7      '("#B2B2B2" "color-249" "white"        ))
                 (base8      '("#D0D0D0" "color-252" "brightwhite"  ))
                 (fg         '("#CEDBE5" "color-152" "brightwhite"  ))
                 (fg-alt     '("#E5F4FF" "color-195" "brightwhite"  ))

                 (green      '("#23d18c" "#A6E22E" "green"      ))
                 (beige      '("#8f8d88"))
                 (purple     '("#a29bfe"))
                 (blue       '("#5677fc" "#268bd2" "brightblue" ))
                 (dark-blue  '("#727280" "#727280" "blue"       ))
                 (red        '("#e84855" "#E74C3C" "red"        ))
                 (yellow     '("#ffe066" "#E6DB74" "yellow"     ))
                 (grey       '("#424b54" "#525254" "brightblack"))
                 (orange     '("#f0aa85" "#FD971F" "brightred"  ))
                 (pink       '("#ff5d8f"))
                 (magenta    '("#F92660" "#F92660" "magenta"))
                 (teal       green)
                 (violet     '("#9C91E4" "#9C91E4" "brightmagenta"))
                 (quince     '("#f49e4c"))
                 (spiced     '("#eab464"))
                 (squash     '("#f38375"))
                 (blush      '("#e9dbdb"))
                 (white      '("#ffffff" "#FFFFFF" "brightwhite"))
                 (black      '("#000000" "black"   "black"      ))
                 (cyan       '("#00cecb" "#66D9EF" "brightcyan" ))
                 (dark-cyan  '("#8FA1B3" "#8FA1B3" "cyan"))

                 ;; face categories
                 (highlight      cyan)
                 (vertical-bar   (doom-lighten bg 0.1))
                 (selection      base5)
                 (builtin        orange)
                 (comments       (if doom-flate-brighter-comments quince base5))
                 (doc-comments   (if doom-flate-brighter-comments (doom-lighten quince 0.1) (doom-lighten base5 0.25)))
                 (constants      quince)
                 (functions      green)
                 (keywords       violet)
                 (methods        green)
                 (operators      red)
                 (type           pink)
                 (strings        yellow)
                 (variables      yellow)
                 (numbers        quince)
                 (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    cyan)
                 (vc-added       (doom-darken green 0.15))
                 (vc-deleted     red)

                 ;; custom categories
                 (hidden     `(,(car bg) "black" "black"))
                 (-modeline-pad
                  (when doom-flate-padded-modeline
                    (if (integerp doom-flate-padded-modeline) doom-flate-padded-modeline 4)))

                 (modeline-fg nil)
                 (modeline-fg-alt base4)

                 (modeline-bg base1)
                 (modeline-bg-inactive (doom-darken base2 0.2))

                 (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))


  ;;;; Base theme face overrides
                ((cursor :background pink)
                 ((font-lock-comment-face &override) :slant 'italic)
                 ((font-lock-type-face &override) :slant 'italic)
                 (lazy-highlight :background quince :foreground base0 :distant-foreground base0 :bold bold)
                 ((line-number &override) :foreground base5 :distant-foreground nil)
                 ((line-number-current-line &override) :foreground cyan :distant-foreground nil)
                 (mode-line
                  :background modeline-bg :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color modeline-bg)))
                 (mode-line-inactive
                  :background modeline-bg-inactive :foreground modeline-fg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color modeline-bg-inactive)))

   ;;;; centaur-tabs
                 (centaur-tabs-selected-modified :inherit 'centaur-tabs-selected
                                                 :background bg
                                                 :foreground yellow)
                 (centaur-tabs-unselected-modified :inherit 'centaur-tabs-unselected
                                                   :background bg-alt
                                                   :foreground yellow)
                 (centaur-tabs-active-bar-face :background yellow)
                 (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected :foreground fg)
                 (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground fg)
   ;;;; css-mode <built-in> / scss-mode
                 (css-proprietary-property :foreground keywords)
   ;;;; doom-modeline
                 (doom-modeline-bar :background yellow)
                 (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
                 (doom-modeline-buffer-path :inherit 'bold :foreground green)
                 (doom-modeline-buffer-project-root :foreground green :weight 'bold)
                 (doom-modeline-buffer-modified :inherit 'bold :foreground orange)


                 (isearch :foreground base0 :background green)
   ;;;; ediff <built-in>
                 (ediff-fine-diff-A :background (doom-blend pink bg 0.3) :weight 'bold)
   ;;;; evil
                 (evil-search-highlight-persist-highlight-face :background quince)
   ;;;; evil-snipe
                 (evil-snipe-first-match-face :foreground base0 :background green)
                 (evil-snipe-matches-face     :foreground green :underline t)
   ;;;; flycheck
                 (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
                 (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
                 (flycheck-info    :underline `(:style wave :color ,green)  :background base3)
   ;;;; helm
                 (helm-swoop-target-line-face :foreground pink :inverse-video t)
   ;;;; ivy
                 (ivy-current-match :background base3)
                 (ivy-minibuffer-match-face-1 :background base1 :foreground base4)
   ;;;; markdown-mode
                 (markdown-blockquote-face :inherit 'italic :foreground dark-blue)
                 (markdown-list-face :foreground pink)
                 (markdown-pre-face  :foreground cyan)
                 (markdown-link-face :inherit 'bold :foreground blue)
                 ((markdown-code-face &override) :background (doom-lighten base2 0.045))
   ;;;; neotree
                 (neo-dir-link-face   :foreground cyan)
                 (neo-expand-btn-face :foreground pink)
   ;;;; outline <built-in>
                 ((outline-1 &override) :foreground pink)
                 ((outline-2 &override) :foreground orange)
   ;;;; org <built-in>
                 (org-ellipsis :foreground orange)
                 (org-tag :foreground yellow :bold nil)
                 ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
                 (org-todo :foreground yellow :bold 'inherit)
                 (org-list-dt :foreground yellow)
   ;;;; rainbow-delimiters
                 (rainbow-delimiters-depth-1-face :foreground yellow)
                 (rainbow-delimiters-depth-2-face :foreground orange)
                 (rainbow-delimiters-depth-3-face :foreground green)
                 (rainbow-delimiters-depth-4-face :foreground cyan)
                 (rainbow-delimiters-depth-5-face :foreground magenta)
                 (rainbow-delimiters-depth-6-face :foreground orange)
                 (rainbow-delimiters-depth-7-face :foreground green))

  ;;;; Base theme variable overrides
                ;; ()
                )

;;; doom-flate-theme.el ends here
