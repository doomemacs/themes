;;; doom-earl-grey-theme.el --- a gentle color scheme, for code -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-earl-grey-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-earl-grey-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-earl-grey-theme
  :type 'boolean)

(defcustom doom-earl-grey-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-earl-grey-theme
  :type 'boolean)

(defcustom doom-earl-grey-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-earl-grey-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-earl-grey
  "A gentle color scheme for code."

  ;; name        default   256       16
  (
   ;; Earl Grey Colors
   (eg-fg       '("#605A52" "#626262" ""))
   (eg-fg2      '("#4C4741" "#4e4e4e" ""))
   (eg-bg       '("#FCFBF9" "#FFFFFF"   "white"))
   (eg-bg2      '("#F7F3EE" "#FFFFFF"   "white"))

   (eg-purple   '("#83577D" "#875FAF" "magenta"))
   (eg-blue     '("#556995" "#5F87AF" "brightblue"))
   (eg-teal     '("#477A7B" "#87AFAF" "brightgreen"))
   (eg-orange   '("#886A44" "#875F00" "brightred"))
   (eg-green    '("#747B4D" "#5F875F" "green"))
   (eg-red      '("#8F5652" "#870000" "red"))
   (eg-berry    '("#AA5087" "#996699" "brightmagenta"))

   (eg-grey1    '("#ECEBE8" "#E4E4E4" "white"))
   (eg-grey2    '("#DDDBD8" "#DADADA" "brightblack"))
   (eg-grey3    '("#CDCBC7" "#C6C6C6" "brightblack"))
   (eg-grey4    '("#BEBBB6" "#B2B2B2" "brightblack"))
   (eg-grey5    '("#AEABA6" "#A8A8A8" "brightblack"))
   (eg-grey6    '("#9E9A95" "#949494" "brightblack"))
   (eg-grey7    '("#8F8A84" "#8A8A8A" "brightblack"))
   (eg-grey8    '("#7F7A73" "#767676" "brightblack"))
   (eg-grey9    '("#706A63" "#6C6C6C" "brightblack"))

   (eg-purple1  '("#F0EBED" "#D7D7FF" "brightmagenta"))
   (eg-purple2  '("#E4DAE0" "#D7D7FF" "brightmagenta"))
   (eg-purple3  '("#D8CAD4" "#D7D7FF" "brightmagenta"))
   (eg-purple4  '("#CCB9C7" "#D7D7FF" "brightmagenta"))
   (eg-purple5  '("#C0A9BB" "#AF87FF" "brightmagenta"))
   (eg-purple6  '("#B399AF" "#AF87FF" "brightmagenta"))
   (eg-purple7  '("#A788A2" "#AF87FF" "brightmagenta"))
   (eg-purple8  '("#9B7896" "#AF87FF" "magenta"))
   (eg-purple9  '("#8F6789" "#AF87FF" "magenta"))

   (eg-blue1    '("#EBECEF" "#87D7FF" "brightblue"))
   (eg-blue2    '("#DBDEE5" "#87D7FF" "brightblue"))
   (eg-blue3    '("#CACFDB" "#87D7FF" "brightblue"))
   (eg-blue4    '("#B9C1D1" "#87D7FF" "brightblue"))
   (eg-blue5    '("#A9B2C7" "#87D7FF" "brightblue"))
   (eg-blue6    '("#98A3BD" "#87AFFF" "brightblue"))
   (eg-blue7    '("#8795B3" "#87AFFF" "brightblue"))
   (eg-blue8    '("#7686A9" "#87AFFF" "blue"))
   (eg-blue9    '("#66789F" "#87AFFF" "blue"))

   (eg-teal1    '("#EAEEEC" "#5FD7D7" "brightgreen"))
   (eg-teal2    '("#D8E1E0" "#5FD7D7" "brightgreen"))
   (eg-teal3    '("#C6D4D3" "#5FD7D7" "brightgreen"))
   (eg-teal4    '("#B4C7C7" "#5FD7D7" "brightgreen"))
   (eg-teal5    '("#A2BBBA" "#5FD7D7" "brightgreen"))
   (eg-teal6    '("#8FAEAD" "#00AFAF" "brightgreen"))
   (eg-teal7    '("#7DA1A1" "#00AFAF" "brightgreen"))
   (eg-teal8    '("#6B9494" "#00AFAF" "brightgreen"))
   (eg-teal9    '("#598788" "#00AFAF" "brightgreen"))

   (eg-orange1  '("#F0EDE7" "#D7AF5F" "brightred"))
   (eg-orange2  '("#E5DED5" "#D7AF5F" "brightred"))
   (eg-orange3  '("#D9D0C3" "#D7AF5F" "brightred"))
   (eg-orange4  '("#CEC1B1" "#D7AF5F" "brightred"))
   (eg-orange5  '("#C2B39F" "#D7AF5F" "brightred"))
   (eg-orange6  '("#B6A48C" "#D7AF5F" "brightred"))
   (eg-orange7  '("#AB967A" "#D7AF5F" "brightred"))
   (eg-orange8  '("#9F8768" "#D7AF5F" "brightred"))
   (eg-orange9  '("#947956" "#D7AF5F" "brightred"))

   (eg-green1   '("#EEEEE8" "#5FAF5F" "green"))
   (eg-green2   '("#E1E1D7" "#5FAF5F" "green"))
   (eg-green3   '("#D3D5C5" "#5FAF5F" "green"))
   (eg-green4   '("#C6C8B4" "#5FAF5F" "green"))
   (eg-green5   '("#B8BBA3" "#5FAF5F" "green"))
   (eg-green6   '("#AAAE92" "#5F875F" "green"))
   (eg-green7   '("#9DA181" "#5F875F" "green"))
   (eg-green8   '("#8F956F" "#5F875F" "green"))
   (eg-green9   '("#82885E" "#5F875F" "green"))

   (eg-red1     '("#F1EBE8" "#D78787" "brightred"))
   (eg-red2     '("#E6DAD8" "#D78787" "brightred"))
   (eg-red3     '("#DBCAC7" "#D78787" "brightred"))
   (eg-red4     '("#D0B9B6" "#D78787" "brightred"))
   (eg-red5     '("#C6A9A6" "#D78787" "brightred"))
   (eg-red6     '("#BB9895" "#D75F5F" "brightred"))
   (eg-red7     '("#B08884" "#D75F5F" "brightred"))
   (eg-red8     '("#A57773" "#D75F5F" "red"))
   (eg-red9     '("#9A6763" "#D75F5F" "red"))

   (eg-berry1     '("#F4EAEE" "#D787D7" "brightmagenta"))
   (eg-berry2     '("#ECD9E2" "#D787D7" "brightmagenta"))
   (eg-berry3     '("#E3C8D7" "#D787D7" "brightmagenta"))
   (eg-berry4     '("#DBB7CB" "#D787D7" "brightmagenta"))
   (eg-berry5     '("#D3A6C0" "#D787D7" "brightmagenta"))
   (eg-berry6     '("#CB94B5" "#AF00AF" "brightmagenta"))
   (eg-berry7     '("#C383A9" "#AF00AF" "brightmagenta"))
   (eg-berry8     '("#BA729E" "#AF00AF" "brightmagenta"))
   (eg-berry9     '("#B26192" "#AF00AF" "brightmagenta"))

   (bg         eg-bg)
   (bg-alt     eg-bg2)
   (base0      (doom-lighten bg 0.1))
   (base1      eg-grey2)
   (base2      eg-grey3)
   (base3      eg-grey4)
   (base4      eg-grey5)
   (base5      eg-grey6)
   (base6      eg-grey7)
   (base7      eg-grey8)
   (base8      eg-fg2)
   (fg         eg-fg)
   (fg-alt     eg-grey8)

   (grey base5)
   (red       eg-red)
   (orange    eg-orange)
   (green     eg-green)
   (teal      eg-teal)
   (yellow    eg-orange)
   (blue      eg-blue)
   (dark-blue eg-blue)
   (magenta   eg-purple)
   (violet    eg-purple)
   (cyan      eg-teal)
   (dark-cyan eg-teal)

   ;; face categories -- required for all themes
   (highlight eg-blue8)
   (vertical-bar base2)
   (selection eg-purple4)
   (builtin fg)
   (comments (if doom-earl-grey-brighter-comments
                 eg-grey7
                 eg-grey6))
   (doc-comments comments)
   (constants teal)
   (functions fg)
   (keywords magenta)
   (methods fg)
   (operators fg)
   (type fg)
   (strings green)
   (variables blue)
   (numbers teal)
   (region eg-berry1)
   (error red)
   (warning yellow)
   (success green)
   (vc-modified eg-orange8)
   (vc-added eg-green8)
   (vc-deleted eg-red8)

   ;; custom categories
   (hidden `(,(car bg) "black" "black"))
   (-modeline-bright doom-earl-grey-brighter-modeline)
   (-modeline-pad
    (when doom-earl-grey-padded-modeline
      (if (integerp doom-earl-grey-padded-modeline)
          doom-earl-grey-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt eg-grey6)

   (modeline-bg
    (if -modeline-bright
        (doom-darken eg-grey2 0.1)
      eg-grey2))
   (modeline-bg-l
    (if -modeline-bright
        base2
      (doom-blend base1 fg 0.96)))
   (modeline-bg-inactive eg-grey1)
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  (
   ;; Modeline
   (doom-modeline-buffer-path       :foreground blue)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path
                                    :bold 'bold)
   (doom-modeline-info              :foreground green)
   (doom-modeline-project-dir       :foreground magenta)
   (doom-modeline-evil-insert-state :foreground teal)
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))
   (doom-modeline-project-root-dir :foreground base6)

   ;; solaire
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Font-Lock
   (font-lock-comment-face
    :foreground comments
    :inherit 'italic)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
    (font-lock-comment-delimiter-face    :inherit font-lock-comment-face)
    (font-lock-builtin-face              :foreground fg
                                         :inherit 'italic :extend t)
    (font-lock-type-face                 :foreground fg
                                         :inherit 'italic :extend t)
    (font-lock-variable-name-face        :foreground blue)
    (font-lock-warning-face              :foreground red)
    (font-lock-negation-char-face        :foreground orange
                                         :inherit 'default
                                         :extend t)
    (font-lock-preprocessor-face         :foreground orange
                                         :inherit 'default
                                         :extend t)
    (font-lock-preprocessor-char-face    :inherit 'default)
    (font-lock-regexp-grouping-backslash :inherit 'default)
    (font-lock-regexp-grouping-construct :inherit 'default)
    (font-lock-constant-face             :foreground teal)
    (font-lock-function-name-face        :foreground fg
                                         :inherit 'italic :extend t)

    ;; makefile-*-mode
    (makefile-targets :foreground magenta)
    (makefile-space :background eg-red2)
    (makefile-makepp-perl :background eg-blue1)

    ;; which-key
    (which-key-key-face                   :foreground eg-purple8)
    (which-key-group-description-face     :foreground eg-blue8)
    (which-key-command-description-face   :foreground fg)
    (which-key-local-map-description-face :foreground orange)
    (which-key-separator-face             :background bg-alt
                                          :foreground comments)

    ;; highlight-numbers-mode
    (highlight-numbers-number :foreground teal)

    ;; web-mode
    (web-mode-doctype-face           :foreground comments)
    (web-mode-html-tag-face          :foreground magenta)
    (web-mode-html-attr-name-face    :foreground blue)
    (web-mode-html-attr-value-face   :inherit 'font-lock-string-face)
    (web-mode-html-entity-face       :foreground orange
                                     :inherit 'italic)
    (web-mode-block-control-face     :foreground magenta)
    (web-mode-html-tag-bracket-face  :foreground fg-alt)
    (web-mode-symbol-face            :foreground blue)
    (web-mode-string-face            :inherit 'font-lock-string-face)
    (web-mode-current-element-highlight-face :foreground bg)

    ;; xml
    (nxml-element-local-name :foreground magenta)

    ;; ocaml
    (tuareg-font-lock-governing-face :foreground magenta)
    (tuareg-font-lock-operator-face :foreground orange)

    ;; haskell
    (haskell-constructor-face :foreground teal)
    (haskell-operator-face :foreground fg)
    ((haskell-type-face &override)
     :foreground blue
     :inherit 'font-lock-type-face
     :extend t)
    ((haskell-definition-face &override)
     :foreground magenta
     :inherit 'font-lock-function-name-face
     :extend t)

    ;; Highlight
    (lazy-highlight :foreground eg-fg2
                    :background eg-berry2 :inherit 'default :extend t)

    ;; php
    (php-$this :foreground orange)


    ;; rjsx-mode
    (rjsx-tag  :foreground magenta)
    (rjsx-text :inherit 'default)
    (rjsx-tag-bracket-face :foreground fg-alt)
    (rjsx-attr :foreground blue)

    ;; highlight-quoted-mode
    (highlight-quoted-symbol :foreground blue)
    (highlight-quoted-quote  :foreground teal)

    ;; rainbow-delimiters
    (rainbow-delimiters-depth-1-face :foreground eg-blue6)
    (rainbow-delimiters-depth-2-face :foreground eg-purple6)
    (rainbow-delimiters-depth-3-face :foreground eg-green6)
    (rainbow-delimiters-depth-4-face :foreground eg-orange6)
    (rainbow-delimiters-depth-5-face :foreground eg-teal6)
    (rainbow-delimiters-depth-6-face :foreground eg-blue6)
    (rainbow-delimiters-depth-7-face :foreground eg-purple6)
    (rainbow-delimiters-unmatched-face  :foreground red
                                        :weight 'bold
                                        :inverse-video t)
    (rainbow-delimiters-mismatched-face
     :inherit 'rainbow-delimiters-unmatched-face)

    ;; swiper
    (swiper-line-face    :background  eg-purple2
                         :foreground fg
                         :weight 'semi-bold)
    (swiper-match-face-1 :inherit 'unspecified
                         :background  eg-purple1
                         :foreground fg)
    (swiper-background-match-face-1 :inherit 'unspecified
                         :background  eg-bg2
                         :foreground fg)
    (swiper-match-face-2 :inherit 'unspecified
                         :background eg-purple1
                         :foreground eg-purple)
    (swiper-background-match-face-2 :inherit 'unspecified
                         :background eg-purple1
                         :foreground eg-purple
                         :weight 'semi-bold)
    (swiper-match-face-3 :inherit 'unspecified
                         :background eg-blue1
                         :foreground blue)
    (swiper-background-match-face-3 :inherit 'unspecified
                         :background eg-blue1
                         :foreground blue
                         :weight 'semi-bold)
    (swiper-match-face-4 :inherit 'unspecified
                         :background eg-teal1
                         :foreground teal)
    (swiper-background-match-face-4 :inherit 'unspecified
                         :background eg-teal1
                         :foreground teal
                         :weight 'semi-bold)

    ;; tooltip
    (tooltip :background bg-alt :foreground fg)

    ;; company
    (company-tooltip            :inherit 'tooltip)
    (company-tooltip-annotation            :foreground magenta)
    (company-tooltip-annotation-selection  :foreground magenta )
    (company-tooltip-common                :foreground magenta
                                           :distant-foreground bg-alt
                                           :weight 'bold)
    (company-tooltip-search     :background magenta
                                :foreground bg
                                :distant-foreground fg
                                :weight 'bold)
    (company-tooltip-search-selection :background eg-purple1)
    (company-tooltip-selection  :background eg-purple1
                                :weight 'bold)
    (company-tooltip-mouse      :background eg-purple8
                                :foreground bg
                                :distant-foreground fg)
    (company-tooltip-annotation :foreground magenta
                                :distant-foreground bg)
    (company-scrollbar-bg       :inherit 'tooltip)
    (company-scrollbar-fg       :background highlight)
    (company-preview            :foreground comments)
    (company-preview-common     :background base3
                                :foreground highlight)
    (company-preview-search     :inherit 'company-tooltip-search)
    (company-template-field     :inherit 'match)
    (company-echo-common        :background eg-red2
                                :foreground fg)

    ;; company-box
    (company-box-candidate :foreground fg)

   ((region &override)
    :foreground fg)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base6)
   ((paren-face-match &override) :foreground red :background eg-grey1 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override) :foreground teal)

   ;; parens
   ((show-paren-match &override)
    :background eg-grey1)

   ;; elscreen
   (elscreen-tab-other-screen-face :background bg-alt :foreground fg)

   ;; Magit / Diff
   (magit-diff-hunk-heading-highlight :foreground bg :background eg-blue8 :weight 'bold)
   (magit-diff-hunk-heading :foreground bg :background eg-blue5)
   (magit-blame-heading     :foreground magenta
                            :background eg-grey1 :extend t)
   (magit-blame-date       :foreground blue)

   (magit-diff-removed :background eg-red1
                       :foreground eg-red)
   (magit-diff-removed-highlight :background eg-red3
                                 :foreground eg-red)
   (diff-refine-removed :background eg-red9
                        :foreground eg-red1)

   (magit-diff-added :background eg-green1
                     :foreground eg-green)
   (magit-diff-added-highlight :background eg-green3
                               :foreground eg-green)
   (diff-refine-added :background eg-green
                      :foreground bg)

   (diff-refine-changed :background eg-purple9
                        :foreground bg)

   (git-commit-summary :foreground fg)


   ;; Dired
   (diredfl-date-time    :foreground blue)
   (diredfl-dir-heading  :foreground magenta :weight 'bold)

   ;; ivy
   (ivy-posframe :background eg-blue1)
   (ivy-virtual :foreground eg-blue8)
   (ivy-cursor :foreground bg-alt
               :background fg)
   (ivy-minibuffer-match-face-1
    :background nil
    :foreground comments
    :weight 'semi-bold)
    (ivy-minibuffer-match-face-2
     :inherit 'ivy-minibuffer-match-face-1
     :foreground eg-purple :background eg-purple1)
    (ivy-minibuffer-match-face-3
     :inherit 'ivy-minibuffer-match-face-2
     :foreground blue :background eg-orange1)
    (ivy-minibuffer-match-face-4
     :inherit 'ivy-minibuffer-match-face-2
     :foreground teal :background eg-teal1)

   (internal-border
    :foreground eg-blue8
    :background eg-blue1)
   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-property             :foreground blue
                             :inherit 'italic)
   (css-proprietary-property :foreground orange)
   (css-selector             :foreground magenta)
   (web-mode-css-property-name-face :foreground fg)

   ;; markdown-mode
   (markdown-header-face           :inherit 'bold
                                   :foreground magenta)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground green)
   (markdown-list-face             :foreground fg
                                   :inherit 'bold)
   (markdown-link-face             :foreground teal)
   (markdown-url-face              :foreground blue)
   (markdown-italic-face           :inherit 'italic
                                   :foreground fg)
   (markdown-bold-face             :inherit 'bold
                                   :foreground fg)
   (markdown-markup-face           :foreground fg
                                   :inherit 'bold)
   (markdown-blockquote-face       :inherit 'italic
                                   :foreground orange)
   (markdown-pre-face              :foreground orange
                                   :extend t)
   (markdown-code-face             :foreground orange
                                   :extend t)
   (markdown-reference-face        :foreground blue)
   (markdown-inline-code-face      :inherit '(markdown-code-face markdown-pre-face)
                                   :extend nil)
   (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
   (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
   (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
   (markdown-html-tag-delimiter-face :inherit 'default)
   (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)

   (nav-flash-face :background eg-purple1 :foreground fg :weight 'bold)

   ;; org-mode
   ((outline-1 &override) :foreground magenta)
   ((outline-2 &override) :foreground red)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground orange)
   ((outline-5 &override) :foreground magenta)
   ((outline-6 &override) :foreground red)
   ((outline-7 &override) :foreground teal)
   ((outline-8 &override) :foreground orange)

   (org-drawer  :foreground eg-orange)
   (org-ellipsis :underline nil :background bg     :foreground red)
   ((org-block-begin-line &override)
    :foreground orange
    :background bg-alt
    :weight 'semi-bold
    :extend t)
   ((org-block &override)
    :background bg-alt
    :foreground fg
    :extend t)
   ((org-quote &override)
    :foreground orange
    :background bg-alt
    :extend t)
   ((org-document-title &override)
    :foreground magenta)

   ;; js2-mode
   (js2-function-param    :foreground blue)
   (js2-function-call     :foreground fg :inherit 'italic)
   (js2-object-property   :foreground fg)
   (js2-jsdoc-tag         :foreground doc-comments)
   (js2-external-variable :foreground fg)

   ;; racket
   (racket-keyword-argument-face :foreground orange)
   (racket-selfeval-face :foreground teal)
   (racket-debug-break-face :foreground bg :background red)

   ;; clojure
   (clojure-keyword-face :foreground blue)

   ;; elixir
   (elixir-atom-face :foreground blue)
   (elixir-attribute-face :foreground teal)

   ;; lsp
   (lsp-ui-doc-background      :background bg-alt)
   (lsp-face-highlight-read    :inherit 'lazy-highlight)
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)

   ;; doom dashboard
   (doom-dashboard-banner      :foreground eg-grey5)
   (doom-dashboard-menu-title  :foreground eg-purple8)
   (doom-dashboard-menu-desc   :foreground eg-green8)
   (doom-dashboard-footer-icon :foreground eg-orange8)
   (doom-dashboard-loaded      :foreground eg-blue8)

   ;; evil-snipe
   (evil-snipe-first-match-face :foreground bg :background blue)

   ;; End
   )

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-earl-grey-theme.el ends here
