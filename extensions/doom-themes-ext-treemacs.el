;;; doom-themes-ext-treemacs.el --- ... -*- lexical-binding: t; no-byte-compile: t -*-
;;
;; Copyright (C) 2018-2022 Henrik Lissner
;;
;; Author: Henrik Lissner <contact@henrik.io>
;; Maintainer: Henrik Lissner <contact@henrik.io>
;; Created: July 10, 2018
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

(defgroup doom-themes-treemacs nil
  "Options for doom's treemacs theme."
  :group 'doom-themes)


;;
;;; Variables

(defcustom doom-themes-treemacs-enable-variable-pitch t
  "If non-nil, remap file, folder & project labels to `variable-pitch'.

See `doom-themes-treemacs-variable-pitch-face'."
  :type 'boolean
  :group 'doom-themes-treemacs)

(defcustom doom-themes-treemacs-line-spacing 1
  "Line-spacing for treemacs buffer."
  :type 'integer
  :group 'doom-themes-treemacs)

(defcustom doom-themes-treemacs-theme "doom-atom"
  "Default treemacs theme."
  :type '(radio (const :doc "A minimalistic atom-inspired icon theme" "doom-atom")
                (const :doc "A colorful icon theme leveraging all-the-icons" "doom-colors"))
  :group 'doom-themes-treemacs)

(defcustom doom-themes-treemacs-bitmap-indicator-width 3
  "Default treemacs bitmap indicators width."
  :type 'integer
  :group 'doom-themes-treemacs)

(defcustom doom-themes-treemacs-variable-pitch-face 'variable-pitch
  "The face to remap file/directory labels to.

Only takes effect if `doom-themes-treemacs-enable-variable-pitch' is non-nil."
  :type 'face
  :group 'doom-themes-treemacs)


;;
;;; Faces
(defface doom-themes-treemacs-root-face
  '((t (:inherit font-lock-string-face)))
  "Face used for the root icon in doom themes' treemacs theme."
  :group 'doom-themes-treemacs)

(defface doom-themes-treemacs-file-face
  '((t (:inherit font-lock-doc-face :slant normal)))
  "Face used for the directory and file icons in doom themes' treemacs theme."
  :group 'doom-themes-treemacs)

;;
;;; Library

(defun doom-themes-hide-fringes-maybe (&rest _)
  "Remove fringes in current window if `treemacs-fringe-indicator-mode' is nil"
  (when (display-graphic-p)
    (if treemacs-fringe-indicator-mode
        (set-window-fringes nil doom-themes-treemacs-bitmap-indicator-width 0)
      (set-window-fringes nil 0 0))))

(defun doom-themes-setup-tab-width (&rest _)
  "Set `tab-width' to 1, so tab characters don't ruin formatting."
  (setq tab-width 1))

(defun doom-themes-define-treemacs-fringe-indicator-bitmap ()
  "Defines `treemacs--fringe-indicator-bitmap'"
  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
        (make-vector 26 #b111) nil doom-themes-treemacs-bitmap-indicator-width)))

(defun doom-themes-setup-line-spacing ()
  "Set `line-spacing' in treemacs buffers."
  (setq line-spacing doom-themes-treemacs-line-spacing))

(defun doom-themes-hide-modeline ()
  (setq mode-line-format nil))

(defun doom-themes-enable-treemacs-variable-pitch-labels (&rest _)
  (when doom-themes-treemacs-enable-variable-pitch
    (dolist (face '(treemacs-root-face
                    treemacs-git-unmodified-face
                    treemacs-git-modified-face
                    treemacs-git-renamed-face
                    treemacs-git-ignored-face
                    treemacs-git-untracked-face
                    treemacs-git-added-face
                    treemacs-git-conflict-face
                    treemacs-directory-face
                    treemacs-directory-collapsed-face
                    treemacs-file-face
                    treemacs-tags-face))
      (let ((faces (face-attribute face :inherit nil)))
        (set-face-attribute
         face nil :inherit
         `(,doom-themes-treemacs-variable-pitch-face
           ,@(delq 'unspecified (if (listp faces) faces (list faces)))))))))

(defun doom-themes-fix-treemacs-icons-dired-mode ()
  "Set `tab-width' to 1 in dired-mode if `treemacs-icons-dired-mode' is active."
  (funcall (if treemacs-icons-dired-mode #'add-hook #'remove-hook)
           'dired-mode-hook
           #'doom-themes-setup-tab-width))

;;
;;; Bootstrap

(with-eval-after-load 'treemacs
  (unless (require 'all-the-icons nil t)
    (error "all-the-icons isn't installed"))

  (add-hook 'treemacs-mode-hook #'doom-themes-setup-tab-width)
  (add-hook 'treemacs-mode-hook #'doom-themes-setup-line-spacing)
  (add-hook 'treemacs-mode-hook #'doom-themes-define-treemacs-fringe-indicator-bitmap)

  ;; Fix #293: tabs messing up formatting in `treemacs-icons-dired-mode'
  (add-hook 'treemacs-icons-dired-mode-hook #'doom-themes-fix-treemacs-icons-dired-mode)

  ;; The modeline isn't useful in treemacs
  (add-hook 'treemacs-mode-hook #'doom-themes-hide-modeline)

  ;; Disable fringes (and reset them everytime treemacs is selected because it
  ;; may change due to outside factors)
  (add-hook 'treemacs-mode-hook #'doom-themes-hide-fringes-maybe)
  (advice-add #'treemacs-select-window :after #'doom-themes-hide-fringes-maybe)

  ;; variable-pitch labels for files/folders
  (doom-themes-enable-treemacs-variable-pitch-labels)
  (advice-add #'load-theme :after #'doom-themes-enable-treemacs-variable-pitch-labels)

  ;; minimalistic atom-inspired icon theme
  (let ((face-spec 'doom-themes-treemacs-file-face))
    (treemacs-create-theme "doom-atom"
      :config
      (progn
        (treemacs-create-icon
         :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face 'doom-themes-treemacs-root-face))
         :extensions (root-open))
        (treemacs-create-icon
         :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face 'doom-themes-treemacs-root-face))
         :extensions (root-closed))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face face-spec)
                       (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
         :extensions (dir-open))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                       (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
         :extensions (dir-closed))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face face-spec)
                       (all-the-icons-octicon "package" :v-adjust 0 :face face-spec)) :extensions (tag-open))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                       (all-the-icons-octicon "package" :v-adjust 0 :face face-spec))
         :extensions (tag-closed))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face face-spec))
         :extensions (tag-leaf))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "flame" :v-adjust 0 :face face-spec))
         :extensions (error))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "stop" :v-adjust 0 :face face-spec))
         :extensions (warning))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face face-spec))
         :extensions (info))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-media" :v-adjust 0 :face face-spec))
         :extensions ("ai" "aiff" "avi" "bmp" "eps" "flac" "gif" "ico" "indd"
                      "jpeg" "jpg" "midi" "mkv" "mov" "mp3" "mp4" "ogg" "png"
                      "psd" "svg" "tif" "tiff" "wav" "webm" "webp"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-code" :v-adjust 0 :face face-spec))
         :extensions ("accdb" "accdt" "actionscript" "adoc" "adoc" "ansible"
                      "antlr" "applescript" "asciidoc" "asm" "c" "cask" "cc"
                      "cc" "clj" "cljc" "cljs" "cmake" "coffee" "cpp" "css"
                      "cxx" "cython" "d" "dart" "diet" "diff" "dml"
                      "docker-compose.yml" "dockerfile" "dscript" "edn" "eex"
                      "el" "elm" "ex" "exs" "fennel" "fish" "fortran"
                      "fortran-modern" "fortranfreeform" "fsharp" "gdscript"
                      "go" "gradle" "graphql" "h" "hh" "hpp" "hs" "htm" "html"
                      "hy" "iced" "inc" "ino" "j2" "j2" "java" "jinja" "jinja2"
                      "jl" "js" "jsx" "kt" "kts" "ledger" "less" "lhs" "lisp"
                      "lua" "makefile" "matlab" "merlin" "mips" "ml" "mli"
                      "moonscript" "nim" "nims" "nix" "objectpascal" "ocaml"
                      "pascal" "patch" "pde" "perl" "pgsql" "php" "php4" "php5"
                      "phps" "pl" "plt" "pm" "pm6" "pony" "pp" "pp" "pro"
                      "prolog" "ps1" "purs" "py" "pyc" "r" "racket" "rb" "rd"
                      "rdx" "re" "rei" "rkt" "rktd" "rktl" "rs" "rsx" "sass"
                      "sbt" "scala" "scm" "scpt" "scrbl" "scribble" "scss" "sh"
                      "sql" "styles" "sv" "tex" "tpp" "ts" "tsx" "v"
                      "vagrantfile" "vh" "vhd" "vhdl" "vhms" "vim" "vue" "xsl"
                      "zsh" "zshrc"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "book" :v-adjust 0 :face face-spec))
         :extensions ("azw" "azw3" "cb7" "cba" "cbr" "cbt" "cbz" "ceb" "chm"
                      "djvu" "doc" "docx" "exe" "fb2" "inf" "kf8" "kfx" "lit"
                      "lrf" "lrx" "mobi" "opf" "or" "oxps" "pdb" "pdb" "pdb"
                      "pdg" "pkg" "prc" "ps" "rtf" "tr2" "tr3" "txt" "xeb" "xps"
                      "pot" "potx" "potm" "pps" "ppsx" "ppsm" "ppt" "pptx"
                      "pptm" "pa" "ppa" "ppam" "sldm" "sldx" ))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-faicon "cogs" :height 0.85 :v-adjust 0 :face face-spec))
         :extensions ("Vagrantfile" "babel.config.js" "babelignore" "babelrc"
                      "babelrc.js" "babelrc.json" "bashrc" "bazel" "bazelrc"
                      "bower.json" "bowerrc" "cabal" "cfg" "conf" "config"
                      "cson" "csv" "editorconfig" "envrc" "eslintignore"
                      "eslintrc" "feature" "gemfile" "git" "gitattributes"
                      "gitconfig" "gitignore" "gitmodules" "ideavimrc" "iml"
                      "ini" "inputrc" "json" "ledgerrc" "lock" "nginx"
                      "npm-shrinkwrap.json" "npmignore" "npmrc"
                      "package-lock.json" "package.json" "phpunit" "pkg" "plist"
                      "pom.xml" "properties" "terminalrc" "toml" "tridactylrc"
                      "vimperatorrc" "vimrc" "vrapperrc" "xdefaults" "xml"
                      "xresources" "yaml" "yarn-integrity" "yarnclean"
                      "yarnignore" "yarnrc" "yml"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face face-spec))
         :extensions ("md" "markdown" "rst" "org" "log" "txt" "contribute"
                      "license" "readme" "changelog"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face face-spec))
         :extensions ("exe" "dll" "obj" "so" "o" "out" "elc" "cmake-cache" "csr"
                      "eslintcache" "crt" "cer" "der" "pfx" "p12" "p7b" "p7r"
                      "DS_STORE" "key" "pem" "src" "crl" "sst" "stl" "ipynb"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-pdf" :v-adjust 0 :face face-spec))
         :extensions ("pdf"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-zip" :v-adjust 0 :face face-spec))
         :extensions ("zip" "xz" "7z" "tar" "gz" "rar" "tgz" "jar"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face face-spec))
         :extensions (fallback))))

    (treemacs-create-theme "doom-colors"
      :extends "doom-atom"
      :config
      (progn
        (treemacs-create-icon
         :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face 'doom-themes-treemacs-root-face))
         :extensions (root-open))
        (treemacs-create-icon
         :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face 'doom-themes-treemacs-root-face))
         :extensions (root-closed))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "flame" :height 0.8 :v-adjust 0 :face 'all-the-icons-red))
         :extensions (error))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "stop" :height 0.8 :v-adjust 0 :face 'all-the-icons-yellow))
         :extensions (warning))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face 'all-the-icons-green))
         :extensions (info))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-alltheicon "git" :height 0.85 :v-adjust 0.0 :face 'all-the-icons-red))
         :extensions ("gitignore" "git" "gitattributes" "gitconfig" "gitmodules"))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "book" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-blue))
         :extensions (license))

        (dolist (item all-the-icons-extension-icon-alist)
          (let* ((extension (car item))
                 (func (cadr item))
                 (args (append (list (cadr (cdr item))) '(:v-adjust -0.05 :height 0.85) (cdr (cddr item))))
                 (icon (apply func args)))
            (let* ((icon-pair (cons (format "  %s\t" icon) " "))
                   (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
                   (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
                   (gui-icon  (car icon-pair))
                   (tui-icon  (cdr icon-pair)))
              (ht-set! gui-icons extension gui-icon)
              (ht-set! tui-icons extension tui-icon))))

        ;; File extensions for whom the above did not work (likely because their
        ;; regexp is too complicated to be reversed with
        ;; `doom-themes--get-treemacs-extensions' -- which is too naive)
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-fileicon "R" :v-adjust 0 :face 'all-the-icons-dblue))
         :extensions ("r"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-code" :v-adjust 0 :face face-spec))
         :extensions ("elc")))))

  (treemacs-load-theme doom-themes-treemacs-theme))

;;;###autoload
(defun doom-themes-treemacs-config ()
  "Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.")

(provide 'doom-themes-ext-treemacs)
;;; doom-themes-treemacs.el ends here
