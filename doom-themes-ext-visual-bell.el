;;; doom-themes-ext-visual-bell.el --- flash mode-line on error -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016-2021 Henrik Lissner
;;
;; Author: Henrik Lissner <https://github.com/hlissner>
;; Created: December 6, 2020
;; Modified: May 30, 2021
;; Version: 2.0.0
;; Keywords: custom themes, faces
;; Homepage: https://github.com/hlissner/emacs-doom-themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;; Code:

(require 'face-remap)

(defface doom-themes-visual-bell '((t :inherit error))
  "Face to use for the mode-line when `doom-themes-visual-bell-config' is used."
  :group 'doom-themes)

;;;###autoload
(defun doom-themes-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((doom-themes--bell-cookie (face-remap-add-relative 'mode-line 'doom-themes-visual-bell)))
    (force-mode-line-update)
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (with-current-buffer buf
                        (face-remap-remove-relative cookie)
                        (force-mode-line-update)))
                    doom-themes--bell-cookie
                    (current-buffer))))

;;;###autoload
(defun doom-themes-visual-bell-config ()
  "Enable flashing the mode-line on error."
  (setq ring-bell-function #'doom-themes-visual-bell-fn
        visible-bell t))

(provide 'doom-themes-ext-visual-bell)
;;; doom-themes-ext-visual-bell.el ends here
