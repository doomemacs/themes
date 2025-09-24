;;; doom-themes-ext-visual-bell.el --- flash mode-line on error -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019-2024 Henrik Lissner
;;
;; Author: Henrik Lissner <contact@henrik.io>
;; Maintainer: Henrik Lissner <contact@henrik.io>
;; Created: July 29, 2019
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
  ;; Since emacs 29, the mode-line face is the parent of the new face
  ;; mode-line-active and mode-line-inactive. For remapping purposes, the
  ;; mode-line-active face has to be used, see details at:
  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=53636
  (let* ((buf (current-buffer))
         (faces (if (facep 'mode-line-active)
                   '(mode-line-active solaire-mode-line-active-face)
                 '(mode-line solaire-mode-line-face)))
         (cookies (mapcar (lambda (face)
                            (when (facep face)
                              (face-remap-add-relative face 'doom-themes-visual-bell)))
                          faces)))
    (force-mode-line-update)
    (run-with-timer 0.15 nil
                    (lambda ()
                      (with-current-buffer buf
                        (mapc #'face-remap-remove-relative cookies)
                        (force-mode-line-update))))))

;;;###autoload
(defun doom-themes-visual-bell-config ()
  "Enable flashing the mode-line on error."
  (setq ring-bell-function #'doom-themes-visual-bell-fn
        visible-bell t))

(provide 'doom-themes-ext-visual-bell)
;;; doom-themes-ext-visual-bell.el ends here
