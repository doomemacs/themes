;;; doom-themes-ext-visual-bell.el --- flash mode-line on error -*- lexical-binding: t; -*-

(require 'face-remap)

(defface doom-visual-bell '((t (:inherit error)))
  "Face to use for the mode-line when `doom-themes-visual-bell-config' is used."
  :group 'doom-themes)

;;;###autoload
(defun doom-themes-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((doom-themes--bell-cookie (face-remap-add-relative 'mode-line 'doom-visual-bell)))
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
