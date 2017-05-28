;;; doom-nlinum.el

(unless doom--inhibit-warning
  (message "doom-themes: loading doom-nlinum directly is obsolete, call `doom-themes-nlinum-config' instead"))

(defvar doom--nlinum-hl-overlay nil)
(defvar doom--nlinum-hl-line 0)

(defun doom-nlinum-hl-hook ()
  (if nlinum-mode
      (add-hook 'post-command-hook 'doom-nlinum-hl-line nil t)
    (remove-hook 'post-command-hook 'doom-nlinum-hl-line t)))

(defun doom--nlinum-overlay-p (ov)
  (overlay-get ov 'nlinum))

(defun doom-nlinum-hl-line (&rest _)
  "Highlight the current line number with nlinum."
  (while-no-input
    (let* ((pbol (line-beginning-position))
           (peol (1+ pbol))
           (max (point-max))
           (lineno (string-to-number (format-mode-line "%l"))))
      (unless (= doom--nlinum-hl-line lineno)
        (setq doom--nlinum-hl-line lineno)
        ;; Handle EOF case
        (when (>= peol max)
          (setq peol max))
        (jit-lock-fontify-now pbol peol)
        ;; Unhighlight previous highlight
        (when doom--nlinum-hl-overlay
          (let* ((disp (get-text-property 0 'display (overlay-get doom--nlinum-hl-overlay 'before-string)))
                 (str (nth 1 disp)))
            (put-text-property 0 (length str) 'face 'linum str)
            (setq doom--nlinum-hl-overlay nil)
            disp))
        (let ((ov (cl-find-if #'doom--nlinum-overlay-p (overlays-in pbol peol))))
          ;; Try to deal with evaporating line numbers (a known nlinum bug)
          (unless (or ov (eobp))
            (nlinum--flush)
            (setq ov (cl-find-if #'doom--nlinum-overlay-p (overlays-in pbol peol))))
          (when ov
            (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
              (put-text-property 0 (length str) 'face 'doom-linum-highlight str)
              (setq doom--nlinum-hl-overlay ov))))))))

(defun doom-nlinum-unhl-first-line (&optional pt)
  "Removes the hanging overlay hl-line sometimes leaves behind."
  (ignore-errors
    (dolist (overlay (overlays-at (or pt (point))))
      (when (eq (overlay-get overlay 'face) 'hl-line)
        (delete-overlay overlay)))))

(eval-after-load "nlinum"
  (lambda ()
    (add-hook 'nlinum-mode-hook #'doom-nlinum-hl-hook)
    (add-hook 'nlinum-mode-hook #'doom-nlinum-unhl-first-line)
    (add-hook 'after-change-major-mode-hook #'doom-nlinum-unhl-first-line)
    ;; hl-line-mode forces the current line number to redraw.
    ;; TODO manual redraw without hl-line
    (unless (bound-and-true-p global-hl-line-mode)
      (add-hook 'nlinum-mode-hook #'hl-line-mode))))

(provide 'doom-nlinum)
;;; doom-nlinum.el ends here
