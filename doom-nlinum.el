;;; doom-nlinum.el

(defvar doom--nlinum-hl-overlay nil)
(defvar doom--nlinum-hl-line 0)

(defun doom-nlinum-hl-hook ()
  (if nlinum-mode
      (add-hook 'post-command-hook 'doom-nlinum-hl-line nil t)
    (remove-hook 'post-command-hook 'doom-nlinum-hl-line t)))

(defun doom-nlinum-hl-line (&rest _)
  "Highlight current line number."
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
        (let ((ov (cl-find-if (lambda (ov) (overlay-get ov 'nlinum))
                              (overlays-in pbol peol))))
          (when ov
            (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
              (put-text-property 0 (length str) 'face 'doom-nlinum-highlight str)
              (setq doom--nlinum-hl-overlay ov))))))))

(defun doom-nlinum-unhl-first-line ()
  "Removes the hanging overlay hl-line sometimes leaves on the first line."
  (ignore-errors
    (dolist (overlay (overlays-at (point-min)))
      (when (eq (overlay-get overlay 'face) 'hl-line)
        (delete-overlay overlay)))))

(eval-after-load "nlinum"
  (lambda ()
    (add-hook 'nlinum-mode-hook 'doom-nlinum-hl-hook)
    (add-hook 'nlinum-mode-hook 'doom-nlinum-unhl-first-line)
    (unless (bound-and-true-p global-hl-line-mode)
      (add-hook 'nlinum-mode-hook 'hl-line-mode))))

(provide 'doom-nlinum)
;;; doom-nlinum.el ends here
