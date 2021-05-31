;;; test-helper.el --- test init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(require 'doom-themes)
(require 'doom-themes-base)

(defalias '-color-p 'doom-themes--colors-p)
;; (defalias '-build-face 'doom-themes--build-face)

(defun -build-face (face)
  (eval (doom-themes--build-face face)))

(defmacro -colorize! (rulesA rulesB type)
  `(should
    (equal (doom-themes--colorize ',rulesA ',type)
           ',rulesB)))

(defmacro -with-colors! (colors &rest body)
  (declare (indent defun))
  `(let ((doom-themes--colors ',colors))
     (let* (,@colors)
       (setq doom-themes--colors
             (list ,@(cl-loop for (var val) in colors
                              collect `(cons ',var ,val))))
       ,@body)))

(defmacro -with-faces! (faces &rest body)
  (declare (indent defun))
  (let ((doom-themes--faces (doom-themes--apply-faces nil faces)))
    `(let* ((doom-themes--faces ',doom-themes--faces)
            (faces (list ,@(mapcar #'doom-themes--build-face doom-themes--faces))))
       ,@body
       faces)))



;;
;;; Bootstrap

(while command-line-args-left
  (let ((regexp "\\.el\\'")
        (path (expand-file-name (pop command-line-args-left))))
    (if (file-directory-p path)
        (setq command-line-args-left
              (append (directory-files path nil regexp)
                      command-line-args-left))
      (when (string-match-p regexp path)
        (load path nil t)))))
(ert-run-tests-batch)

;;; test-helper.el ends here
