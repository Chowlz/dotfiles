;;; modules/+lisp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ++lisp/lisp-wrap-paren (&optional _)
  "Wrap a symbol with parenthesis."
  (interactive "P")
  (sp-wrap-with-pair "("))

;;;###autoload
(defun ++lisp/lisp-wrap-curly (&optional _)
  "Wrap a symbol with curly brackets."
  (interactive "P")
  (sp-wrap-with-pair "{"))

;;;###autoload
(defun ++lisp/lisp-wrap-square (&optional _)
  "Wrap a symbol with square brackets."
  (interactive "P")
  (sp-wrap-with-pair "["))

;;;###autoload
(defun ++lisp/lisp-wrap-earmuff (&optional _)
  "Wrap a symbol with earmuffs."
  (interactive "P")
  (sp-wrap-with-pair "*"))

;;;###autoload
(defun ++lisp/lisp-insert-sexp-before ()
  "Insert sexp before the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-backward-sexp)
    (evil-insert-state)
    (sp-newline)
    (evil-previous-visual-line)
    (evil-end-of-line)
    (insert " ")
    (sp-insert-pair "(")
    (indent-for-tab-command)))

;;;###autoload
(defun ++list/lisp-insert-sexp-after ()
  "Insert sexp after the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-up-sexp)
    (evil-insert-state)
    (sp-newline)
    (sp-insert-pair "(")))

;;;###autoload
(defun ++lisp/lisp-next-paren (&optional closing)
  "Go to the next/previous closing/opening parenthesis/bracket/brace."
  (if closing
      (let ((curr (point)))
        (forward-char)
        (unless (eq curr (search-forward-regexp "[])}]"))
          (backward-char)))
    (search-backward-regexp "[[({]")))

;;;###autoload
(defun ++lisp/lisp-prev-opening-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (++lisp/lisp-next-paren))

;;;###autoload
(defun ++lisp/lisp-next-closing-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (++lisp/lisp-next-paren 'closing))

;;;###autoload
(defun ++lisp/lisp-forward-symbol (&optional arg)
  "Go to the beginning of the next symbol."
  (interactive "P")
  (let ((n (if (char-equal (char-after) ?\() 1 2)))
    (sp-forward-symbol (+ (if arg arg 0) n))
    (sp-backward-symbol)))

;;;###autoload
(defun ++lisp/lisp-cleanup-sexp ()
  "Cleanup sexp."
  (interactive)
  (save-excursion
    (let ((beg nil)
          (end nil))
      (if (char-equal (char-after) ?\() (forward-char))
      (sp-beginning-of-sexp)
      (setq beg (point))
      (sp-end-of-sexp)
      (setq end (point))
      (evil-visual-select beg end 'inclusive)
      (delete-trailing-whitespace)
      (indent-region beg end nil)
      (untabify beg end))))
