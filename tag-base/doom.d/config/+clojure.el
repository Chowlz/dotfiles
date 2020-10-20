;;; modules/+clojure.el -*- lexical-binding: t; -*-

;; Based off of spacemac's https://github.com/syl20bnr/spacemacs/blob/develop/layers/+lang/clojure/funcs.el
;; commit: 3663b29a48d8a47c9920f9e9261f94630ca389d8
(after! cider
  (set-popup-rule! "^\\*cider-error*" :quit t :modeline t)
  (set-popup-rule! "^\\*cider-repl" :ignore t :modeline t))

;;;###autoload
(defun ++clojure/cider-find-var (sym-name &optional arg)
  "Attempts to jump-to-definition of the symbol-at-point. If CIDER fails, or not available, falls
back to dumb-jump."
  (interactive (list (cider-symbol-at-point)))
  (if (and (cider-connected-p) (cider-var-info sym-name))
      (unless (eq 'symbol (type-of (cider-find-var nil sym-name)))
        (dumb-jump-go))
    (dumb-jump-go)))

;;;###autoload
(defun ++clojure/cider-display-error-buffer (&optional arg)
  "Displays the *cider-error* buffer in the current window. If called with a prefix argument, uses
the other-window instead."
  (interactive "P")
  (let ((buffer (get-buffer cider-error-buffer)))
    (when buffer
      (funcall (if (equal arg '(4))
                   'switch-to-buffer-other-window
                 'switch-to-buffer)
               buffer))))

;;;###autoload
(defun ++clojure/cider-eval-sexp-beginning-of-line ()
  "Evaluate the last sexp at the beginning of the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cider-eval-last-sexp)))

;;;###autoload
(defun ++clojure/cider-eval-sexp-end-of-line ()
  "Evaluate the last sexp at the end of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (cider-eval-last-sexp)))

;;;###autoload
(defun ++clojure/cider-eval-in-repl-no-focus (form)
  "Insert FORM in the REPL buffer and eval it."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (with-current-buffer (cider-current-connection)
    (let ((pt-max (point-max)))
      (goto-char pt-max)
      (insert form)
      (indent-region pt-max (point))
      (cider-repl-return)
      (with-selected-window (get-buffer-window (cider-current-connection))
        (goto-char (point-max))))))

;;;###autoload
(defun ++clojure/cider-send-last-sexp-to-repl ()
  "Send last sexp to REPL and evaluate it without changing the focus."
  (interactive)
  (++clojure//cider-eval-in-repl-no-focus (cider-last-sexp)))

;;;###autoload
(defun ++clojure/cider-send-last-sexp-to-repl-focus ()
  "Send last sexp to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-last-sexp-in-repl t)
  (evil-insert-state))

;;;###autoload
(defun ++clojure/cider-send-region-to-repl (start end)
  "Send region to REPL and evaluate it without changing the focus."
  (interactive "r")
  (++clojure//cider-eval-in-repl-no-focus
   (buffer-substring-no-properties start end)))

;;;###autoload
(defun ++clojure/cider-send-region-to-repl-focus (start end)
  "Send region to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive "r")
  (cider-insert-in-repl
   (buffer-substring-no-properties start end) t)
  (evil-insert-state))

;;;###autoload
(defun ++clojure/cider-send-function-to-repl ()
  "Send current function to REPL and evaluate it without changing the focus."
  (interactive)
  (++clojure//cider-eval-in-repl-no-focus (cider-defun-at-point)))

;;;###autoload
(defun ++clojure/cider-send-function-to-repl-focus ()
  "Send current function to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-defun-in-repl t)
  (evil-insert-state))

;;;###autoload
(defun ++clojure/cider-send-ns-form-to-repl-focus ()
  "Send ns form to REPL and evaluate it and switch to the REPL in `insert state'."
  (interactive)
  (cider-insert-ns-form-in-repl t)
  (evil-insert-state))

;;;###autoload
(defun ++clojure/cider-find-and-clear-repl-buffer ()
  "Calls cider-find-and-clear-repl-output interactively with C-u prefix set so that it clears the
whole REPL buffer, not just the output."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'cider-find-and-clear-repl-output)))
