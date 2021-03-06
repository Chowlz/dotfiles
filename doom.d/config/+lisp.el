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

;; Keybindings
(map!
 (:prefix (";" . "lisp")
  (:desc "Start of sexp"                          :nv  "C-a"     #'sp-beginning-of-sexp
   :desc "End of sexp"                            :nv  "C-e"     #'sp-end-of-sexp
   :desc "End of sexp"                            :nv  "$"       #'sp-end-of-sexp
   :desc "Jump item"                              :nv  "%"       #'evil-jump-item
   :desc "Start of sexp"                          :nv  "^"       #'sp-beginning-of-sexp
   :desc "Insert sexp before"                     :nv  "("       #'++lisp/lisp-insert-sexp-before
   :desc "Insert sexp after"                      :nv  ")"       #'++lisp/lisp-insert-sexp-after
   :desc "Cleanup sexp"                           :nv  ";"       #'++lisp/lisp-cleanup-sexp
   :desc "Absorb sexp"                            :nv  "a"       #'sp-absorb-sexp
   :desc "Barf sexp"                              :nv  "b"       #'sp-forward-barf-sexp
   :desc "Backward barf sexp"                     :nv  "B"       #'sp-backward-barf-sexp
   :desc "Convolute sexp"                         :nv  "c"       #'sp-convolute-sexp
   :desc "Forward kill splice sexp"               :nv  "e"       #'sp-splice-sexp-killing-forward
   :desc "Backward kill splice sexp"              :nv  "E"       #'sp-splice-sexp-killing-backward
   :desc "Backward symbol"                        :nv  "h"       #'sp-backward-symbol
   :desc "Backward sexp"                          :nv  "H"       #'sp-backward-sexp
   :desc "Next closing paren"                     :nv  "j"       #'++lisp/lisp-next-closing-paren
   :desc "Join sexp"                              :nv  "J"       #'sp-join-sexp
   :desc "Prev opening paren"                     :nv  "k"       #'++lisp/lisp-prev-opening-paren
   :desc "Forward symbol"                         :nv  "l"       #'++lisp/lisp-forward-symbol
   :desc "Forward sexp"                           :nv  "L"       #'sp-forward-sexp
   :desc "Raise sexp"                             :nv  "r"       #'sp-raise-sexp
   :desc "Forward slurp sexp"                     :nv  "s"       #'sp-forward-slurp-sexp
   :desc "Backward slurp sexp"                    :nv  "S"       #'sp-backward-slurp-sexp
   :desc "Transpose sexp"                         :nv  "t"       #'sp-transpose-sexp
   :desc "Unwrap"                                 :nv  "u"       #'sp-unwrap-sexp
   :desc "Yank sexp"                              :nv  "y"       #'sp-copy-sexp
   (:prefix ("`" . "hybrid")
    :desc "Hybrid kill"                            :nv  "k"       #'sp-kill-hybrid-sexp
    :desc "Hybrid push"                            :nv  "p"       #'sp-push-hybrid-sexp
    :desc "Hybrid slurp"                           :nv  "s"       #'sp-slurp-hybrid-sexp
    :desc "Hybrid transpose"                       :nv  "t"       #'sp-transpose-hybrid-sexp)
   (:prefix ("d" . "delete")
    :desc "Delete symbol"                          :nv   "s"      #'sp-kill-symbol
    :desc "Backward delete symbol"                 :nv   "S"      #'sp-backward-kill-symbol
    :desc "Delete word"                            :nv   "w"      #'sp-kill-word
    :desc "Backward delete word"                   :nv   "W"      #'sp-backward-kill-word
    :desc "Delete sexp"                            :nv   "x"      #'sp-kill-sexp
    :desc "Backward delete sexp"                   :nv   "X"      #'sp-backward-kill-sexp)
   (:prefix ("w" . "wrap")
    :desc "Wrap with ()"                           :nv  "("       #'++lisp/lisp-wrap-paren
    :desc "Wrap with {}"                           :nv  "{"       #'++lisp/lisp-wrap-curly
    :desc "Wrap with []"                           :nv  "["       #'++lisp/lisp-wrap-square
    :desc "Wrap with **"                           :nv  "*"       #'++lisp/lisp-wrap-earmuff))))
