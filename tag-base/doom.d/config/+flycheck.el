;;; modules/+flycheck.el -*- lexical-binding: t; -*-

(after! flycheck
  ;; Use defaults
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-buffer-switch-check-intermediate-buffers nil)

  (set-popup-rule! "^\\*Flycheck errors\\*$"
    :side 'bottom :slot 10 :vslot 100 :size 0.25 :select t :quit t))

;;;###autoload
(defun ++flycheck/flycheck-error-list ()
  "Toggle flycheck's error list window.
    If the error list is visible, hide it. Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))
