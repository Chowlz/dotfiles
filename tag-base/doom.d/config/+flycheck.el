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

;; Keybindings
(map!
 (:leader
  (:prefix ("e" . "flycheck")
   :desc "Clear"                                  :n   "c"       #'flycheck-clear
   :desc "Describe checker"                       :n   "h"       #'flycheck-describe-checker
   :desc "Explain error at point"                 :n   "e"       #'flycheck-explain-error-at-point
   :desc "Errors list"                            :n   "l"       #'++flycheck/flycheck-error-list
   :desc "Select checker"                         :n   "s"       #'flycheck-select-checker
   :desc "Verify setup"                           :n   "v"       #'flycheck-verify-setup)))
