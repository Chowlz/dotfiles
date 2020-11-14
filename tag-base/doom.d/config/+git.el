;;; modules/+git.el -*- lexical-binding: t; -*-

(after! magit
  ;; Ensure lists are expanded in magit-status
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)
  (setf (alist-get 'stashes magit-section-initial-visibility-alist) 'show)

  ;; Maximum length of the header in github messages is 72
  (setq git-commit-summary-max-length 72)
  (setq-hook! 'git-commit-mode-hook fill-column 100)

  ;; Ask to save files in magit-status
  (setq magit-save-repository-buffers t)

  ;; Don't use the current current window in magit-status (split or use opposite window)
  (setq magit-display-buffer-function 'magit-display-buffer-traditional))

;; Alternative for quitting buffer
;;;###autoload
(defun ++git/magit-status-quit (&optional kill-buffer)
  (interactive "P")
  (funcall magit-bury-buffer-function kill-buffer))

;; Keybindings
(undefine-key! doom-leader-map
  ;; magit status -> remapped to "SPC g s"
  "g g"
  ;; magit stage hunk -> reused for magit-status
  "g s")

(after! magit
  ;; Fix quitting magit-status
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'++git/magit-status-quit)
  (map! :map magit-status-mode-map "<escape>"  #'++git/magit-status-quit))

(map!
 (:leader
  (:prefix ("g" . "git")
   :desc "Magit status"                           :n   "s"       #'magit-status
   :desc "Magit blame"                            :n   "b"       #'magit-blame
   :desc "Magit switch branch"                    :n   "B"       #'magit-blame)))
