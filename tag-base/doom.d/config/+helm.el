;;; modules/+helm.el -*- lexical-binding: t; -*-

(after! helm
  (setq helm-buffer-max-length 100)
  (setq helm-ff-lynx-style-map t))

;;;###autoload
(defun ++helm/helm-rg ()
  "Set the directory before calling helm-rg."
  (interactive)
  (require 'helm-rg)
  (let ((helm-rg--current-dir (read-directory-name "Start directory: " default-directory nil t)))
    (helm-rg "")))

;; Keybindings
(undefine-key! helm-find-files-map "RET")

(map!
 (:leader
  (:desc "Save buffer"                            :n   "RET"     #'save-buffer)

  (:prefix ("f" . "file")
   :desc "Find file in private config"            :n   "c"       #'doom/find-file-in-private-config
   :desc "Open file"                              :n   "f"       #'helm-find-files
   :desc "Grep files"                             :n   "g"       #'++helm/helm-rg
   :desc "Find file in project"                   :n   "p"       #'helm-projectile)))
