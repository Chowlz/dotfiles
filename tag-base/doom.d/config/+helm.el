;;; modules/+helm.el -*- lexical-binding: t; -*-

(after! helm
  (setq helm-ff-lynx-style-map t))

;;;###autoload
(defun ++helm/helm-rg ()
  "Set the directory before calling helm-rg."
  (interactive)
  (let ((helm-rg--current-dir (read-directory-name "Start directory: " default-directory nil t)))
    (helm-rg "")))
