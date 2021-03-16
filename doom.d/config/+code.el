;;; modules/+code.el -*- lexical-binding: t; -*-

;; Set modes for shebang
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("ts-node" . typescript-mode))
(add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))

;; Set modes on file extension
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))

;; NOTE Workaround for dumb-jump: https://github.com/jacktasia/dumb-jump/pull/387
(after! dumb-jump
  (advice-add 'dumb-jump-prompt-user-for-choice :override
              (lambda (proj results)
                (let ((choices (--map (dumb-jump--format-result proj it) results)))
                  (cond
                   ((eq dumb-jump-selector 'completing-read)
                    (dumb-jump-to-selected results choices (completing-read "Jump to: " choices)))
                   ((and (eq dumb-jump-selector 'ivy) (fboundp 'ivy-read))
                    (funcall dumb-jump-ivy-jump-to-selected-function results choices proj))
                   ((and (eq dumb-jump-selector 'helm) (fboundp 'helm))
                    (helm :sources
                          (helm-make-source "Jump to: " 'helm-source-sync
                            :action '(("Jump to match" . dumb-jump-result-follow))
                            :candidates (-zip choices results)
                            :persistent-action 'dumb-jump-helm-persist-action)
                          :buffer "*helm dumb jump choices*"))
                   (t
                    (dumb-jump-to-selected results choices (popup-menu* choices))))))))

;; Keybindings
(map!
 (:leader
  (:prefix ("c" . "code")
   :desc "Comment line"                           :n   ";"       #'comment-line)))
