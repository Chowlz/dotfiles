;;; modules/+code.el -*- lexical-binding: t; -*-

;; Set modes for shebang
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("ts-node" . typescript-mode))
(add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))

;; Set modes on file extension
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))

;; Keybindings
(map!
 (:leader
  (:prefix ("c" . "code")
   :desc "Comment line"                           :n   ";"       #'comment-line)))
