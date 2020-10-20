;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Charles Cruz"
      user-mail-address "mail@charlescruz.dev")

;; Set leader keys
(setq doom-leader-key "SPC"
      doom-leader-alt-key "M-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "M-,")

;; Theme
(setq doom-theme 'moonlight)

;; Skip silly exit prompts
(setq confirm-kill-emacs nil)

;; Show line numebers
(setq display-line-numbers-type t)

;; Show trailing whitespace
(setq show-trailing-whitespace t)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Set modes for shebang
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("ts-node" . typescript-mode))
(add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Allow going past one extra character
(setq evil-move-beyond-eol t)

;; Org-mode default directory
(setq org-directory "~/notes/")

(after! company
  ;; Slow it down so I can press enter rather than selecting a suggestion
  (setq company-idle-delay 0.5))

(after! doom-modeline
  ;; Shorten filename
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root))

(load! "config/+typescript")
(load! "config/+clojure")
(load! "config/+git")
(load! "config/+flycheck")
(load! "config/+helm")
(load! "config/+lisp")
(load! "config/+restart-emacs")
(load! "+keybindings")
