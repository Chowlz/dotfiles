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

;; Org-mode default directory
(setq org-directory "~/notes/")

(after! company
  ;; Slow it down so I can press enter rather than selecting a suggestion
  (setq company-idle-delay 0.5))

(load! "config/+asciidoc")
(load! "config/+case-conversion")
(load! "config/+clojure")
(load! "config/+code")
(load! "config/+evil")
(load! "config/+fish")
(load! "config/+flycheck")
(load! "config/+git")
(load! "config/+helm")
(load! "config/+lisp")
(load! "config/+modeline")
(load! "config/+restart-emacs")
(load! "config/+typescript")
