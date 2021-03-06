;;; modules/+evil.el -*- lexical-binding: t; -*-

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Allow going past one extra character
(setq evil-move-beyond-eol t)

;; Keybindings
(undefine-key! doom-leader-map
  ;; evil-window-map -> unused
  "w C-h" "w C-j" "w C-k" "w C-l")

(map!
 ;; Window (prefix "C-W")
 (:map evil-window-map
  "<left>"          #'evil-window-left
  "<down>"          #'evil-window-down
  "<up>"            #'evil-window-up
  "<right>"         #'evil-window-right)

 (:map evil-insert-state-map
  "C-a"             #'doom/backward-to-bol-or-indent
  "C-e"             #'doom/forward-to-last-non-comment-or-eol
  "C-u"             #'doom/backward-kill-to-bol-and-indent)

 (:map evil-normal-state-map
  "C-a"             #'doom/backward-to-bol-or-indent
  "C-e"             #'doom/forward-to-last-non-comment-or-eol
  "C-u"             #'doom/backward-kill-to-bol-and-indent)

 (:map evil-visual-state-map
  "C-a"             #'doom/backward-to-bol-or-indent
  "C-e"             #'doom/forward-to-last-non-comment-or-eol
  "C-u"             #'doom/backward-kill-to-bol-and-indent))
