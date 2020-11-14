;;; modules/+modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline
  ;; Shorten path
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root))
