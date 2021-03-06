;;; modules/+typescript.el -*- lexical-binding: t; -*-

(after! typescript-mode
  (setq typescript-indent-level 2
        typescript-backend 'tide
        typescript-fmt-on-save t)

  ;; Use js-mode's proper indentation function because typescript's inferior
  (require 'js)
  (setq js-indent-level 2)
  (setq js-indent-align-list-continuation nil)
  (advice-add 'typescript--proper-indentation :override 'js--proper-indentation))
