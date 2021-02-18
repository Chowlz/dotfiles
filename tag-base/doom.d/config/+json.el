;;; modules/+json.el -*- lexical-binding: t; -*-

(after! jsonnet-mode
  (set-electric! 'jsonnet-mode :chars '(?\n ?: ?{ ?})))
