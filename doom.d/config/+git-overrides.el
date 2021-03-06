;;; overides/git-overides.el -*- lexical-binding: t; -*-

;; Init copied and modified to use 'z' instead of 'Z' for stashing in the git status transient menu.
;; In doom-emacs, 'z' is used for folding, but this is disabled in spacemacs.
(use-package-hook! evil-magit
  :pre-init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds nil)
  nil)

;; Config copied and modified for the same reason, but removes the evil-define-key* section in the
;; magit-mode-map involved with 'z'.
(use-package-hook! evil-magit
  :pre-config
  (undefine-key! magit-mode-map
    ;; Replaced by z1, z2, z3, etc
    "M-1" "M-2" "M-3" "M-4"
    "1" "2" "3" "4"
    "0") ; moved to g=
  (evil-define-key* 'normal magit-status-mode-map [escape] nil) ; q is enough
  (evil-define-key* '(normal visual) magit-mode-map
    "%"  #'magit-gitflow-popup
    "g=" #'magit-diff-default-context
    "gi" #'forge-jump-to-issues
    "gm" #'forge-jump-to-pullreqs)
  (define-key! 'normal
    (magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-diff-mode-map)
    [tab] #'magit-section-toggle)
  (after! git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (when-let (desc (assoc (car key) evil-magit-rebase-commands-w-descriptions))
        (setcar desc (cdr key))))
    (evil-define-key* evil-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up))
  (transient-replace-suffix 'magit-dispatch 'magit-worktree
    '("%" "Gitflow" magit-gitflow-popup))
  (transient-append-suffix 'magit-dispatch '(0 -1 -1)
    '("*" "Worktree" magit-worktree))
  nil)
