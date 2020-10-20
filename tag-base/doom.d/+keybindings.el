;;; +keybindings.el -*- lexical-binding: t; -*-

;; Unbind keys
(undefine-key! doom-leader-map
  ;; magit status -> remapped to "SPC g s"
  "g g"
  ;; magit stage hunk -> reused for magit-status
  "g s"
  ;; restart and restore-emacs -> reused for custom restart with/without TERM
  "q r"
  ;; evil-window-map -> unused
  "w C-h" "w C-j" "w C-k" "w C-l")

(undefine-key! evil-window-map
  "C-h" "C-j" "C-k" "C-l")

(undefine-key! helm-find-files-map "RET")

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
  "C-u"             #'doom/backward-kill-to-bol-and-indent)

 (:prefix (";" . "lisp")
  (:desc "Start of sexp"                          :nv  "C-a"     #'sp-beginning-of-sexp
   :desc "End of sexp"                            :nv  "C-e"     #'sp-end-of-sexp
   :desc "End of sexp"                            :nv  "$"       #'sp-end-of-sexp
   :desc "Jump item"                              :nv  "%"       #'evil-jump-item
   :desc "Start of sexp"                          :nv  "^"       #'sp-beginning-of-sexp
   :desc "Insert sexp before"                     :nv  "("       #'++lisp/lisp-insert-sexp-before
   :desc "Insert sexp after"                      :nv  ")"       #'++lisp/lisp-insert-sexp-after
   :desc "Cleanup sexp"                           :nv  "'"       #'++lisp/lisp-cleanup-sexp
   :desc "Absorb sexp"                            :nv  "a"       #'sp-absorb-sexp
   :desc "Barf sexp"                              :nv  "b"       #'sp-forward-barf-sexp
   :desc "Backward barf sexp"                     :nv  "B"       #'sp-backward-barf-sexp
   :desc "Convolute sexp"                         :nv  "c"       #'sp-convolute-sexp
   :desc "Forward kill splice sexp"               :nv  "e"       #'sp-splice-sexp-killing-forward
   :desc "Backward kill splice sexp"              :nv  "E"       #'sp-splice-sexp-killing-backward
   :desc "Backward symbol"                        :nv  "h"       #'sp-backward-symbol
   :desc "Backward sexp"                          :nv  "H"       #'sp-backward-sexp
   :desc "Next closing paren"                     :nv  "j"       #'++lisp/lisp-next-closing-paren
   :desc "Join sexp"                              :nv  "J"       #'sp-join-sexp
   :desc "Prev opening paren"                     :nv  "k"       #'++lisp/lisp-prev-opening-paren
   :desc "Forward symbol"                         :nv  "l"       #'++lisp/lisp-forward-symbol
   :desc "Forward sexp"                           :nv  "L"       #'sp-forward-sexp
   :desc "Raise sexp"                             :nv  "r"       #'sp-raise-sexp
   :desc "Forward slurp sexp"                     :nv  "s"       #'sp-forward-slurp-sexp
   :desc "Backward slurp sexp"                    :nv  "S"       #'sp-backward-slurp-sexp
   :desc "Transpose sexp"                         :nv  "t"       #'sp-transpose-sexp
   :desc "Unwrap"                                 :nv  "u"       #'sp-unwrap-sexp
   :desc "Yank sexp"                              :nv  "y"       #'sp-copy-sexp
   (:prefix ("`" . "hybrid")
    :desc "Hybrid kill"                            :nv  "k"       #'sp-kill-hybrid-sexp
    :desc "Hybrid push"                            :nv  "p"       #'sp-push-hybrid-sexp
    :desc "Hybrid slurp"                           :nv  "s"       #'sp-slurp-hybrid-sexp
    :desc "Hybrid transpose"                       :nv  "t"       #'sp-transpose-hybrid-sexp)
   (:prefix ("d" . "delete")
    :desc "Delete symbol"                          :nv   "s"      #'sp-kill-symbol
    :desc "Backward delete symbol"                 :nv   "S"      #'sp-backward-kill-symbol
    :desc "Delete word"                            :nv   "w"      #'sp-kill-word
    :desc "Backward delete word"                   :nv   "W"      #'sp-backward-kill-word
    :desc "Delete sexp"                            :nv   "x"      #'sp-kill-sexp
    :desc "Backward delete sexp"                   :nv   "X"      #'sp-backward-kill-sexp)
   (:prefix ("w" . "wrap")
    :desc "Wrap with ()"                           :nv  "("       #'++lisp/lisp-wrap-paren
    :desc "Wrap with {}"                           :nv  "{"       #'++lisp/lisp-wrap-curly
    :desc "Wrap with []"                           :nv  "["       #'++lisp/lisp-wrap-square
    :desc "Wrap with **"                           :nv  "*"       #'++lisp/lisp-wrap-earmuff)))

 (:leader
  (:desc "Save buffer"                            :n   "RET"     #'save-buffer)

  (:prefix ("-" . "case conversion")
   (:prefix ("f" . "flat")
    :desc "Upper"                                  :n   "u"       #'case-conversion-upper
    :desc "Lower"                                  :n   "l"       #'case-conversion-lower)
   (:prefix ("c" . "camel")
    :desc "Camel"                                  :n   "c"       #'case-conversion-camel
    :desc "Pascal"                                 :n   "p"       #'case-conversion-pascal)
   (:prefix ("k" . "kebab")
    :desc "Kebab"                                  :n   "k"       #'case-conversion-kebab
    :desc "Train"                                  :n   "t"       #'case-conversion-train
    :desc "Cobol"                                  :n   "c"       #'case-conversion-cobol)
   (:prefix ("s" . "snake")
    :desc "Snake"                                  :n   "s"       #'case-conversion-snake
    :desc "Camel snake"                            :n   "c"       #'case-conversion-camel-snake
    :desc "Screaming snake"                        :n   "S"       #'case-conversion-screaming-snake))

  (:prefix ("c" . "code")
   :desc "Comment line"                           :n   ";"       #'comment-line)

  (:prefix ("e" . "flycheck")
   :desc "Clear"                                  :n   "c"       #'flycheck-clear
   :desc "Describe checker"                       :n   "h"       #'flycheck-describe-checker
   :desc "Explain error at point"                 :n   "e"       #'flycheck-explain-error-at-point
   :desc "Errors list"                            :n   "l"       #'++flycheck/flycheck-error-list
   :desc "Select checker"                         :n   "s"       #'flycheck-select-checker
   :desc "Verify setup"                           :n   "v"       #'flycheck-verify-setup)

  (:prefix ("f" . "file")
   :desc "Find file in private config"            :n   "c"       #'doom/find-file-in-private-config
   :desc "Open file"                              :n   "f"       #'helm-find-files
   :desc "Grep files"                             :n   "g"       #'++helm/helm-rg
   :desc "Find file in project"                   :n   "p"       #'helm-projectile)

  (:prefix ("g" . "git")
   :desc "Magit status"                           :n   "s"       #'magit-status
   :desc "Magit blame"                            :n   "b"       #'magit-blame
   :desc "Magit switch branch"                    :n   "B"       #'magit-blame)

  (:prefix ("q" . "quit/session")
   (:prefix ("r" . "restart")
    :desc "Restart with TERM=xterm-24bits"          :n   "r"       #'++restart-emacs/restart-session-24-bit-color
    :desc "Restart with TERM unset"                 :n   "u"       #'++restart-emacs/restart-session-base-color)))

 )

(after! magit
  ;; Fix quitting magit-status
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'++git/magit-status-quit)
  (map! :map magit-status-mode-map "<escape>"  #'++git/magit-status-quit))

;; Based off of spacemac's https://github.com/syl20bnr/spacemacs/blob/develop/layers/+lang/clojure/packages.el
;; commit: ba982822c6b4b9e784d5c1fa35afa3e5126b4593
(after! (cider clj-refactor)
  ;; Unbind
  (map!
   :localleader
   (:map clojure-mode-map
    "R" nil)
   (:map cider-repl-mode-map
    "," nil "c" nil "n" nil "q" nil "r" nil)
   (:map (clojure-mode-map clojurescript-mode-map)
    "'" nil "\"" nil "c" nil "C" nil "i" nil "e" nil "g" nil
    "h" nil "m" nil "M" nil "n" nil "r" nil "t" nil))

  (map!
   :localleader
   (:map cider-repl-mode-map
    :desc "Switch to code"                         ","       #'cider-switch-to-last-clojure-buffer)
   (:map (clojure-mode-map clojurescript-mode-map)
    :desc "Switch to repl"                         ","       #'cider-switch-to-repl-buffer)
   (:map (clojure-mode-map clojurescript-mode-map cider-repl-mode-map)
    :desc "Start repl"                             "'"       #'sesman-start
    (:prefix ("d"  . "debug")
     :desc "Debug defun at point"                   "b"       #'cider-debug-defun-at-point
     :desc "Display cider errors"                   "e"       #'++clojure/cider-display-error-buffer
     :desc "Cider inspect"                          "i"       #'cider-inspect
     :desc "Cider inspect last result"              "r"       #'cider-inspect-last-result)
    (:prefix ("e"  . "evaluation")
     :desc "Eval sexp at beginning of line"         "^"         #'++clojure/cider-eval-sexp-beginning-of-line
     :desc "Eval sexp at end of line"               "$"         #'++clojure/cider-eval-sexp-end-of-line
     :desc "Eval defun to comment"                  ";"         #'cider-eval-defun-to-comment
     :desc "Eval buffer"                            "b"         #'cider-eval-buffer
     :desc "Eval last sexp"                         "e"         #'cider-eval-last-sexp
     :desc "Eval defun at point"                    "f"         #'cider-eval-defun-at-point
     :desc "Eval interupt"                          "i"         #'cider-interrupt
     :desc "Eval macroexpand"                       "m"         #'cider-macroexpand-1
     :desc "Eval macroexpand-all"                   "M"         #'cider-macroexpand-all
     :desc "Eval region"                            "r"         #'cider-eval-region
     :desc "Undef"                                  "u"         #'cider-undef
     :desc "Eval sexp at point"                     "v"         #'cider-eval-sexp-at-point
     :desc "Eval sexp up to point"                  "V"         #'cider-eval-sexp-up-to-point
     :desc "Eval last sexp and replace"             "w"         #'cider-eval-last-sexp-and-replace
     (:prefix ("n" . "namespace")
      :desc "Namespace refresh"                      "n"         #'cider-ns-refresh
      :desc "Namespace reload"                       "r"         #'cider-ns-reload
      :desc "Namespace reload all"                   "R"         #'cider-ns-reload-all)
     (:prefix ("p" . "pprint")
      :desc "Eval pprint defun to comment"            ";"         #'cider-pprint-eval-defun-to-comment
      :desc "Eval pprint last sexp to comment"        ":"         #'cider-pprint-eval-last-sexp-to-comment
      :desc "Eval pprint defun at point"              "f"         #'cider-pprint-eval-defun-at-point
      :desc "Eval pprint last sexp"                   "e"         #'cider-pprint-eval-last-sexp))
    (:prefix ("f"  . "format")
     :desc "Format buffer"                          "="         #'cider-format-buffer
     :desc "Format defun"                           "f"         #'cider-format-defun
     :desc "Format region"                          "r"         #'cider-format-region
     (:prefix ("e" . "edn")
      :desc "Format edn buffer"                      "b"         #'cider-format-edn-buffer
      :desc "Format edn last sexp"                   "e"         #'cider-format-edn-last-sexp
      :desc "Format edn region"                      "r"         #'cider-format-edn-region))
    (:prefix ("g"  . "goto")
     :desc "Pop back"                               "b"         #'cider-pop-back
     :desc "Classpath"                              "c"         #'cider-classpath
     :desc "Find var"                               "g"         #'++clojure/cider-find-var
     :desc "Jump to compilation error"              "e"         #'cider-jump-to-compilation-error
     :desc "Find namespace"                         "n"         #'cider-find-ns
     :desc "Find resource"                          "r"         #'cider-find-resource
     :desc "Browse spec"                            "s"         #'cider-browse-spec
     :desc "Browse all specs"                       "S"         #'cider-browse-spec-all)
    (:prefix ("h"  . "help")
     :desc "Apropos"                                "a"         #'cider-apropos
     :desc "Cheatsheet"                             "c"         #'cider-cheatsheet
     :desc "Clojuredocs"                            "d"         #'cider-clojuredocs
     :desc "Doc"                                    "h"         #'cider-doc
     :desc "Javadoc"                                "j"         #'cider-javadoc
     :desc "Browse namespace"                       "n"         #'cider-browse-ns
     :desc "Browse all namespaces"                  "N"         #'cider-browse-ns-all
     :desc "Browse spec"                            "s"         #'cider-browse-spec
     :desc "Browse all specs"                       "S"         #'cider-browse-spec-all)
    (:prefix ("m"  . "manage repls")
     :desc "Sesman start"                           "'"         #'sesman-start
     :desc "Sesman browser"                         "b"         #'sesman-browser
     :desc "Sesman info"                            "i"         #'sesman-info
     :desc "Sesman goto"                            "g"         #'sesman-goto
     :desc "Sesman quit"                            "q"         #'sesman-quit
     :desc "Sesman restart"                         "r"         #'sesman-restart
     (:prefix ("c" . "cider connect")
      :desc "Cider connect clj"                      "j"         #'cider-connect-clj
      :desc "Cider connect sibling clj"              "J"         #'cider-connect-sibling-clj
      :desc "Cider connect clj&cljs"                 "m"         #'cider-connect-clj&cljs
      :desc "Cider connect cljs"                     "s"         #'cider-connect-cljs
      :desc "Cider connect sibling cljs"             "S"         #'cider-connect-sibling-cljs)
     (:prefix ("j" . "cider jack-in")
      :desc "Cider jack-in clj"                      "j"         #'cider-jack-in-clj
      :desc "Cider jack-in clj&cljs"                 "m"         #'cider-jack-in-clj&cljs
      :desc "Cider jack-in cljs"                     "s"         #'cider-jack-in-cljs)
     (:prefix ("l" . "link")
      :desc "Sesman link with buffer"                "b"         #'sesman-link-with-buffer
      :desc "Sesman link with directory"             "d"         #'sesman-link-with-directory
      :desc "Sesman unlink"                          "u"         #'sesman-unlink
      :desc "Sesman link with project"               "p"         #'sesman-link-with-project)
     (:prefix ("s" . "cider session")
      :desc "Cider quit"                             "q"         #'cider-quit
      :desc "Cider restart"                          "r"         #'cider-restart))
    (:prefix ("p"  . "profile")
     :desc "Profile samples"                        "+"         #'cider-profile-samples
     :desc "Profile clear"                          "c"         #'cider-profile-clear
     :desc "Toggle profile namespace"               "n"         #'cider-profile-ns-toggle
     :desc "Profile var summary"                    "s"         #'cider-profile-var-summary
     :desc "Profile summary"                        "S"         #'cider-profile-summary
     :desc "Profile toggle"                         "t"         #'cider-profile-toggle
     :desc "Profile var profiled p"                 "v"         #'cider-profile-var-profiled-p)
    (:prefix ("r" . "refactor")
     :desc "Toggle keyword-string"                  ":"         #'clojure-toggle-keyword-string
     :desc "Profile samples"                        "+"         #'cider-profile-samples
     :desc "Align"                                  "="         #'clojure-align
     :desc "Insert namespace form"                  "n"         #'clojure-insert-ns-form
     :desc "Insert namespace form at point"         "N"         #'clojure-insert-ns-form-at-point
     :desc "Cycle if"                               "i"         #'clojure-cycle-if
     :desc "Cycle privacy"                          "p"         #'clojure-cycle-privacy
     :desc "Sort namespace"                         "s"         #'clojure-sort-ns
     :desc "Unwind"                                 "U"         #'clojure-unwind
     :desc "Unwind all"                             "U"         #'clojure-unwind-all
     (:prefix ("c" . "convert collection")
      :desc "Convert to set"                         "#"         #'clojure-convert-collection-to-set
      :desc "Convert to quoted list"                 "'"         #'clojure-convert-collection-to-quoted-list
      :desc "Convert to list"                        "("         #'clojure-convert-collection-to-list
      :desc "Convert to vector"                      "["         #'clojure-convert-collection-to-vector
      :desc "Convert to map"                         "{"         #'clojure-convert-collection-to-map)
     (:prefix ("t" . "Thread")
      :desc "Thread first"                           "f"         #'clojure-thread-first-all
      :desc "Thread last"                            "l"         #'clojure-thread-last-all
      :desc "Thread"                                 "t"         #'clojure-thread))
    (:prefix ("s"  . "send to repl")
     :desc "Load buffer"                            "b"         #'cider-load-buffer
     :desc "Load buffer and switch to repl"         "B"         #'cider-load-buffer-and-switch-to-repl-buffer
     :desc "Send last sexp"                         "e"         #'++clojure/cider-send-last-sexp-to-repl
     :desc "Send last sexp and switch to repl"      "E"         #'++clojure/cider-send-last-sexp-to-repl-focus
     :desc "Send function"                          "f"         #'++clojure/cider-send-function-to-repl
     :desc "Send function and switch to repl"       "F"         #'++clojure/cider-send-function-to-repl-focus
     :desc "Clear repl"                             "l"         #'cider-find-and-clear-repl-output
     :desc "Clear whole repl"                       "L"         #'++clojure/cider-find-and-clear-repl-buffer
     :desc "Set namespace"                          "n"         #'cider-repl-set-ns
     :desc "Send namespace and switch to repl"      "N"         #'++clojure/cider-send-ns-form-to-repl-focus
     :desc "Switch repls"                           "o"         #'cider-repl-switch-to-other
     :desc "Send region"                            "r"         #'++clojure/cider-send-region-to-repl
     :desc "Send region and switch to repl"         "R"         #'++clojure/cider-send-region-to-repl-focus
     :desc "Switch namespace and switch to repl"    "s"         #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
     :desc "Require repl utils"                     "u"         #'cider-repl-require-repl-utils)
    (:prefix ("t"  . "test")
     :desc "Rerun test"                             "a"        #'cider-test-rerun-test
     :desc "Show report"                            "b"        #'cider-test-show-report
     :desc "Run loaded tests"                       "l"        #'cider-test-run-loaded-tests
     :desc "Run namespace tests"                    "n"        #'cider-test-run-ns-tests
     :desc "Run project tests"                      "p"        #'cider-test-run-project-tests
     :desc "Rerun failed tests"                     "r"        #'cider-test-rerun-failed-tests
     :desc "Run namespace tests with filters"       "s"        #'cider-test-run-ns-tests-with-filters
     :desc "Run test"                               "t"        #'cider-test-run-test))))
