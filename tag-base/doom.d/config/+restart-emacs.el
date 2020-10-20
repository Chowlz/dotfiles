;;; modules/+restart-emacs.el -*- lexical-binding: t; -*-

(after! restart-emacs
  (defun restart-emacs--start-emacs-in-terminal (&optional args)
    "Start Emacs in current terminal (customized for 24-bit color)"
    (suspend-emacs (format (if (string= (getenv "COLORTERM") "truecolor") "fg ; TERM=xterm-24bits %s %s -nw" "fg ; %s %s -nw")
                           (shell-quote-argument (restart-emacs--get-emacs-binary))
                           (restart-emacs--string-join (mapcar #'shell-quote-argument
                                                               args)
                                                       " ")))))

;;;###autoload
(defun ++restart-emacs/restart-session-base-color ()
  "Restart Emacs without TERM set "
  (interactive)
  (setenv "COLORTERM" nil)
  (restart-emacs))

;;;###autoload
(defun ++restart-emacs/restart-session-24-bit-color ()
  "Restart Emacs with TERM=xterm-24bits"
  (interactive)
  (setenv "COLORTERM" "truecolor")
  (restart-emacs))
