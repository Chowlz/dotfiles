;; Adapted from https://github.com/akicho8/string-inflection/blob/master/string-inflection.el

(defconst case-conversion-word-chars "a-zA-Z0-9_-")

(defun case-conversion-get-word ()
  "Gets the symbol near the cursor"
  (interactive)
  (let* ((start (if (use-region-p)
                    (region-end)
                  (progn
                    (skip-chars-forward case-conversion-word-chars)
                    (point))))
         (end (if (use-region-p)
                  (region-beginning)
                (progn
                  (skip-chars-backward case-conversion-word-chars)
                  (point))))
         (str (buffer-substring start end)))
    (prog1
        (if (use-region-p)
            (replace-regexp-in-string "[[:space:].:/]+" "_" str) ; 'aa::bb.cc dd/ee' => 'aa_bb_cc_dd_ee'
          str)
      (delete-region start end))))

;; Underscore functions
(defun case-conversion-to-snake (str)
  "Convert to snake_case"
  (let ((case-fold-search nil))
    (thread-last str
      (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2")    ; ASingleLine_-Here   -> ASingle_Line_-Here
      (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2") ; ASingle_Line_-Here  -> A_Single_Line_-Here
      (replace-regexp-in-string "-" "_")                                  ; A_Single_Line_-Here -> A_Single_Line__Here
      (replace-regexp-in-string "_+" "_")                                 ; A_Single_Line__Here -> A_Single_Line_Here
      (downcase))))

(defun case-conversion-to-camel-snake (str)
  "Convert to Camel_Snake_Case"
  (mapconcat 'capitalize
             (thread-first str
               (case-conversion-to-snake)
               (split-string "_"))
             "_"))

(defun case-conversion-to-screaming-snake (str)
  "Convert to SCREAMING_SNAKE_CASE"
  (let ((case-fold-search nil))
    (thread-last str
      (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2")    ; ASingleLine_-Here   -> ASingle_Line_-Here
      (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2") ; ASingle_Line_-Here  -> A_Single_Line_-Here
      (replace-regexp-in-string "-" "_")                                  ; A_Single_Line_-Here -> A_Single_Line__Here
      (replace-regexp-in-string "_+" "_")                                 ; A_Single_Line__Here -> A_Single_Line_Here
      (upcase))))

;; Hyphen functions
(defun case-conversion-to-kebab (str)
  "Convert to kebab-case"
  (let ((case-fold-search nil))
    (thread-last str
      (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2")
      (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2")
      (replace-regexp-in-string "_" "-")
      (replace-regexp-in-string "-+" "-")
      (downcase))))

(defun case-conversion-to-train (str)
  "Convert to Train-Case"
  (mapconcat 'capitalize
             (thread-first str
               (case-conversion-to-kebab)
               (split-string "_"))
             "-"))

(defun case-conversion-to-cobol (str)
  "Convert to COBOL-CASE"
  (let ((case-fold-search nil))
    (thread-last str
      (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2")
      (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2")
      (replace-regexp-in-string "_" "-")
      (replace-regexp-in-string "-+" "-")
      (upcase))))

;; Camel functions
(defun case-conversion-to-camel (str)
  "Convert to camelCase"
  (let* ((word-list (thread-first str
                      (case-conversion-to-snake)
                      (split-string "_")))
         (first (downcase (car word-list)))
         (rest (mapconcat 'capitalize (cdr word-list) "")))
    (concat first rest)))

(defun case-conversion-to-pascal (str)
  "Convert to PascalCase"
  (mapconcat 'capitalize
             (thread-first str
               (case-conversion-to-snake)
               (split-string "_"))
             ""))

;; Upper/lower functions
(defun case-conversion-to-lower (str)
  "Convert to lowercase"
  (let ((case-fold-search nil))
    (thread-last str
      (replace-regexp-in-string "_" "")
      (replace-regexp-in-string "-" "")
      (downcase))))

(defun case-conversion-to-upper (str)
  "Convert to uppercase"
  (let ((case-fold-search nil))
    (thread-last str
      (replace-regexp-in-string "_" "")
      (replace-regexp-in-string "-" "")
      (upcase))))

;; Upper/lower variations
;;;###autoload
(defun case-conversion-lower ()
  "Ex: foobar"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-lower)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

;;;###autoload
(defun case-conversion-upper ()
  "Ex: FOOBAR"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-upper)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

;; Camel case variations
;;;###autoload
(defun case-conversion-camel ()
  "Ex: fooBar (i.e. lower casmel case)"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-camel)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

;;;###autoload
(defun case-conversion-pascal ()
  "Ex: FooBar (i.e. upper casmel case)"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-pascal)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

(defalias 'case-conversion-lower-camel 'case-conversion-camel)
(defalias 'case-conversion-upper-camel 'case-conversion-pascal)

;; Hyphen variations
;;;###autoload
(defun case-conversion-kebab ()
  "Ex: foo-bar"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-kebab)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

;;;###autoload
(defun case-conversion-train ()
  "Ex: Foo-Bar"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-train)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

;;;###autoload
(defun case-conversion-cobol ()
  "Ex: FOO-BAR"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-cobol)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

(defalias 'case-conversion-screaming-kebab 'case-conversion-cobol)

;; Underscore variations
;;;###autoload
(defun case-conversion-snake ()
  "Ex: foo_bar"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-snake)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

;;;###autoload
(defun case-conversion-camel-snake ()
  "Ex: Foo_Bar"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-camel-snake)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

;;;###autoload
(defun case-conversion-screaming-snake ()
  "Ex: FOO_BAR"
  (interactive)
  (thread-last (case-conversion-get-word)
    (case-conversion-to-screaming-snake)
    (insert))
  (skip-chars-backward case-conversion-word-chars))

(provide 'case-conversion)
