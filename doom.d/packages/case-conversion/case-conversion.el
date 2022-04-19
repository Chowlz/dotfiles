;; Adapted from https://github.com/akicho8/string-inflection/blob/master/string-inflection.el

(defconst case-conversion-word-chars "a-zA-Z0-9_-")

(defgroup case-conversion nil
  "Change the casing of words."
  :group 'convenience)

(defcustom case-conversion-skip-backward-when-done nil
  "Controls the position of the cursor after an inflection.
If nil remain at the end of the string after inflecting, else move backward to
the beginning."
  :group 'case-conversion
  :type 'boolean)

(defcustom case-conversion-erase-chars-when-region "./"
  "When selected in the region, this character is included in the transformation as part of the string.
Exactly assume that the underscore exists.
For example, when you select `Foo/Bar', it is considered that `Foo_Bar' is selected.
If include `:', select `FOO::VERSION' to run `M-x\ case-conversion-underscore' to `foo_version'."
  :group 'case-conversion
  :type 'string)

(defun case-conversion-get-word ()
  "Gets the symbol near the cursor"
  (interactive)
  (let* ((start (if (use-region-p)
                    (region-end)
                  (progn
                    (skip-chars-forward case-conversion-word-chars)

                    ;; https://github.com/akicho8/string-inflection/issues/30
                    ;;
                    ;;   objectName->method --> "objectName-" NG
                    ;;                      --> "objectName"  OK
                    (when (and (not (eobp)) (not (bobp)))
                      (when (string= (buffer-substring (1- (point)) (1+ (point))) "->")
                        (forward-char -1)))

                    (point))))
         (end (if (use-region-p)
                  (region-beginning)
                (progn
                  (skip-chars-backward case-conversion-word-chars)
                  (point))))
         (str (buffer-substring start end)))
    (prog1
        (progn
          (when (use-region-p)
            ;; https://github.com/akicho8/string-inflection/issues/31
            ;; Multiple lines will be one line because [:space:] are included to line breaks
            (setq str (replace-regexp-in-string (concat "[" case-conversion-erase-chars-when-region "]+") "_" str)) ; 'aa::bb.cc dd/ee' => 'aa_bb_cc dd_ee'

            ;; kebabing a region can insert an unexpected hyphen
            ;; https://github.com/akicho8/string-inflection/issues/34
            (with-syntax-table (copy-syntax-table)
              (modify-syntax-entry ?_ "w")
              (setq str (replace-regexp-in-string "_+\\b" "" str))   ; '__aA__ __aA__' => '__aA __aA'
              (setq str (replace-regexp-in-string "\\b_+" "" str)))) ; '__aA __aA'     => 'aA aA'
          str)
      (delete-region start end))))

(defun case-conversion-insert (s)
  (insert s)
  (when case-conversion-skip-backward-when-done (skip-chars-backward case-conversion-word-chars)))

;; Upper/lower functions
(defun case-conversion-to-lower (str)
  "Convert to lowercase"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "_" "" str))
    (setq str (replace-regexp-in-string "-" "" str))
    (downcase str)))

(defun case-conversion-to-upper (str)
  "Convert to uppercase"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "_" "" str))
    (setq str (replace-regexp-in-string "-" "" str))
    (upcase str)))

;; Hyphen functions
(defun case-conversion-to-kebab (str)
  "Convert to kebab-case"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "_" "-" str))
    (setq str (replace-regexp-in-string "-+" "-" str))
    (downcase str)))

(defun case-conversion-to-train (str)
  "Convert to Train-Case"
  (setq str (case-conversion-to-kebab str))
  (mapconcat 'capitalize (split-string str "-") "-"))

(defun case-conversion-to-camel-kebab (str)
  "Convert to camel-Kebab-Case"
  (setq str (case-conversion-to-train str))
  (let ((first-char (substring str nil 1))
        (rest-str (substring str 1)))
    (concat (downcase first-char) rest-str)))

(defun case-conversion-to-screaming-kebab (str)
  "Convert to SCREAMING_KEBAB-CASE"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "_" "-" str))
    (setq str (replace-regexp-in-string "-+" "-" str))
    (upcase str)))

;; Underscore functions
(defun case-conversion-to-snake (str)
  "Convert to snake_case"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))    ; ASingleLine_-Here   -> ASingle_Line_-Here
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str)) ; ASingle_Line_-Here  -> A_Single_Line_-Here
    (setq str (replace-regexp-in-string "-" "_" str))                                  ; A_Single_Line_-Here -> A_Single_Line__Here
    (setq str (replace-regexp-in-string "_+" "_" str))                                 ; A_Single_Line__Here -> A_Single_Line_Here
    (downcase str)))

(defun case-conversion-to-pascal-snake (str)
  "Convert to Pascal_Snake_Case"
  (setq str (case-conversion-to-snake str))
  (mapconcat 'capitalize (split-string str "_") "_"))

(defun case-conversion-to-camel-snake (str)
  "Convert to camel_Snake_Case"
  (setq str (case-conversion-to-pascal-snake str))
  (let ((first-char (substring str nil 1))
        (rest-str (substring str 1)))
    (concat (downcase first-char) rest-str)))

(defun case-conversion-to-screaming-snake (str)
  "Convert to SCREAMING_SNAKE_CASE"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))    ; ASingleLine_-Here   -> ASingle_Line_-Here
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str)) ; ASingle_Line_-Here  -> A_Single_Line_-Here
    (setq str (replace-regexp-in-string "-" "_" str))                                  ; A_Single_Line_-Here -> A_Single_Line__Here
    (setq str (replace-regexp-in-string "_+" "_" str))                                 ; A_Single_Line__Here -> A_Single_Line_Here
    (upcase str)))

;; Camel functions
(defun case-conversion-to-camel (str)
  "Convert to camelCase"
  (let* ((word-list (split-string (case-conversion-to-snake str) "_"))
         (first (downcase (car word-list)))
         (rest (mapconcat 'capitalize (cdr word-list) "")))
    (concat first rest)))

(defun case-conversion-to-pascal (str)
  "Convert to PascalCase"
  (setq str (case-conversion-to-snake str))
  (mapconcat 'capitalize (split-string str "_") ""))

;; Camel case variations
;;;###autoload
(defun case-conversion-camel ()
  "Ex: fooBar (i.e. lower casmel case)"
  (interactive)
  (case-conversion-insert (case-conversion-to-camel (case-conversion-get-word))))

;;;###autoload
(defun case-conversion-pascal ()
  "Ex: FooBar"
  (interactive)
  (case-conversion-insert (case-conversion-to-pascal (case-conversion-get-word))))

;; Upper/lower variations
;;;###autoload
(defun case-conversion-lower ()
  "Ex: foobar"
  (interactive)
  (case-conversion-insert (case-conversion-to-lower (case-conversion-get-word))))

;;;###autoload
(defun case-conversion-upper ()
  "Ex: FOOBAR"
  (interactive)
  (case-conversion-insert (case-conversion-to-upper (case-conversion-get-word))))

;; Hyphen variations
;;;###autoload
(defun case-conversion-camel-kebab ()
  "Ex: foo-Bar"
  (interactive)
  (case-conversion-insert (case-conversion-to-camel-kebab (case-conversion-get-word))))

;;;###autoload
(defun case-conversion-kebab ()
  "Ex: foo-bar"
  (interactive)
  (case-conversion-insert (case-conversion-to-kebab (case-conversion-get-word))))

;;;###autoload
(defun case-conversion-train ()
  "Ex: Foo-Bar"
  (interactive)
  (case-conversion-insert (case-conversion-to-train (case-conversion-get-word))))

;;;###autoload
(defun case-conversion-screaming-kebab ()
  "Ex: FOO-BAR"
  (interactive)
  (case-conversion-insert (case-conversion-to-screaming-kebab (case-conversion-get-word))))

;; Snake variations
;;;###autoload
(defun case-conversion-camel-snake ()
  "Ex: foo_Bar"
  (interactive)
  (case-conversion-insert (case-conversion-to-camel-snake (case-conversion-get-word))))

;;;###autoload
(defun case-conversion-pascal-snake ()
  "Ex: Foo_Bar"
  (interactive)
  (case-conversion-insert (case-conversion-to-pascal-snake (case-conversion-get-word))))

;;;###autoload
(defun case-conversion-snake ()
  "Ex: foo_bar"
  (interactive)
  (case-conversion-insert (case-conversion-to-snake (case-conversion-get-word))))

;;;###autoload
(defun case-conversion-screaming-snake ()
  "Ex: FOO_BAR"
  (interactive)
  (case-conversion-insert (case-conversion-to-screaming-snake (case-conversion-get-word))))

(provide 'case-conversion)
