;;; modules/+case-conversion.el -*- lexical-binding: t; -*-

;; Keybindings
(map!
 (:leader
  (:prefix ("-" . "case conversion")
   (:prefix ("f" . "flat")
    :desc "UPPERCASE"                              :n   "u"       #'case-conversion-upper
    :desc "lowercase"                              :n   "l"       #'case-conversion-lower)
   (:prefix ("c" . "camel")
    :desc "camelCase"                              :n   "c"       #'case-conversion-camel
    :desc "PascalCase"                             :n   "p"       #'case-conversion-pascal)
   (:prefix ("k" . "kebab")
    :desc "kebab-case"                             :n   "k"       #'case-conversion-kebab
    :desc "camel-Kebab-Case"                       :n   "c"       #'case-conversion-camel-kebab
    :desc "Train-Case"                             :n   "t"       #'case-conversion-train
    :desc "SCREAMING-KEBAB-CASE"                   :n   "S"       #'case-conversion-screaming-kebab)
   (:prefix ("s" . "snake")
    :desc "camel_Snake_Case"                       :n   "c"       #'case-conversion-camel-snake
    :desc "Pascal_Snake_Case"                      :n   "p"       #'case-conversion-pascal-snake
    :desc "snake_case"                             :n   "s"       #'case-conversion-snake
    :desc "SCREAMING_SNAKE_CASE"                   :n   "S"       #'case-conversion-screaming-snake))))
