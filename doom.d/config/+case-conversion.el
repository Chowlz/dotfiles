;;; modules/+case-conversion.el -*- lexical-binding: t; -*-

;; Keybindings
(map!
 (:leader
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
    :desc "Screaming snake"                        :n   "S"       #'case-conversion-screaming-snake))))
