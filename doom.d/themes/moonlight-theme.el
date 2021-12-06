;;; moonlight-theme.el --- inspired by VS code's Moonlight -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup moonlight-theme nil
  "Options for the custom `doom-moonlight` theme."
  :group 'doom-themes)

(defcustom moonlight-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'moonlight-theme
  :type '(choice integer boolean))

(setq evil-emacs-state-tag    " E ")
(setq evil-normal-state-tag   " N ")
(setq evil-insert-state-tag   " I ")
(setq evil-visual-state-tag   " V ")
(setq evil-replace-state-tag  " R ")
(setq evil-motion-state-tag   " M ")
(setq evil-operator-state-tag " O ")
(setq evil-lisp-state-tag     " L ")

;;
(def-doom-theme moonlight
  "A dark theme inspired by VS code's Moonlight"

  ;; name        default   256       16
  (;; ADJUSTED
   (bg         '("#212337" "#262626" "black"))
   (bg-alt     '("#191a2a" "#1c1c1c" "black"))
   (base0      '("#161a2a" "#1c1c1c" "black"))
   (base1      '("#191a2a" "#1c1c1c" "brightblack"))
   (base2      '("#1e2030" "#262626" "brightblack"))
   (base3      '("#222436" "#262626" "brightblack"))
   (base4      '("#2f334d" "#3a3a3a" "brightblack"))
   (base5      '("#444a73" "#585858" "brightblack"))
   (base6      '("#828bb8" "#8787af" "brightblack"))
   (base7      '("#a9b8e8" "#afafd7" "brightblack"))
   (base8      '("#b4c2f0" "#afafff" "white"))
   (indigo     '("#7a88cf" "#8787d7" "brightblack"))
   (region     '("#383e5c" "#444444" "brightblack"))
   (fg         '("#c8d3f5" "#d7d7ff" "brightwhite"))
   (fg-alt     '("#b4c2f0" "#afafff" "white"))

   (red-region   '("#4e3444" "#5f0000" "brightred"))
   (green-region '("#414a48" "#005f00" "brightgreen"))

   (grey base5)

   (red           '("#ff757f" "#ff8787" "red"))
   (dark-red      '("#ff5370" "#ff5f5f" "red"))
   (light-red     '("#ff98a4" "#ff87af" "brightred"))
   (orange        '("#ff995e" "#ff875f" "brightred"))
   (green         '("#c3e88d" "#afd787" "green"))
   (light-green   '("#c7f59b" "#afff87" "brightgreen"))
   (teal          '("#77e0c6" "#87d7d7" "brightgreen"))
   (dark-teal     '("#4fd6be" "#5fd7af" "green"))
   (light-teal    '("#7af8ca" "#87ffd7" "brightgreen"))
   (yellow        '("#ffc777" "#ffd787" "yellow"))
   (light-yellow  '("#ffdb8e" "ffd7af"  "brightyellow"))

   (blue          '("#82aaff" "#87afff" "blue"))
   (dark-blue     '("#4976eb" "#5f87d7" "blue"))
   (light-blue    '("#50c4fa" "#5fd7ff" "brightblue"))
   (magenta       '("#c099ff" "#af87ff" "magenta"))
   (light-magenta '("#baacff" "#afafff" "brightmagenta"))
   (violet        '("#f989d3" "#ff87d7" "magenta"))
   (pink          '("#f3c1ff" "#ffafff" "magenta"))
   (light-pink    '("#fca7ea" "#ffafd7" "brightmagenta"))
   (cyan          '("#7fdaff" "#afffff" "cyan"))
   (light-cyan    '("#b4f9f8" "#d7ffff" "brightcyan"))
   (dark-cyan     '("#86e1fc" "#87d7ff" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base0)
   (line-highlight base4)
   (selection      region)
   (builtin        magenta)
   (comments       indigo)
   (doc-comments   (doom-lighten comments 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        red)
   (operators      dark-cyan)
   (type           yellow)
   (strings        green)
   (variables      light-red)
   (numbers        orange)
   (region         region)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       teal)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (doom-darken base2 0.1))
   (modeline-bg-alt (doom-darken bg 0.1))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when moonlight-padded-modeline
      (if (integerp moonlight-padded-modeline) moonlight-padded-modeline 4))))

  ;; Base theme face overrides
  ((font-lock-string-face :foreground green :weight 'bold)
   (font-lock-keyword-face :foreground keywords)
   (font-lock-comment-face :foreground comments)
   (font-lock-doc-face :foreground doc-comments)
   (fringe :background base2)
   (hl-line :background line-highlight)
   (lazy-highlight :background base4 :foreground fg)
   ((line-number &override) :foreground base5 :background (doom-darken bg 0.06))
   ((line-number-current-line &override) :foreground fg :background line-highlight)
   ((linum &inherit line-number))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (tooltip :background base0 :foreground fg)

   ;; all-the-icons
   (all-the-icons-red        :foreground red)
   (all-the-icons-red-alt    :foreground red)
   (all-the-icons-lred       :foreground light-red)
   (all-the-icons-dred       :foreground dark-red)
   (all-the-icons-green      :foreground teal)
   (all-the-icons-green-alt  :foreground teal)
   (all-the-icons-lgreen     :foreground green)
   (all-the-icons-dgreen     :foreground dark-teal)
   (all-the-icons-yellow     :foreground yellow)
   (all-the-icons-yellow-alt :foreground yellow)
   (all-the-icons-lyellow    :foreground (doom-lighten yellow 0.3))
   (all-the-icons-dyellow    :foreground orange)
   (all-the-icons-orange     :foreground orange)
   (all-the-icons-orange-alt :foreground orange)
   (all-the-icons-lorange    :foreground orange)
   (all-the-icons-dorange    :foreground orange)
   (all-the-icons-blue       :foreground blue)
   (all-the-icons-blue-alt   :foreground teal)
   (all-the-icons-lblue      :foreground (doom-lighten blue 0.3))
   (all-the-icons-dblue      :foreground (doom-darken blue 0.1))
   (all-the-icons-maroon     :foreground magenta)
   (all-the-icons-maroon-alt :foreground magenta)
   (all-the-icons-lmaroon    :foreground light-magenta)
   (all-the-icons-dmaroon    :foreground magenta)
   (all-the-icons-purple     :foreground magenta)
   (all-the-icons-purple-alt :foreground magenta)
   (all-the-icons-lpurple    :foreground light-magenta)
   (all-the-icons-dpurple    :foreground magenta)
   (all-the-icons-cyan       :foreground dark-cyan)
   (all-the-icons-cyan-alt   :foreground dark-cyan)
   (all-the-icons-lcyan      :foreground (doom-lighten dark-cyan 0.3))
   (all-the-icons-dcyan      :foreground dark-cyan)
   (all-the-icons-pink       :foreground pink)
   (all-the-icons-pink-alt   :foreground pink)
   (all-the-icons-lpink      :foreground light-pink)
   (all-the-icons-dpink      :foreground pink)
   (all-the-icons-silver     :foreground (doom-lighten grey 0.2))
   (all-the-icons-silver-alt :foreground (doom-lighten grey 0.2))
   (all-the-icons-lsilver    :foreground (doom-lighten grey 0.4))
   (all-the-icons-dsilver    :foreground grey)

   ;; all-the-icons-dired
   (all-the-icons-dired-dir-face :foreground indigo)

   ;; company
   (company-tooltip :inherit 'tooltip)
   (company-tooltip-common :foreground highlight)

   ;; company-box
   (company-box-annotation :foreground base7)

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; diredfl
   (diredfl-date-time    :foreground blue)
   (diredfl-file-name    :foreground fg)
   (diredfl-file-suffix  :foreground light-blue)
   (diredfl-symlink      :foreground yellow)

   ;; dired+
   (diredp-number :foreground orange)

   ;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored  :foreground light-cyan)
   (dired-k-added    :foreground vc-added)

   ;; diff
   (diff-refine-added :background (doom-lighten green-region 0.1) :inverse nil)
   (diff-refine-removed :background (doom-lighten red-region 0.1) :inverse nil)

   ;; doom-emacs
   (doom-dashboard-menu-desc :foreground dark-cyan)
   (doom-dashboard-menu-tile :foreground dark-teal)
   (doom-modeline-buffer-file       :foreground base7)
   (doom-modeline-icon-inactive     :foreground indigo)
   (doom-modeline-evil-emacs-state    :inherit 'bold :foreground bg :background blue)
   (doom-modeline-evil-normal-state   :inherit 'bold :foreground bg :background dark-cyan)
   (doom-modeline-evil-insert-state   :inherit 'bold :foreground bg :background green)
   (doom-modeline-evil-visual-state   :inherit 'bold :foreground bg :background yellow)
   (doom-modeline-evil-replace-state  :inherit 'bold :foreground bg :background orange)
   (doom-modeline-evil-motion-state   :inherit 'bold :foreground bg :background violet)
   (doom-modeline-evil-operator-state :inherit 'bold :foreground bg :background teal)
   (doom-modeline-evil-lisp-state     :inherit 'bold :foreground bg :background pink)
   (doom-modeline-project-dir       :foreground light-teal)
   (doom-modeline-buffer-path       :foreground blue)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ;; evil
   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ;; helm
   (helm-ff-dotted-directory :foreground blue :weight 'bold)
   (helm-ff-directory        :foreground light-cyan :weight 'normal)
   (helm-ff-executable       :foreground green :background bg :weight 'normal)
   (helm-ff-prefix           :foreground bg :background yellow :weight 'normal)
   (helm-ff-symlink          :foreground yellow :background bg :weight 'normal)
   (helm-ff-invalid-symlink  :foreground red :background bg :weight 'bold)
   (helm-ff-file             :foreground fg :background bg :weight 'normal)
   (helm-ff-file-extension   :foreground light-blue :background bg :weight 'normal)

   ;; helm-rg
   (helm-rg-base-rg-cmd-face                    :foreground fg      :weight 'normal)
   (helm-rg-extra-arg-face                      :foreground light-cyan    :weight 'normal)
   (helm-rg-active-arg-face                     :foreground green   :weight 'normal)
   (helm-rg-inactive-arg-face                   :foreground fg      :weight 'normal)
   (helm-rg-error-message                       :foreground red     :weight 'bold)
   (helm-rg-directory-header-face               :foreground blue    :weight 'bold)
   (helm-rg-file-match-face                     :foreground blue    :weight 'bold)
   (helm-rg-title-face                          :foreground magenta :weight 'bold)
   (helm-rg-colon-separator-ripgrep-output-face :foreground indigo  :weight 'bold)
   (helm-rg-line-number-match-face              :foreground indigo  :weight 'bold)

   ;; ivy-posframe
   (ivy-posframe :background base0)
   (ivy-posframe-border :background base0)

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground dark-teal)
   (js2-object-property-access :foreground fg-alt)
   (js2-function-param         :foreground pink)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground light-cyan)

   ;; lsp-mode
   (lsp-face-highlight-read :background region)
   (lsp-face-highlight-textual :background region)
   (lsp-face-highlight-write :background region)
   (lsp-face-semhl-type-primative :foreground orange)
   (lsp-face-semhl-method :foreground magenta)

   ;; magit
   (magit-filename :foreground light-cyan)
   (magit-blame-culprit :foreground light-cyan)
   (magit-blame-header :foreground green)
   (magit-blame-sha1 :foreground light-cyan)
   (magit-blame-subject :foreground light-cyan)
   (magit-blame-time :foreground green)
   (magit-blame-name :foreground light-cyan)
   (magit-blame-heading :foreground green)
   (magit-blame-hash :foreground light-cyan)
   (magit-blame-summary :foreground light-cyan)
   (magit-blame-date :foreground green)
   (magit-log-date :foreground fg-alt)
   (magit-log-graph :foreground fg-alt)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-other :foreground yellow)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground yellow)
   (magit-reflog-reset :foreground red)
   (magit-branch :foreground magenta :weight 'bold)
   (magit-branch-current :foreground blue :weight 'bold :box t)
   (magit-branch-local :foreground blue :weight 'bold)
   (magit-branch-remote :foreground light-cyan :weight 'bold)
   (magit-diff-conflict-heading :foreground fg)
   (magit-diff-file-header :foreground yellow)
   (magit-diff-file-heading :foreground blue :weight 'light)
   (magit-diff-file-heading-highlight :foreground blue :weight 'bold)
   (magit-diff-file-heading-selection :foreground blue :weight 'bold :background base1)
   (magit-diff-hunk-heading :foreground yellow :weight 'light)
   (magit-diff-hunk-heading-highlight :foreground yellow :weight 'bold)
   (magit-diff-hunk-heading-selection :inherit 'selection :weight 'bold)
   (magit-diff-added :foreground green :weight 'light)
   (magit-diff-removed :foreground red :weight 'light)
   (magit-diff-context :foreground fg :weight 'light)
   (magit-diff-added-highlight :foreground green :background green-region :weight 'bold)
   (magit-diff-removed-highlight :foreground red :background red-region :weight 'bold)
   (magit-diff-context-highlight :foreground fg :weight 'bold)
   (magit-diff-base :foreground fg :weight 'light)
   (magit-diff-base-highlight :foreground fg :weight 'bold)
   (magit-diff-lines-boundary :background fg :foreground base2)
   (magit-diff-lines-heading :background fg :foreground base2)
   (magit-log-author :foreground light-cyan)
   (magit-log-head-label-head :background light-cyan :foreground bg-alt :weight 'bold)
   (magit-log-head-label-local :background red :foreground bg-alt :weight 'bold)
   (magit-log-head-label-remote :background green :foreground bg-alt :weight 'bold)
   (magit-log-head-label-tags :background magenta :foreground bg-alt :weight 'bold)
   (magit-log-head-label-wip :background yellow :foreground bg-alt :weight 'bold)
   (magit-process-ng :foreground red :weight 'bold)
   (magit-process-ok :foreground light-cyan :weight 'bold)
   (magit-section-heading :foreground magenta)
   (magit-section-highlight :weight 'bold)
   (section-heading-selection :foreground magenta :weight 'bold)
   (magit-section-title :background bg-alt :foreground magenta :weight 'bold)
   (magit-cherry-equivalent :foreground magenta)
   (magit-cherry-unmatched :foreground orange)
   (magit-reflog-checkout :foreground blue)
   (magit-reflog-cherry-pick :foreground green)
   (magit-bisect-bad :foreground red)
   (magit-bisect-good :foreground green)
   (magit-bisect-skip :foreground fg)
   (magit-dimmed :foreground base8)

   ;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)

   ;; markdown-mode
   (markdown-header-face           :inherit 'bold :foreground yellow)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground magenta :inherit 'italic)
   (markdown-list-face             :foreground red)
   (markdown-url-face              :inherit 'underline :foreground orange)
   (markdown-gfm-checkbox-face     :foreground blue)
   (markdown-blockquote-face       :inherit 'italic :foreground fg)
   (mmm-default-submode-face       :background base1)

   ;; message <built-in>
   (message-header-name       :foreground green)
   (message-header-subject    :foreground highlight :weight 'bold)
   (message-header-to         :foreground highlight :weight 'bold)
   (message-header-cc         :inherit 'message-header-to :foreground (doom-darken highlight 0.15))
   (message-header-other      :foreground violet)
   (message-header-newsgroups :foreground yellow)
   (message-header-xheader    :foreground doc-comments)
   (message-separator         :foreground comments)
   (message-mml               :foreground comments :slant 'italic)
   (message-cited-text        :foreground magenta)

   ;; nav-flash
   (nav-flash-face :background region)

   ;; nix-mode
   (nix-attribute-face :foreground blue)
   (nix-builtin-face :foreground dark-teal)

   ;; org <built-in>
   ((outline-1 &override) :foreground light-blue)
   ((outline-2 &override) :foreground dark-cyan)
   ((outline-3 &override) :foreground light-red)
   ((outline-4 &override) :foreground blue)
   ((outline-5 &override) :foreground magenta)
   ((outline-6 &override) :foreground red)
   ((outline-7 &override) :foreground violet)
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)

   ;; popup
   (popup-face :inherit 'tooltip)
   (popup-selection-face :inherit 'tooltip)
   (popup          :inherit 'tooltip)
   (popup-tip-face :inherit 'tooltip)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground light-red)
   (rainbow-delimiters-depth-4-face :foreground light-cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)

   ;; rjsx-mode
   (rjsx-tag :foreground violet)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)

   ;; treemacs
   (treemacs-directory-face :foreground blue)
   (treemacs-git-modified-face :foreground blue)

   ;; workspaces
   (+workspace-tab-selected-face :background region :foreground blue)

   ;; which-key
   (which-func :foreground blue)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground magenta)
   (which-key-local-map-description-face :foreground light-cyan)))

;;; moonlight-theme.el ends here;
