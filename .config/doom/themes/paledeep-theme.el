;;; paledeep-theme.el --- inspired by Material-PaleNight -*- no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup paledeep-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-paledeep-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'paledeep-theme
  :type '(choice integer boolean))

;;
(def-doom-theme paledeep
  "A dark theme inspired by doom-palenight and doom-challenger-deep"

  ;; name        default   256       16
  ((bg         '("#12111E" nil       nil))
   (bg-alt     '("#1E1C31" nil       nil))
   (base0      '("#1c1f2b" "black"   "black"))
   (base1      '("#1e212e" "#262626" "brightblack"))
   (base2      '("#232635" "#303030" "brightblack"))
   (base3      '("#3C435E" "#3a3a3a" "brightblack"))
   (base4      '("#4E5579" "#444444" "brightblack"))
   (base5      '("#676E95" "#585858" "brightblack"))
   (base6      '("#697098" "#626262" "brightblack"))
   (base7      '("#717CB4" "#767676" "brightblack"))
   (base8      '("#A6Accd" "#a8a8a8" "white"))
   (fg         '("#EEFFFF" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#bcbcbc" "white"))

   (grey base5)

   (red         '("#ff5370" "#ff0000" "red"))
   (orange      '("#f78c6c" "#ff5f00" "brightred"))
   (green       '("#c3e88d" "#afff00" "green"))
   (teal        '("#44b9b1" "#00d7af" "brightgreen"))
   (yellow      '("#ffcb6b" "#ffd700" "brightyellow"))
   (blue        '("#82aaff" "#5fafff" "brightblue"))
   (dark-blue   '("#7986E7" "#d7ffff" "blue"))
   (magenta     '("#c792ea" "#d787d7" "brightmagenta"))
   (violet      '("#bb80b3" "#d787af" "magenta"))
   (cyan        '("#89DDFF" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#80cbc4" "#00d7af" "cyan"))

   ;; face categories -- required for all themes
   (highlight      red)
   (vertical-bar   bg)
   (selection      base4)
   (builtin        blue)
   (comments       base3)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       cyan)
   (methods        blue)
   (operators      cyan)
   (type           magenta)
   (strings        green)
   (variables      yellow)
   (numbers        orange)
   (region         base6)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     bg-alt)
   (modeline-bg-alt bg)
   (modeline-fg     red)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-paledeep-padded-modeline
      (if (integerp doom-paleedeep-padded-modeline) doom-paledeep-padded-modeline 4))))

  ;; --- base faces ------------------------
  (((lazy-highlight &override) :background base4 :foreground fg :distant-foreground fg :bold bold)
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   (mode-line
    :background bg :foreground dark-blue
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)) :height 0.99)

   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)) :height 0.99)

   (solaire-mode-line-face
    :background bg :foreground dark-blue
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)) :height 0.99)
   (solaire-mode-line-inactive-face
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)) :height 0.99)

   (doom-modeline-evil-normal-state :foreground bg)
   (doom-modeline-evil-insert-state :foreground dark-blue)
   (doom-modeline-evil-visual-state :foreground green)
   (doom-modeline-evil-replace-state :foreground red)

   ;; major-mode faces ------------------------
   ;; man-mode
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)

   ;; org-mode
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   (org-document-title :height 1.3 :foreground dark-blue)
   (org-meta-line :height 1.3 :foreground yellow)
   (org-document-info-keyword :inherit 'bold :height 1.4 :foreground yellow)
   (org-date :height 1.2 :foreground yellow)
   (org-tag :height 0.8 :foreground base7)
   (org-todo :height 1.08 :foreground green)
   (org-code :foreground blue)
   (org-done :height 1.08 :foreground (doom-darken blue 0.28))
   (org-checkbox :height 1.23 :foreground dark-blue)


   ;; --- plugin faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground yellow)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; company
   (company-tooltip :foreground fg :background bg-alt)
   (company-tooltip-selection :foreground dark-blue :background (doom-darken bg-alt 0.25))
   (company-tooltip-common :foreground red :inherit 'bold)
   (company-tooltip-mouse :foreground dark-blue :background (doom-darken bg-alt 0.25))

   ;; lsp
   (lsp-ui-sideline-current-symbol :background dark-blue :foreground bg)
   (lsp-ui-sideline-symbol-info :background bg-alt :foreground base5)

   ;; treemacs
   (treemacs-directory-face :foreground dark-blue)
   (treemacs-file-face :foreground red)

   ;; elfeed
   (elfeed-search-title-face :foreground fg)
   (elfeed-search-tag-face :foreground dark-blue)
   (elfeed-search-unread-title-face :foreground red)

   ;; whichkey
   (which-key-key-face :foreground red :inherit 'bold :height 1.1)
   (which-key-group-description-face :foreground dark-blue :inherit 'bold)
   (which-key-command-description-face :foreground fg-alt)
   (which-key-separator-face :foreground magenta)

   ;; nav flash
   (nav-flash-face :background bg :foreground red)

   ;; ace window
   (aw-leading-char-face :foreground red :height 5.0)

   ;; ivy
   (ivy-current-match :background bg-alt :foreground red)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face     :foreground magenta)
   (rainbow-delimiters-depth-2-face     :foreground orange)
   (rainbow-delimiters-depth-3-face     :foreground green)
   (rainbow-delimiters-depth-4-face     :foreground cyan)
   (rainbow-delimiters-depth-5-face     :foreground violet)
   (rainbow-delimiters-depth-6-face     :foreground yellow)
   (rainbow-delimiters-depth-7-face     :foreground blue)
   (rainbow-delimiters-depth-8-face     :foreground teal)
   (rainbow-delimiters-depth-9-face     :foreground dark-cyan)
   (rainbow-delimiters-unmatched-face   :background red :foreground bg)
   (show-paren-match                    :background base7 :foreground red)
   (highlight-parentheses-colors        :background bg :foreground dark-blue)

   ;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)

   ;; tooltip
   (tooltip                  :background (doom-darken bg-alt 0.2) :foreground fg)
   (line-number              :background bg :foreground base3)
   (line-number-current-line :background bg :foreground red)
   (lazy-highlight :background red :foreground bg :inherit 'bold)))

(provide 'paledeep-theme)
