(setq
 comp-deferred-compilation t
 comp-async-report-warnings-errors nil
 comp-always-compile t)

(setq user-full-name "M3dry"
      user-mail-address "m3dry@protonmail.com")

(setq doom-theme 'paledeep)

(setq fringe-mode 8)

(setq display-line-numbers-type 'relative)

(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 16)
      doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font" :size 16)
      doom-big-font (font-spec :family "Mononoki Nerd Font" :size 25))
(after! doom-themes
        (setq doom-themes-enable-bold t
              doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-keyword-face :slant italic)
  '(font-lock-comment-face :slant italic))

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(map! :leader
      :desc "evaluate whole buffer"
      "e b" #'eval-buffer
      :leader
      :desc "evaluate a region"
      "e r" #'eval-region
      :leader
      :desc "evaluate defun"
      "e d" #'eval-defun
      :leader
      :desc "evaluate expression before point"
      "e l" #'eval-last-sexp)

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
(setq dired-open-extensions '(("gif" . "sxiv -a")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv"))
      delete-by-moving-to-trash t)

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :leader
      :desc "Dired jump to current"
      "d j" #'dired-jump
      (:after dired
        (:map dired-mode-map
         :leader
         :desc "Peep-dired image previews"
         "d p" #'peep-dired
         :leader
         :desc "Dired view file"
         "d v" #'dired-view-file)))
(evil-define-key 'normal dired-mode-map
  (kbd "h")   'dired-single-up-directory
  (kbd "l")   'dired-single-buffer
  (kbd "RET") 'dired-open-file
  (kbd "H")   'dired-hide-dotfiles-mode)
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j")   'peep-dired-next-file
  (kbd "k")   'peep-dired-prev-file)

(map! :leader
      :desc "Toggle rainbow mode"
      "t c" #'rainbow-mode)

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.7)

(setq elfeed-feeds '(("https://www.reddit.com/r/linux/new.rss" linux reddit)
                    ("https://www.reddit.com/r/linuxmasterrace/new/.rss" linuxmasterrace reddit memes)
                    ("https://www.reddit.com/r/linuxmemes/new/.rss" linuxmemes reddit memes)
                    ("https://www.reddit.com/r/voidlinux/new/.rss" voidlinux reddit)
                    ("https://www.reddit.com/r/suckless/new/.rss" suckless reddit)
                    ("https://www.reddit.com/r/emacs/new.rss" emacs reddit editor)
                    ("https://www.reddit.com/r/vim/new.rss" vim reddit editor)
                    ("https://www.reddit.com/r/agraelus/new.rss" agraelus reddit)))

(map! :leader
      :desc "Launch elfeed"
      "e f" #'elfeed)

(setq browse-url-browser-function 'eww-browse-url)
(map! :leader
      :desc "Eww web browser"
      "e w" #'eww
      :leader
      :desc "Eww reload page"
      "e R" #'eww-reload
      :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words)

(after! org
  (setq org-directory "~/my-stuff/org/"
        org-agenda-files '("~/my-stuff/org/agenda/")
        org-ellipsis "  "
        org-modules
        '(org-habit
          ol-bibtex)
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-hide-emphasis-markers t
        org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'week)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :todo "TODAY"
                            :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Due Soon"
                             :deadline future
                             :order 8)
                            (:name "Overdue"
                             :deadline past
                             :face error
                             :order 7)
                            (:name "School"
                             :tag  "school"
                             :order 3)
                            (:name "Homework"
                             :tag "homework"
                             :order 7)
                            (:name "Tests"
                             :tag "test"
                             :order 13)
                            (:name "Habits"
                             :habit
                             :order 14)
                            )))))))
        org-capture-templates
        `(("t" "Todos")
          ("tt" "Todo" entry (file+olp "~/my-stuff/org/agenda/Inbox.org" "Todo")
           "* TODO %?\n %U\n %a"))
        org-todo-keywords
        '((sequence
           "TODO(t)"
           "PROJ(p)"
           "|"
           "WAIT(w)"
           "DONE(d)"
           "CANCELLED(c)" ))
        org-tag-alist
        '((:startgroup)
          (:endgroup)
          ("lesson" . ?l)
          ("school" . ?S)
          ("homework" . ?h)
          ("test" . ?t)
          ("english" . ?e)
          ("habits" . ?H))
        org-refile-targets '(("~/my-stuff/org/Archive.org" :maxlevel . 4)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Mononoki Nerd Font" :height (cdr face))))
(defun dw/org-babel-tangle-dont-ask ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dw/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))
(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(map! :leader
      :desc "Eval calculations in org doc"
      "o c" #'literate-calc-eval-buffer
      :leader
      :desc "Eval calculation on selected line"
      "o l" #'literate-calc-eval-line
      :leader
      :desc "Tangle source blocks"
      "m m" #'org-babel-tangle)
(add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)

(setq! global-prettify-symbols-mode 't)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t))
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)

(map! :leader
      :desc "Edit School agenda file"
      "v a s" #'(lambda () (interactive) (find-file "~/my-stuff/org/agenda/School.org"))
      :leader
      :desc "Edit English agenda file"
      "v a e" #'(lambda () (interactive) (find-file "~/my-stuff/org/agenda/English.org"))
      :leader
      :desc "Edit Habits agenda file"
      "v a h" #'(lambda () (interactive) (find-file "~/my-stuff/org/agenda/Habits.org"))
      :leader
      :desc "Edit Inbox agenda file"
      "v a i" #'(lambda () (interactive) (find-file "~/my-stuff/org/Inbox.org"))
      :leader
      :desc "Edit Archive agenda file"
      "v a a" #'(lambda () (interactive) (find-file "~/my-stuff/org/Archive.org"))
      :leader
      :desc "Edit dwm/dwm.c"
      "v d d" #'(lambda () (interactive) (find-file "~/.config/dwm/dwm.c"))
      :leader
      :desc "Edit dwm/dwm.org"
      "v d c" #'(lambda () (interactive) (find-file "~/.config/dwm/dwm.org"))
      :leader
      :desc "Edit doom config.org"
      "v e c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
      :leader
      :desc "Edit doom init.el"
      "v e i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
      :leader
      :desc "Edit doom packages.el"
      "v e p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))
      :leader
      :desc "Edit theme"
      "v e t" #'(lambda () (interactive) (find-file "~/.config/doom/themes/paledeep-theme.el")))

(setq fancy-splash-image "~/.config/doom/doom.png")

(setq
 lsp-idle-delay 0
 lsp-headerline-breadcrumb-enable t
 lsp-completion-show-detail t
 lsp-completion-show-kind t
 lsp-enable-folding t
 lsp-semantic-tokens-mode t
 lsp-enable-semantic-highlighting t
 company-idle-delay 0
 company-tooltip-limit 25
 company-tooltip-idle-delay 0)
(after! lsp-clangd
  (set-lsp-priority! 'clangd 1))

(map! :leader
      :desc "Find definition"
      "r d" #'lsp-find-definition
      :leader
      :desc "Find references"
      "r r" #'lsp-find-references
      :leader
      :desc "Show references in treemacs"
      "r R" #'lsp-treemacs-references
      :leader
      :desc "Find declaration"
      "r D" #'lsp-find-declaration
      :leader
      :desc "Enable code lens"
      "r c" #'lsp-lens-mode
      :leader
      :desc "Enabale breadcrumb"
      "r b" #'lsp-headerline-breadcrumb-mode
      :leader
      :desc "Show error in treemacs"
      "r e" #'lsp-treemacs-errors-list
      :leader
      :desc "Show symbols in treemacs"
      "r s" #'lsp-treemacs-symbols
      :leader
      :desc "Open treemacs"
      "t o" #'treemacs)

(setq which-key-idle-delay 0.5)

(define-key evil-insert-state-map (kbd "C-;") (lambda () (interactive) (up-list)))

(setq
 doom-modeline-height 35
 doom-modeline-bar-width 8
 doom-modeline-major-mode-icon t
 doom-modeline-enable-word-count t)
(setq all-the-icons-scale-factor 0.95)
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq display-time-world-list
  '(("Etc/UTC" "UTC")
    ("Europe/Prague" "Prague")
    ("America/New_York" "New York")
    ("Europe/Athens" "Athens")
    ("Pacific/Auckland" "Auckland")
    ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %R %Z")

(defhydra hydra-resize (:timeout 2)
  ("h" evil-window-decrease-width)
  ("l" evil-window-increase-width)
  ("k" evil-window-decrease-height)
  ("j" evil-window-increase-height))
(map! :leader
      :desc "Resize windows"
      "W" #'hydra-resize/body)

(super-save-mode +1)
(setq super-save-auto-save-when-idle t)

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(map! :desc "Drag selection down"
      "H-s-j" #'drag-stuff-down
      :desc "Drag selection up"
      "H-s-k" #'drag-stuff-up
      :desc "Drag selection left"
      "H-s-h" #'drag-stuff-left
      :desc "Drag selection right"
      "H-s-l" #'drag-stuff-right)

(after! ivy
  (setq
   ivy-initial-inputs-alist
   '((counsel-minor           . "^+")
     (counsel-org-capture     . "^")
     (counsel-M-x             . "^")
     (counsel-describe-symbol . "^")
     (counsel-faces           . "^")
     (org-refile              . "^")
     (org-agenda-refile       . "^")
     (org-capture-refile      . "^")
     (woman                   . "^"))
   ivy-height 25))

(map! :desc "swiper seach"
      "C-/" #'swiper
      :leader
      :desc "switch buffer"
      "<" #'counsel-switch-buffer
      :leader
      :desc "switch buffer"
      "," #'+ivy/switch-buffer
      :leader
      :desc "Search with ripgrep in current directory"
      "s a" #'counsel-rg)

(after! ivy-posframe
  (setq
   ivy-posframe-mode t
   ivy-posframe-border-width 0
   ivy-posframe-font (font-spec :family "Mononoki Nerd Font" :size 17)
   ivy-posframe-width 135
   ivy-posframe-min-width  135
   ivy-posframe-height 25
   ivy-posframe-min-height 25
   ivy-posframe-parameters
   '((left-fringe . 8))
   ivy-posframe-display-functions-alist
   '((t                          . ivy-posframe-display-at-frame-center)
     (counsel-describe-variable  . ivy-posframe-display-at-frame-bottom-left)
     (counsel-describe-function  . ivy-posframe-display-at-frame-bottom-left)
     (counsel-describe-symbol    . ivy-posframe-display-at-frame-bottom-left)
     (counsel-faces              . ivy-posframe-display-at-frame-bottom-left)
     (swiper                     . ivy-posframe-display-at-point)
     (counsel-find-file          . ivy-display-function-fallback))))

(map! :leader
      :desc "Ripgrep on projects"
      "p h" #'counsel-projectile-rg)
