(setq
 comp-deferred-compilation t
 comp-async-report-warnings-errors nil
 comp-always-compile t)

(setq doom-font (font-spec :family "ComicCodeLigatures Nerd Font" :size 13)
      doom-big-font (font-spec :family "ComicCodeLigatures Nerd Font" :size 16)
      doom-serif-font (font-spec :family "ComicCodeLigatures Nerd Font" :size 13)
      doom-unicode-font (font-spec :family "DejaVu Sans"))

(setq doom-theme 'doom-palenight)
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
  '(org-level-6 ((t (:inherit outline-6 :height 1.0)))))

(custom-set-faces!
  '(font-lock-comment-face :inherit 'italic)
  '(font-lock-keyword-face :inherit 'italic)
  '(font-lock-type-face :inherit 'bold)
  '(org-document-title :foreground "#ffcb6b" :height 1.7 :inherit 'italic)
  '(org-document-info-keyword :foreground "#c792ea" :height 1.4)
  '(org-document-info :height 1.5 :inherit 'italic)
  '(org-date :foreground "#ffcb6b" :height 1.2 :inherit 'italic)
  '(org-block-begin-line :foreground "#82aaff" :background "#232635" :height 1.1 :inherit 'italic :extend t)
  '(org-block-end-line :foreground "#82aaff" :background nil :height 1.1 :inherit 'italic)
  '(org-tag :foreground "#8d92af" :height 0.7)
  '(calendar-weekday-header :foreground "#c792ea"))

(defun cp/company-number ()
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))

(after! company-box
  (setq company-idle-delay 0.3
        company-tooltip-limit 25
        company-tooltip-idle-delay 0.5
        company-minimum-prefix-length 2)

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'cp/company-number))
     (number-sequence 0 9))
  (define-key map " " (lambda ()
                        (interactive)
                        (company-abort)
                        (self-insert-command 1)))
  (define-key map (kbd "TAB") #'yas-expand)
  (define-key map (kbd "S-TAB") #'yas-expand)))

(map! :leader
      "f p" #'(lambda () (interactive) (doom-project-find-file "~/.config/flake/dots/doom")))

(setq eros-eval-result-prefix "==> ")

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

(setq display-line-numbers-type 'visual)

(map! :mnv "k"   #'evil-previous-visual-line
      :mnv "j"   #'evil-next-visual-line
      :mnv "gk"  #'evil-previous-line
      :mnv "gj"  #'evil-next-line
      :mnv "h"   #'evil-backward-char
      :mnv "l"   #'evil-forward-char)

(after! evil (setq evil-ex-substitute-global t))

(set-default 'truncate-lines t)

(setq
 truncate-lines t
 truncate-partial-width-windows t)

(setq undo-limit 80000000)

(map! :i "C-p" #'up-list)

(after! evil-escape
  (setq evil-escape-excluded-major-modes nil
        evil-escape-excluded-states nil
        evil-escape-inhibit-functions nil
        evil-escape-key-sequence nil))

(setq sentence-end-double-space nil)

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)

(setq
 evil-vsplit-window-right t
 evil-split-window-below t
 window-divider-default-bottom-width 0
 window-divider-default-right-width 0)
(set-fringe-mode 0)

(set-fringe-mode 0)

(set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode))

(use-package! smart-semicolon
  :defer t
  :hook (c-mode . smart-semicolon-mode)
  :config
  (setq smart-semicolon-block-chars '(32 59)))

(map! :leader
      "," #'consult-buffer
      "<" #'consult-buffer-other-window)

(use-package! affe
  :after orderless
  :config
  (consult-customize
   affe-grep
   :prompt "Search in Project  ")
  (consult-customize
   affe-find
   :prompt "Find file in Project  "))

(defhydra hd-consult (:exit t
                      :hint nil)
"

 _i_: consult imenu   _a_: consult org agenda   _b_: consult buffer other window
 _t_: consult theme   _d_: consult ma           _B_: consult buffer other frame
                    _f_: consult set font     _m_: consult minor mode menu
                    _h_: affe grep
                    _j_: consul org heading
                    _k_: consul buffer
                    _l_: consult line
                    _;_: affe find
                    _'_: consult find
--------------------------------------------------------------------------------------
 _q_: quit
"
  ("i" consult-imenu)
  ("t" consult-theme)
  ("a" consult-org-agenda)
  ("d" consult-man)
  ("f" consult-set-font)
  ("h" affe-grep)
  ("j" consult-org-heading)
  ("k" consult-buffer)
  ("l" consult-line)
  (";" affe-find)
  ("'" consult-find)
  ("m" consult-minor-mode-menu)
  ("b" consult-buffer-other-window)
  ("B" consult-buffer-other-frame)
  ("q" nil))

(defhydra hd-splits (:timeout 2
                     :hint nil)
"
 ^Windows^
---------------------
 _h_: decrease width
 _k_: decrease height
 _j_: increase height
 _l_: increase width
 _=_: balance windows
---------------------
 _q_: quit
"
  ("h" evil-window-decrease-width)
  ("l" evil-window-increase-width)
  ("k" evil-window-decrease-height)
  ("j" evil-window-increase-height)
  ("=" balance-windows)
  ("q" nil))

(map! :leader
      :desc "Consult functions"
      "k" #'hd-consult/body
      :leader
      :desc "Resize windows"
      "j" #'hd-splits/body)
(map! :desc "Ripgrep on projects"
      "C-;" #'+default/search-project)

(map! :leader
      :desc "Toggle rainbow mode"
      "t c" #'rainbow-mode)

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(setq
 doom-modeline-height 23
 doom-modeline-bar-width 3
 doom-modeline-major-mode-icon t
 doom-modeline-enable-word-count t
 doom-modeline-buffer-file-name-style 'truncate-except-project
 all-the-icons-scale-factor 1)
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq calendar-date-style "european"
      calendar-day-abbrev-array '["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"])

(set-popup-rule! "^\\*vterm" :size 0.20 :vslot -4 :select t :quit nil :ttl 0)

(setq
 org-agenda-files '("~/my-stuff/Org/Agenda/")
 org-agenda-property-position 'next-line
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-include-deadlines t
 org-agenda-start-with-log-mode t
 org-agenda-align-tags-to-column 48
 org-agenda-time-leading-zero t
 org-agenda-skip-timestamp-if-done t
 org-agenda-custom-commands
 '(("o" "Overview"
    ((agenda "" (
                 (org-agenda-prefix-format " %?-12t% s")
                 (org-agenda-span 'week)
                 (org-agenda-start-day "-1d")
                 (org-agenda-overriding-header "⚡ This week")
                 (org-agenda-current-time-string "<----------- Now")
                 (org-agenda-scheduled-leaders '("SCHEDULED: " "Scheduled: "))
                 (org-agenda-deadline-leaders '("DEADLINE: " "Deadline: "))
                 (org-agenda-sorting-strategy '(priority-up))))
     (todo "" (
               (org-agenda-overriding-header "\n⚡ Today")
               (org-agenda-skip-timestamp-if-done t)
               (org-agenda-prefix-format " %?-12t% s")
               (org-agenda-span 'day)
               (org-agenda-start-day "+0d")
               (org-agenda-sorting-strategy '(priority-up))))
     (tags-todo "+PRIORITY=\"A\"" (
                                   (org-agenda-overriding-header "\n⚡ High priority")
                                   (org-agenda-skip-timestamp-if-done t)
                                   (org-agenda-prefix-format " %?-12t% s")))
     (tags-todo "+Effort<=20&+Effort>0" ()
                                        (org-agenda-overriding-header "\n⚡ Low effort")
                                        (org-agenda-skip-timestamp-if-done t)
                                        (org-agenda-prefix-format " %?-12t% s")
                                        (org-agenda-sorting-strategy '(priority-up))
                                        (org-agenda-max-todos 10))
     (todo "TODO" (
                   (org-agenda-overriding-header "\n⚡ To Do")
                   (org-agenda-skip-timestamp-if-done t)
                   (org-agenda-prefix-format " %?-12t% s")
                   (org-agenda-sorting-strategy '(priority-up))))
     (todo "PROJ" (
                   (org-agenda-overriding-header "\n⚡ Projects")
                   (org-agenda-skip-timestamp-if-done t)
                   (org-agenda-prefix-format " %?-12t% s")
                   (org-agenda-sorting-strategy '(priority-up))))))))

(setq
 org-roam-directory "~/my-stuff/Org/Roam/"
 org-roam-completion-everywhere t
 +org-roam-open-buffer-on-find-file nil)

(setq org-roam-dailies-directory "Journal/")

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
         :unnarrowed t))
      org-roam-dailies-capture-templates
      '(("t" "todo" entry "* TASKS\n- [ ] %?" :empty-lines 1 :target
          (file+head "%<%Y-%m-%d>.org" "#+title: Journal-%<%Y-%m-%d>\n\n#+filetags: Journal\n\n* Gratitude"))
        ("d" "date" entry "* %<%H:%M>: %?" :empty-lines 1 :target
          (file+head "%<%Y-%m-%d>.org" "#+title: Journal-%<%Y-%m-%d>\n#+filetags: Journal\n\n* Gratitude"))
        ("n" "no date" entry "* %?" :empty-lines 1 :target
          (file+head "%<%Y-%m-%d>.org" "#+title: Journal-%<%Y-%m-%d>\n\n#+filetags: Journal\n\n* Gratitude"))
        ("N" "nothing" entry "%?" :empty-lines 1 :target
          (file+head "%<%Y-%m-%d>.org" "#+title: Journal-%<%Y-%m-%d>\n\n#+filetags: Journal\n\n* Gratitude"))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(map! :i "C-c n i" #'org-roam-node-insert
      :i "C-c n I" #'org-roam-node-insert-immediate)

(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  (setq org-startup-with-latex-preview t))

(setq
 org-priority-lowest ?D
 org-priority-highest ?A
 org-priority-faces
 '((?A . error)
   (?B . warning)
   (?C . success)
   (?D . outline-4)))

(after! org
  (setq
   org-todo-keywords
   '((sequence
      "TODO(t)"
      "NEXT(n)"
      "PROJ(p)"
      "|"
      "WAIT(w)"
      "DONE(d)"
      "CANCELLED(c)"))
   org-todo-keyword-faces
   '(("TODO"      . (:foreground "#f78c6c" :inherit 'bold))
     ("NEXT"      . (:foreground "#ff5370" :inherit 'bold :height 1.15))
     ("PROJ"      . (:foreground "#5fafff" :inherit 'bold))
     ("WAIT"      . (:foreground "#eeffff" :inherit 'bold))
     ("DONE"      . (:foreground "#c3e88d" :inherit 'bold :box "#c3e88d"))
     ("CANCELLED" . (:foreground "#717cb4" :inherit 'bold :strike-through t)))))

(after! org
  (setq org-capture-templates
        `(("t" "Todo")
          ("ti" "Important" entry (file+olp "~/my-stuff/Org/Agenda/Inbox.org" "Important")
           "* TODO %?\n%U")
          ("tt" "Today" entry (file+olp "~/my-stuff/Org/Agenda/Inbox.org" "Today")
           "* TODO %?\n%U")
          ("tl" "Later" entry (file+olp "~/my-stuff/Org/Agenda/Inbox.org" "Later")
           "* TODO %?\n%U"))))

(after! org-superstar
  (setq
   org-superstar-headline-bullets-list '("⬢" "⬡" "◆" "◈" "◇" "●" "◉" "○" "✹" "✿" "✤" "✜")
   org-superstar-remove-leading-stars t
   org-superstar-special-todo-items t
   org-superstar-todo-bullet-alist
   '(("TODO"      . ?)
     ("NEXT"      . ?)
     ("PROJ"      . ?)
     ("WAIT"      . ?)
     ("CANCELLED" . ?)
     ("DONE"      . ?))
   org-superstar-item-bullet-alist
   '((?- . ?•)
     (?+ . ?➤))))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoemphasis t
        org-appear-autoentities t)
  (add-hook! 'org-appear-mode-hook
    ;; for proper first-time setup, `org-appear--set-elements' needs to
    ;; be run after other hooks have acted.
    (org-appear--set-elements)
    (add-hook! evil-insert-state-entry :local (org-appear-mode 1))
    (add-hook! evil-insert-state-exit :local (org-appear-mode -1))))

(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "")
                                    (?B . "")
                                    (?C . "")
                                    (?D . ""))))

(setq
 org-ellipsis " ⬎ "
 org-list-allow-alphabetical t
 org-hide-emphasis-markers t)

(use-package! org-fragtog :hook (org-mode . org-fragtog-mode))

(add-hook 'org-mode-hook #'org-inline-pdf-mode)

(add-hook 'org-mode-hook #'org-inline-anim-mode)

(use-package! org-autolist
  :defer t
  :hook (org-mode . org-autolist-mode))

(after! org
  (setq
   org-startup-folded 'content
   org-startup-with-inline-images t))

(toc-org-mode)
(setq toc-org-max-depth 4)

(setq org-export-with-tags nil)

(after! org
  (setq org-refile-targets '(("~/my-stuff/Org/Archive.org" :maxlevel . 4))))
(advice-add 'org-refile :after 'org-save-all-org-buffers)
