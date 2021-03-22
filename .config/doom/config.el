(setq user-full-name "M3dry"
      user-mail-address "m3dry@protonmail.com")

(setq doom-theme 'doom-one)

(setq display-line-numbers-type t)

(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font" :size 15)
      doom-big-font (font-spec :family "Mononoki Nerd Font" :size 18))
(after! doom-themes
        (setq doom-themes-enable-bold t
              doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-keyword-face :slant italic))
  '(font-lock-comment-face :slant italic)

(add-to-list 'default-frame-alist '(alpha 95 95))

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
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(map! :leader
      :desc "Comment or uncomment lines"
      "g c c" #'comment-line
      :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines
      :leader
      :desc "Toggle rainbow mode"
      "t c" #'rainbow-mode)

(after! mastodon
  (setq mastodon-instance-url "https://fosstodon.org/"))

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
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-hide-emphasis-markers t
        org-link-abbrev-alist
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords
        '((sequence
           "TODO(t)"
           "TEST(T)"
           "HOMEWORK(h)"
           "PROJ(p)"
           "|"
           "WAIT(w)"
           "DONE(d)"
           "CANCELLED(c)" ))
        org-tag-alist
        '((:startgroup)
          (:endgroup)
          ("schedule" . ?s)
          ("school" . ?S)
          ("english" . ?e))
        org-agenda-custom-commands
        '(("T" "Next Tests"
           ((todo "TEST"
                  ((org-agenda-overriding-header "Next Tests")))))
          ("E" Low Effort "+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files))))
        org-refile-targets '(("~/my-stuff/org/agenda/Archive.org" :maxlevel . 4))))
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(map! :leader
      :desc "eval calculations in org doc"
      "o c" #'literate-calc-eval-buffer
      :leader
      :desc "Eval calculation on selected line"
      "o l" #'literate-calc-eval-line)

(setq! global-prettify-symbols-mode 't)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)

(map! :leader
      :desc "Edit school agenda file"
      "v a s" #'(lambda () (interactive) (find-file "~/my-stuff/org/agenda/school.org"))
      :leader
      :desc "Edit dwm/dwm.c"
      "v d d" #'(lambda () (interactive) (find-file "~/.config/dwm/dwm.c"))
      :leader
      :desc "Edit dwm/config.def.h"
      "v d c" #'(lambda () (interactive) (find-file "~/.config/dwm/config.def.h"))
      :leader
      :desc "Edit doom config.el"
      "v c" #'(lambda () (interactive) (find-file "~/.config/doom/config.el"))
      :leader
      :desc "Edit doom init.el"
      "v i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
      :leader
      :desc "Edit doom packages.el"
      "v p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el")))

(setq fancy-splash-image "~/my-stuff/Pictures/doom.png")
(setq lsp-idle-delay 0.1)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
(setq lsp-ui-doc-enable t)

(map! :leader
      :desc "Find definition"
      "r d" #'lsp-find-definition
      :leader
      :desc "Find references"
      "r R" #'lsp-find-references
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
      :desc "Show error"
      "r e" #'lsp-treemacs-errors-list
      :leader
      :desc "Show references"
      "r r" #'lsp-treemacs-references
      :leader
      :desc "Show symbols"
      "r s" #'lsp-treemacs-symbols)
