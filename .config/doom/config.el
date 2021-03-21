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
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

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

(custom-set-variables
 '(elfeed-feeds
   (quote
    (("https://www.reddit.com/r/linux/new.rss" linux reddit)
     ("https://www.reddit.com/r/linuxmasterrace/new/.rss" linuxmasterrace reddit)
     ("https://www.reddit.com/r/linuxmemes/new/.rss" linuxmemes reddit)
     ("https://www.reddit.com/r/voidlinux/new/.rss" voidlinux reddit)
     ("https://www.reddit.com/r/suckless/new/.rss" suckless reddit)
     ("https://www.reddit.com/r/emacs/new.rss" emacs reddit)
     ("https://www.reddit.com/r/vim/new.rss" vim reddit)
     ("https://www.reddit.com/r/agraelus/new.rss" agraelus reddit)))))

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
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/my-stuff/org/"
        org-agenda-files '("~/my-stuff/org/agenda/")
        org-ellipsis "  "
        org-log-done 'time
        org-journal-dir "~/my-stuff/org/journal/"
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "PROJ(p)"           ; A project that contains other tasks
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled
(setq alert-default-style 'libnotify)

(setq! global-prettify-symbols-mode 't)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)

(map! :leader
      :desc "Go to emms playlist"
      "a a" #'emms-playlist-mode-go
      :leader
      :desc "Emms pause track"
      "a x" #'emms-pause
      :leader
      :desc "Emms stop track"
      "a s" #'emms-stop
      :leader
      :desc "Emms play previous track"
      "a p" #'emms-previous
      :leader
      :desc "Emms play next track"
      "a n" #'emms-next)

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
      "v c" #'(lambda () (interactive) (find-file "~/.doom.d/config.el"))
      :leader
      :desc "Edit doom init.el"
      "v i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el"))
      :leader
      :desc "Edit doom packages.el"
      "v p" #'(lambda () (interactive) (find-file "~/.doom.d/packages.el")))

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
      "r r" #'lsp-find-references
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
      "r e" #'flycheck-list-errors)
