(setq user-full-name "maxminoS")

;; Open new frames in current Emacs
(require 'server)
(if (not (server-running-p)) (server-start))

;; Reload file if edited
(global-auto-revert-mode t)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)

;; Set frame transparency (needs compositor)
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)

;; Set appropriate macOS key modifiers
(setq mac-command-modifier 'meta
      mac-option-modifier 'super)

(when (equal system-type 'darwin)
  (setq trash-directory "~/.Trash"))

(setq user-emacs-directory "~/.emacs.d/cache/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory)
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list" user-emacs-directory) t))
      create-lockfiles nil
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory)
      custom-file (expand-file-name "lisp/custom.el" doom-user-dir))

(setq initial-scratch-message ";; This buffer is for text that is not saved, and for Lisp evaluation.\n;; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n"
      initial-major-mode 'lisp-interaction-mode)

(defvar emax/elfeed-feeds nil)

(load! "lisp/.secret.el")
(add-load-path! "lisp/")

(after! ivy
  (setq ivy-extra-directories '("./"))

  (map! :desc "swiper"
        :g "C-s" #'counsel-grep-or-swiper)

  (map! :desc "swiper-isearch"
        :nv "/" #'swiper-isearch))

(after! counsel
  (setq counsel-find-file-ignore-regexp nil)
  (setq counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s")
  (setq counsel-mode-override-describe-bindings t)

  (map! :leader
        :desc "counsel-switch-buffer"
        "," #'counsel-switch-buffer
        "b b" #'counsel-switch-buffer
        "s s" #'counsel-rg)

  (map! :desc "counsel-switch-buffer"
        :g "C-x b" #'counsel-switch-buffer
        :g "C-x s" #'counsel-rg))

(defun emax/kill-buffer ()
  "Wraps kill-buffer."
  (interactive)
  (ivy-read "Kill buffer: " #'internal-complete-buffer
            :preselect (buffer-name)
            :action #'kill-buffer
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

(after! ivy-rich
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (map! :leader
        :desc "Kill buffer"
        "b k" #'emax/kill-buffer)

  (map! :desc "Kill buffer"
        :g "C-x k" #'emax/kill-buffer))

(column-number-mode)
(setq display-line-numbers-type 'visual)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode -1)
                   (company-mode -1))))

;; Wrap lines
(global-visual-line-mode)

(defvar emax/light-theme 'doom-one-light)
(defvar emax/dark-theme 'doom-one)
(setq doom-theme emax/dark-theme)

(defun emax/set-theme (theme)
  (setq doom-theme theme)
  (load-theme theme t))

(defun emax/toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (if (string= doom-theme emax/dark-theme)
      (emax/set-theme emax/light-theme)
    (emax/set-theme emax/dark-theme)))

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-workspace-name t)
  (add-hook! 'size-indication-mode-hook (setq size-indication-mode nil)))

(after! yascroll
  (global-yascroll-bar-mode 1)
  (setq yascroll:delay-to-hide nil
        yascroll:scroll-bar 'right-fringe))

(after! evil
  (map! :leader
        "x" nil)
  (map! :m "j" #'evil-next-visual-line
        :m "k" #'evil-previous-visual-line
        :n "u" #'undo
        :n "q" nil)) ;; Disable 'q' for macro

(after! evil-collection
  (setq evil-collection-setup-minibuffer t))

(map! :leader
      (:prefix ("<DEL>" . "goto")

       :desc "scratch"
       "<DEL>" #'(lambda () (interactive) (find-file "~/Dropbox/org/scratch.org"))
       :desc "agenda-day"
       "d" #'(lambda () (interactive) (find-file "~/Dropbox/org/agenda/day.org"))
       :desc "agenda-month"
       "m" #'(lambda () (interactive) (find-file "~/Dropbox/org/agenda/month.org"))
       :desc "agenda-year"
       "y" #'(lambda () (interactive) (find-file "~/Dropbox/org/agenda/year.org"))

       :desc "review-film"
       "F" #'(lambda () (interactive) (find-file "~/Dropbox/org/reviews/film.org"))
       :desc "review-show"
       "S" #'(lambda () (interactive) (find-file "~/Dropbox/org/reviews/show.org"))
       :desc "review-music"
       "M" #'(lambda () (interactive) (find-file "~/Dropbox/org/reviews/music.org"))
       :desc "review-book"
       "B" #'(lambda () (interactive) (find-file "~/Dropbox/org/reviews/book.org"))

       :desc "notes"
       "g" #'(lambda () (interactive) (counsel-find-file "~/Dropbox/org/notes/"))
       :desc "notes-programming"
       "p" #'(lambda () (interactive) (counsel-find-file "~/Dropbox/org/notes/programming/"))
       :desc "notes-recreation"
       "r" #'(lambda () (interactive) (counsel-find-file "~/Dropbox/org/notes/recreation/"))
       :desc "notes-technology"
       "t" #'(lambda () (interactive) (counsel-find-file "~/Dropbox/org/notes/technology/"))

       :desc "bookmarks"
       "b" #'(lambda () (interactive) (find-file "~/Dropbox/org/notes/others/bookmarks.org"))
       :desc "essays"
       "e" #'(lambda () (interactive) (find-file "~/Dropbox/org/notes/others/essays.org"))
       :desc "ideas"
       "i" #'(lambda () (interactive) (find-file "~/Dropbox/org/notes/others/ideas.org"))
       :desc "lists"
       "l" #'(lambda () (interactive) (find-file "~/Dropbox/org/notes/others/lists.org"))))

(map! :leader
      (:prefix ("a" . "pass")

       :desc "Insert pass"
       "i" #'password-store-insert
       :desc "Insert OTP"
       "I" #'emax/password-store-otp-insert-code
       :desc "Generate pass"
       "g" #'password-store-generate

       :desc "Copy pass"
       "c" #'password-store-copy
       :desc "Copy field"
       "C" #'password-store-copy-field
       :desc "Copy OTP"
       "o" #'password-store-otp-token-copy

       :desc "Edit pass"
       "e" #'password-store-edit
       :desc "Rename pass"
       "r" #'password-store-rename
       :desc "Remove pass"
       "x" #'password-store-remove))

(after! dired
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)))
  (setq dired-recursive-deletes 'always)

  (map! :leader :desc "Dired Jump" :n "d" #'dired-jump)
  (map! :desc "Dired Jump" :g "C-x d" #'dired-jump)

  (map! (:map dired-mode-map
         :n "N" #'mkdir
         :n "," #'dired-hide-details-mode
         :n "H" #'dired-hide-dotfiles-mode
         :n "h" #'dired-single-up-directory
         :n "l" #'dired-single-buffer
         :n "<tab>" #'dired-subtree-toggle
         :n "<backtab>" #'dired-subtree-cycle
         :n "p" #'peep-dired)))

(after! org
  ;; Replace dashes to bullet
  (font-lock-add-keywords 'org-mode
     '(("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Resize headlines
  (set-face-attribute 'org-level-1 nil :height 1.25)
  (set-face-attribute 'org-level-2 nil :height 1.15)
  (set-face-attribute 'org-level-3 nil :height 1.12)
  (set-face-attribute 'org-level-4 nil :height 1.1)
  (set-face-attribute 'org-level-5 nil :height 1.05)
  (set-face-attribute 'org-level-6 nil :height 1.05)

  (setq org-ellipsis " ▾"
        org-todo-keywords '((sequence "TODO(t)" "DOING(d)" "|" "DONE(x)")
                            (sequence "WAITING(w)" "|" "CANCELED(c)"))
        org-agenda-span 'week
        org-directory "~/Dropbox/org"
        org-default-notes-file "~/Dropbox/org/scratch.org"
        org-agenda-files '("~/Dropbox/org/agenda")
        org-refile-targets '(("~/Dropbox/org/archive.org" :maxlevel . 1))))

(after! org
  (setq org-capture-templates
          `(("c" "Scratch" item (file+headline ,(concat org-directory "/scratch.org") "Untracked")
                  "- %?")
            ("i" "Idea" entry (file ,(concat org-directory "/notes/others/ideas.org"))
                  "* %?" :empty-lines 1)

            ("t" "Task" entry (file+headline ,(concat org-directory "/agenda/tasks.org") "Task Manager")
                  "** TODO %?\n   SCHEDULED: %t" :kill-buffer t)
            ("d" "Deadline" entry (file+headline ,(concat org-directory "/agenda/tasks.org") "Task Manager")
                  "** TODO %?\n   DEADLINE: %^t" :kill-buffer t)

            ("e" "Essay" entry (file ,(concat org-directory "/notes/others/essays.org"))
                  "* %? %^g\n %u" :empty-lines 1 :jump-to-captured t)

            ("r" "Review")
            ("rf" "Film" entry (file ,(concat org-directory "/reviews/film.org"))
                  "* %^{Film Title} (%^{Year Released}) %^g\n%?" :empty-lines 1 :jump-to-captured t)
            ("ra" "Album" plain (file+function ,(concat org-directory "/reviews/music.org") (lambda () (emax/org-capture-existing-heading "Artist")))
                  "** %^{Album Title} %^g\n\n*** %? %^g" :jump-to-captured t)
            ("rb" "Book" entry (file ,(concat org-directory "/reviews/book.org"))
                  "* %^{Book Title} - %^{Author} %^g\n** Chapter 1\n** Review\n%?" :empty-lines 1 :jump-to-captured t)
            ("rs" "Show" entry (file ,(concat org-directory "/reviews/show.org"))
                  "* %^{Show Title} (YYYY)-(YYYY) %^g\n** Season 1\n** Review\n%?" :empty-lines 1 :jump-to-captured t)

            ("j" "Journal")
            ("jd" "Today" plain (file+olp+datetree ,(concat org-directory "/agenda/day.org"))
                  "%?" :tree-type month :kill-buffer t :unnarrowed t)
            ("jm" "This Month" plain (file+function ,(concat org-directory "/agenda/month.org") (lambda () (emax/org-datetree-find-date-create t)))
                  "" :kill-buffer t :unnarrowed t)
            ("jy" "This Year" plain (file+function ,(concat org-directory "/agenda/year.org") (lambda () (emax/org-datetree-find-date-create)))
                  "" :kill-buffer t :unnarrowed t)

            ("l" "Link")))

  (dolist (bookmarks '("Articles" "Blogs" "Entertainment"
                       "Resources" "Social" "Technology"
                       "Videos" "Others"))
       (add-to-list 'org-capture-templates
                   `(,(concat "l" (downcase (substring bookmarks 0 1))) ,bookmarks item (file+headline ,(concat org-directory "/notes/others/bookmarks.org") ,bookmarks)
                          "- [[https://%^{Link}][%^{Name}]]" :kill-buffer t) t)))

(after! forge
  (add-to-list 'auth-sources `(,(expand-file-name "lisp/.authinfo.gpg" doom-user-dir))))

(after! windmove
  (map! :leader
        "w <left>" #'windmove-swap-states-left
        "w <down>" #'windmove-swap-states-down
        "w <up>" #'windmove-swap-states-up
        "w <right>" #'windmove-swap-states-right))

(defvar emax/monocle-windows nil)
(defun emax/toggle-monocle ()
  "Monocle window mode"
  (interactive)
  (if (one-window-p)
      (when emax/monocle-windows
        (set-window-configuration emax/monocle-windows))
    (setq emax/monocle-windows (current-window-configuration))
    (delete-other-windows)))

(map! :leader
      :desc "toggle-monocle"
      "w RET" #'emax/toggle-monocle)

(after! ibuffer
  (map! "C-x C-b" #'ibuffer
        (:map ibuffer-mode-map
         :n "j" #'ibuffer-forward-line
         :n "k" #'ibuffer-backward-line
         :n "h" #'evil-backward-WORD-begin
         :n "l" #'evil-forward-WORD-begin))


  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 22 22 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 12 12 :left :elide) " "
                filename-and-process)
          (mark " " (name 16 16) "   " (mode 8 8))))

  (setq ibuffer-filter-group-name-face 'font-lock-doc-face)


  (setq ibuffer-saved-filter-groups
        '(("Default"
           ("Agenda" (and
                      (filename . "agenda")
                      (filename . ".org")))
           ("Emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (filename . ".emacs.d")))
           ("Document" (or
                        (mode . org-mode)
                        (mode . nov-mode)
                        (mode . doc-view-mode)))
           ("Compile" (or
                       (mode . eshell-mode)
                       (mode . shell-mode)
                       (mode . term-mode)))
           ("Dired" (mode . dired-mode))
           ("Magit" (name . "^magit"))
           ("Help" (or
                    (name . "^*\\(.*\\)*$")
                    (mode . help-mode)
                    (mode . helpful-mode)
                    (mode . Info-mode)
                    (mode . apropos-mode)
                    (mode . debugger-mode)
                    (mode . Man-mode))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode)
              (ibuffer-switch-to-saved-filter-groups "Default"))))

(setq lsp-use-plists t)
(after! lsp-mode
  (add-hook 'lsp-mode-hook (lambda ()
                              (lsp-ui-mode)
                              (lsp-headerline-breadcrumb-mode 1)
                              (company-mode)))

  (set-face-background 'lsp-face-highlight-textual "#1f1147")
  (setq lsp-before-save-edits nil
        lsp-enable-snippet nil
        lsp-restart 'auto-restart
        lsp-eslint-auto-fix-on-save t))

(after! lsp-ui
  (setq lsp-ui-doc-frame-parameters '((left . -3)
                                      (no-focus-on-map . t)
                                      (min-width . 15)
                                      (width . 0)
                                      (min-height . 0)
                                      (height . 0)
                                      (internal-border-width . 1)
                                      (vertical-scroll-bars . nil)
                                      (horizontal-scroll-bars . nil)
                                      (right-fringe . 0)
                                      (menu-bar-lines . 0)
                                      (tool-bar-lines . 0)
                                      (line-spacing . 0)
                                      (unsplittable . t)
                                      (undecorated . t)
                                      (top . -1)
                                      (visibility . nil)
                                      (mouse-wheel-frame . nil)
                                      (no-other-frame . t)
                                      (inhibit-double-buffering . t)
                                      (drag-internal-border . t)
                                      (no-special-glyphs . t)
                                      (alpha . (100 . 100))
                                      (desktop-dont-save . t)))

  ;; Make UI doc and peek more readable
  (set-face-background 'markdown-code-face "#1f1147")
                                        ;(set-face-foreground 'lsp-ui-peek-highlight "#cd00cd")
                                        ;(set-face-background 'lsp-ui-peek-filename "#cd00cd")
                                        ;(set-face-background 'lsp-ui-peek-selection "#8b008b")

  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-delay 0.5))

(after! company-mode
  (global-company-mode -1)
  (setq custom-idle-delay 0.0)
)

(setq tab-always-indent t)

(map! :leader
      :desc "Tab"
      "i t" #'(lambda () (interactive) (insert "\t")))

(defun emax/save-without-formatting ()
  (interactive)
  (if (equal format-all-mode t)
    (progn (format-all-mode -1)
           (save-buffer)
           (format-all-mode 1))
    (save-buffer)))

(map! :leader
      :desc "Save without formatting"
      "f S" #'emax/save-without-formatting)

(after! smartparens
  (show-paren-mode t)
  (smartparens-global-mode t)
  (dolist (mode '(text-mode-hook
                  prog-mode-hook))
    (add-hook! mode (lambda () (smartparens-mode))))

  (map! :leader
        (:map smartparens-mode-map
         (:prefix ("z" . "smartparens")
         ;; Wrap
          "(" #'sp-wrap-round  ; ")"
         "[" #'sp-wrap-square ; "]""
         "{" #'sp-wrap-curly ; "}"
         :desc "Wrap quotes"
         "\"" #'(lambda (&optional args) (interactive "P") (sp-wrap-with-pair "\""))
         :desc "Wrap *"
         "*" #'(lambda (&optional args) (interactive "P") (sp-wrap-with-pair "*"))
         :desc "Wrap /"
         "/" #'(lambda (&optional args) (interactive "P") (sp-wrap-with-pair "/"))
         :desc "Wrap _"
         "_" #'(lambda (&optional args) (interactive "P") (sp-wrap-with-pair "_"))
         :desc "Wrap ="
         "=" #'(lambda (&optional args) (interactive "P") (sp-wrap-with-pair "="))
         :desc "Wrap ~"
         "~" #'(lambda (&optional args) (interactive "P") (sp-wrap-with-pair "~"))
         ;; Unwrap
         "<" #'sp-backward-unwrap-sexp
         ">" #'sp-unwrap-sexp
         ;; Slurp / Barf
         "h" #'sp-backward-slurp-sexp
         "l" #'sp-forward-slurp-sexp
         "<left>" #'sp-forward-barf-sexp
         "<right>" #'sp-backward-barf-sexp
         ;; Swap
         "z" #'sp-transpose-sexp))))

(map! :g "M-;" #'comment-line)

(after! password-store
  (setq password-store-password-length 16))

(after! password-store-otp
   (defun emax/password-store-otp-insert-code" (entry issuer email otp-code)
     Insert a new ENTRY with OTP-URI generated using the enterred ISSUER, EMAIL, and CODE."
     (interactive (list (password-store-otp-completing-read)
                        (read-string "Issuer: ")
                        (read-string "Email: ")
                        (read-passwd "Code: " t)))
     (password-store-otp-add-uri 'insert entry
                                 (concat "otpauth://totp/" issuer ":" email "?secret=" otp-code "&issuer=" issuer))))

(after! elfeed
  (setq elfeed-search-filter "@3-months-ago +unread ")
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-feeds emax/elfeed-feeds)

  (map! (:map elfeed-search-mode-map
         :n "g r" #'elfeed-update)))

(setq display-time-world-list
  '(("" "*AMERICA*")
    ("America/Los_Angeles" "Los Angeles (PT)")
    ("America/New_York" "New York (ET)")
    ("America/Chicago" "Chicago (CT)")
    ("" "")
    ("" "*EUROPE*")
    ("Europe/London" "London (GMT)")
    ("Europe/Berlin" "Germany (GMT+1)")
    ("Europe/Athens" "Greece (GMT+3)")
    ("" "")
    ("" "*ASIA*")
    ("Asia/Jakarta" "Jakarta (GMT+7)")
    ("Asia/Singapore" "Singapore (GMT+8)")))

(setq display-time-world-time-format "- %I:%M%p - %a, %d %b")
