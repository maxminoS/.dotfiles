(setq user-full-name "maxminoS")

;; Ensures using UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Sets default directory
(setq default-directory "~/")

;; Spaces over tabs
(setq-default tab-width 2
  indent-tabs-mode nil)

;; Clean whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Open new frames in current Emacs
(require 'server)
(if (not (server-running-p)) (server-start))

;; Reload file if edited
(global-auto-revert-mode t)

;; Delete selection
(delete-selection-mode t)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(setq use-package-always-ensure t)

(setq gc-cons-threshold 20000000)

(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold 1000000)))

(setq user-emacs-directory "~/.emacs.d/cache/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory)
      byte-compile-warnings '(cl-functions)
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list" user-emacs-directory) t))
      create-lockfiles nil
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory)
      custom-file "~/.emacs.d/lisp/custom.el")

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  (add-hook 'after-init-hook
          (lambda () (message "Loaded in %s" (emacs-init-time)))))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Automatically tangle emacs.org on save
(defun org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.emacs.d/"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

(use-package ivy
  :diminish ivy-mode
  :bind (("C-s" . counsel-grep-or-swiper))
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :diminish counsel-mode
  :bind (("M-x" . counsel-M-x)
          ("C-x b" . 'counsel-switch-buffer))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1)
  (setq-default counsel-mode-override-describe-bindings t))

(use-package ivy-rich
  :diminish ivy-rich-mode
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(defun emax/kill-buffer ()
  "Wraps kill-buffer."
  (interactive)
  (ivy-read "Kill buffer: " #'internal-complete-buffer
            :preselect (buffer-name)
            :action #'kill-buffer
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

(define-key global-map [remap kill-buffer] 'emax/kill-buffer)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(setq inhibit-startup-message t)

(scroll-bar-mode 0)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 2)
(menu-bar-mode -1)
(setq visible-bell nil)

(column-number-mode)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
                 (display-line-numbers-mode 1)
                 (setq display-line-numbers 'relative))))

(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Wrap lines
(global-visual-line-mode 1)

;;Make yes or no prompts y or n
(fset 'yes-or-no-p 'y-or-n-p)

(use-package doom-themes
  :init (load-theme 'doom-outrun-electric t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (display-time-default-load-average nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-workspace-name nil)
  ;; (doom-modeline-mu4e t) ; Requires mu4e-alert
  :config
  (display-battery-mode t)
  (display-time-mode t))

(use-package yascroll
  :config
  (require 'cl)
  (global-yascroll-bar-mode 1)
  :custom
  (yascroll:delay-to-hide nil))

(use-package visual-fill-column
  :config
  (defun emax/visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-shift-width 2)
  :bind
  (:map evil-normal-state-map
    ("q" . nil)) ;; Disable 'q' for macro
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
          (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map "\M-h" 'org-agenda-earlier)
  (evil-define-key 'motion org-agenda-mode-map "\M-l" 'org-agenda-later)
  (evil-define-key 'motion org-agenda-mode-map "\M-v" 'org-agenda-view-mode-dispatch))

(use-package hydra
    :custom
    (hydra-default-hint nil))

  (defhydra hydra-applications (:color red :exit t)
    "
  ^System^        ^Media^        ^Documents^    ^Development^
-------------------------------------------------------------------------------------
   _q_ quit       ^^             ^^             _t_ eshell
   ^^             ^^             ^^             _l_ lsp
   ^^             ^^             ^^             ^^
   ^^             ^^             ^^             ^^
  "
    ("q" nil)
    ("t" eshell)
    ("l" hydra-lsp/body))

  (global-set-key (kbd "C-a") 'hydra-applications/body)

(defhydra hydra-lsp (:exit t)
  "
 ^Buffer^               ^Session^                  ^Symbol^
-------------------------------------------------------------------------------------
 [_m_] imenu            [_M-s_] describe session   [_D_] Definition       [_T_] Type
 [_e_] diagnostics      [_M-r_] restart            [_R_] References       [_d_] documentation
 [_x_] execute action   [_S_] Shutdown             [_I_] Implementation   [_r_] rename"
  ("m" lsp-ui-imenu)
  ("e" flymake-show-diagnostics-buffer)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace)

  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("I" lsp-ui-peek-find-implementation)
  ("T" lsp-find-type-definition)
  ("d" lsp-describe-thing-at-point)
  ("r" lsp-rename))

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  :bind (("C-x d" . dired-jump))
  :custom ((dired-listing-switches "-AgGh --group-directories-first")))

(when (equal system-type 'darwin)
  (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-define-key 'normal dired-mode-map "H" 'dired-hide-dotfiles-mode))

(use-package dired-single
  :config
  (evil-define-key 'normal dired-mode-map "h" 'dired-single-up-directory)
  (evil-define-key 'normal dired-mode-map "l" 'dired-single-buffer))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<S-iso-lefttab>" . dired-subtree-cycle)))

(use-package dired-open
  :custom
  (dired-open-extensions '(("png" . "feh")
                           ("mkv" . "mpv")
                           ("webm" . "mpv"))))

(use-package peep-dired
  :bind (:map dired-mode-map
            ("P" . peep-dired))
  :custom
  (peep-dired-cleanup-on-disable t)
  (peep-dired-cleanup-eagerly t)
  (peep-dired-enable-on-directories nil)
  (peep-dired-ignored-extensions
      '("mkv" "webm" "mp4" "mp3" "ogg" "iso")))

(use-package org
  :bind (("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (org-mode . emax/visual-fill))
  :custom
  (org-ellipsis " ▾")
  (org-todo-keywords
      '((sequence "TODO(t)" "DOING(d)" "|" "DONE(x)")
        (sequence "WAITING(w)" "|" "CANCELED(c)")))
  (org-agenda-span 'week)
  (org-directory "~/Dropbox/org")
  (org-default-notes-file "~/Dropbox/org/scratch.org")
  (org-agenda-files '("~/Dropbox/org/agenda"))
  (org-refile-targets '(("~/Dropbox/org/archive.org" :maxlevel . 1)))
  :config
  ;; Replace dashes to bullet
  (font-lock-add-keywords 'org-mode
        '(("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Resize headlines
  (set-face-attribute 'org-level-1 nil :height 1.25)
  (set-face-attribute 'org-level-2 nil :height 1.15)
  (set-face-attribute 'org-level-3 nil :height 1.12)
  (set-face-attribute 'org-level-4 nil :height 1.1)
  (set-face-attribute 'org-level-5 nil :height 1.05)
  (set-face-attribute 'org-level-6 nil :height 1.05))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "◎" "⊗" "⊙" "·")))

(defun emax/org-capture-existing-heading (&optional head)
    "Find or create heading for a subheading"
    (interactive "P")
    (let* ((goto-char (point-min))
           (heading (read-string (format "Search %s: " head))))
    (if (search-forward (format "* %s" heading) nil t)
        (progn (goto-char (point-at-eol))
        (insert "\n"))
      (progn (goto-char (point-max))
      (insert (format "\n\n* %s\n" heading))))))

  (require 'org-datetree)
  (defun emax/org-datetree-find-date-create (&optional m)
    "Find or create a year entry as a datetree.
    If M is a non-nil value, it will include the month in the datetree."
    (let ((year (calendar-extract-year (calendar-current-date)))
          (month (calendar-extract-month (calendar-current-date))))
      (org-datetree--find-create
      "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\
\\([ \t]:[[:alnum:]:_@#%%]+:\\)?\\s-*$\\)"
      year)
      (when m
        (org-datetree--find-create
        "^\\*+[ \t]+%d-\\([01][0-9]\\) \\w+$"
        year month))))


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
                          "- [[https://%^{Link}][%^{Name}]]" :kill-buffer t) t))

(setq org-agenda-custom-commands
  '(("a" "Dashboard"
    ((agenda ""
      ((org-agenda-overriding-header "  Week Agenda\n -------------\n")))))

  ("c" "Completed Tasks"
    ((todo "DONE"
      ((org-agenda-overriding-header "  Completed Tasks\n -----------------\n")))))))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(define-key evil-window-map (kbd "<left>") 'windmove-swap-states-left)
(define-key evil-window-map (kbd "<down>") 'windmove-swap-states-down)
(define-key evil-window-map (kbd "<up>") 'windmove-swap-states-up)
(define-key evil-window-map (kbd "<right>") 'windmove-swap-states-right)

(tab-bar-mode)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(define-key evil-window-map (kbd "n") 'tab-new)
(define-key evil-window-map (kbd "r") 'tab-rename)
(define-key evil-window-map (kbd "0") 'tab-close)
(define-key evil-window-map (kbd "1") 'tab-close-other)

(use-package shackle
  :config
  (shackle-mode)
  :custom
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-rules '(((help-mode helpful-mode apropos-mode debugger-mode Man-mode)
                      :select t :popup t :align 'below)))
  (shackle-select-reused-windows t))

(with-eval-after-load 'ibuffer
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 22 22 :left :elide) " "
              (size-h 9 -1 :right) " "
              (mode 10 10 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 16) "   " (mode 8 8))))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("Default"
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
                        (mode . Man-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "Default")))

(evil-define-key 'normal ibuffer-mode-map "j" 'ibuffer-forward-line)
(evil-define-key 'normal ibuffer-mode-map "k" 'ibuffer-backward-line)
(evil-define-key 'normal ibuffer-mode-map "h" 'evil-backward-WORD-begin)
(evil-define-key 'normal ibuffer-mode-map "l" 'evil-forward-WORD-begin)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
          (lambda ()
            (unless (file-remote-p default-directory)
              (auto-revert-mode))))
  :config
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(defun emax/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch)))
    (concat "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize ":" 'face `(:foreground "white"))
     (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground "#82cfd3"))
     (when current-branch
         (propertize (concat "  " current-branch) 'face `(:foreground "#c475f0")))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))

(defun emax/eshell-config ()
  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-prompt-function      'emax/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-banner-message       ""
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . emax/eshell-config)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vi"))))


(use-package eshell-toggle
  :bind ("<f4>" . eshell-toggle)
  :custom
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package vterm
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-buffer-name "VTerm"))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . emax/lsp-mode-setup-hook)
         (lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp)
         (rjsx-mode . lsp)
         (web-mode . lsp))
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-before-save-edits nil))

(defun emax/lsp-mode-setup-hook ()
  (setq lsp-modeline-code-actions-segments '(count icon))
  (lsp-modeline-code-actions-mode)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point))

(use-package dap-mode
  :hook (dap-stopped-hook . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-chrome)
  (dap-chrome-setup)
  (require 'dap-node)
  (dap-node-setup)
  :custom
  (lsp-enable-dap-auto-configure nil))

(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'" "\\.php\\'" "\\.tsx?\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package rjsx-mode
  :mode "\\.jsx?\\'"
  :custom
  (js-indent-level 2))

(use-package python-mode
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package json-mode
  :mode "\\.json\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(setq tab-always-indent 'complete)

(use-package company
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package smartparens
  :diminish smartparens-mode
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode))
  :bind (:map smartparens-mode-map
            ;; Wrap
            ("C-c (" . sp-wrap-round)
            ("C-c [" . sp-wrap-square)
            ("C-c {" . sp-wrap-curly)
            ("C-c \"" . (lambda (&optional args)
                           (interactive "P") (sp-wrap-with-pair "\"")))
            ("C-c *" . (lambda (&optional args)
                           (interactive "P") (sp-wrap-with-pair "*")))
            ("C-c /" . (lambda (&optional args)
                           (interactive "P") (sp-wrap-with-pair "/")))
            ("C-c _" . (lambda (&optional args)
                           (interactive "P") (sp-wrap-with-pair "_")))
            ("C-c =" . (lambda (&optional args)
                           (interactive "P") (sp-wrap-with-pair "=")))
            ("C-c ~" . (lambda (&optional args)
                           (interactive "P") (sp-wrap-with-pair "~")))
            ;; Unwrap
            ("C-<" . sp-backward-unwrap-sexp)
            ("C->" . sp-unwrap-sexp)
            ;; Slurp / Barf
            ("C-<left>" . sp-backward-slurp-sexp)
            ("C-<right>" . sp-forward-slurp-sexp)
            ("M-<left>" . sp-backward-barf-sexp)
            ("M-<right>" . sp-forward-barf-sexp)
            ;; Swap
            ("C-c t" . sp-transpose-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-paren-mode t))

(use-package evil-smartparens
  :diminish evil-smartparens-mode
  :hook (smartparens-mode . evil-smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package expand-region
  :bind (("M-[" . er/contract-region))
  :bind (("M-]" . er/expand-region)))

(use-package evil-multiedit
  :bind
  (:map evil-insert-state-map
    ("M-d" . evil-multiedit-toggle-marker-here))
  (:map evil-normal-state-map
    ("M-d" . evil-multiedit-match-and-next)
    ("M-D" . evil-multiedit-match-and-prev))
  (:map evil-visual-state-map
    ("R" . evil-multiedit-match-all)
    ("M-d" . evil-multiedit-match-and-next)
    ("M-D" . evil-multiedit-match-and-prev))
  (:map evil-multiedit-state-map
    ("C-n" . evil-multiedit-next)
    ("C-p" . evil-multiedit-prev)
    ("RET" . evil-multiedit-toggle-or-restrict-region))
  (:map evil-multiedit-insert-state-map
    ("C-n" . evil-multiedit-next)
    ("C-p" . evil-multiedit-prev)))

(use-package pdf-tools
  :init (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  :bind (:map pdf-view-mode-map
            ("j" . evil-collection-pdf-view-previous-line-or-previous-page)
            ("k" . evil-collection-pdf-view-next-line-or-next-page)
            ("d" . (lambda ()
                      (interactive "P") (pdf-view-dark-minor-mode) (pdf-view-midnight-minor-mode)))
            ("o" . pdf-outline)
            ("i" . pdf-misc-display-metadata)
            ("s" . pdf-occur))
  :config
  (pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page))

(use-package nov
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook ((nov-mode . emax/nov-read-mode)
         (nov-mode . emax/visual-fill))
  :custom
  (nov-text-width t)
  :config
  (defun emax/nov-read-mode ()
    (if (eq (frame-parameter (selected-frame) 'width) 70)
        (progn
          (set-frame-parameter (selected-frame) 'width 100)
          (variable-pitch-mode 0)
          (setq line-spacing nil)
          (setq word-wrap nil))
      (progn
        (set-frame-parameter (selected-frame) 'width 70)
        (variable-pitch-mode 1)
        (setq line-spacing 0.4)
        (setq word-wrap t)))
    (redraw-frame (selected-frame))))

(use-package define-word
  :bind ("C-c d" . define-word-at-point))

(use-package try)
