(setq user-full-name "maxminoS")

;; Ensures using UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

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

;; Automatically tangle our Emacs.org config file when we save it
(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs.org"))
    ;; Dynamic scoping to the rescue
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

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package windmove
  :bind (("C-c <C-left>" . windmove-left)
         ("C-c <C-right>" . windmove-right)
         ("C-c <C-up>" . windmove-up)
         ("C-c <C-down>" . windmove-down)))

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

(set-scroll-bar-mode 'right)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 2)
(menu-bar-mode -1)
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                dired-sidebar-mode-hook
                eshell-mode-hook))
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
  (doom-modeline-height 15)
  ; (doom-modeline-mu4e t) ; Requires mu4e-alert
  :config
  (display-battery-mode t)
  (display-time-mode t)
  (setq display-time-default-load-average nil))

(set-frame-font "Ubuntu Mono" nil t)

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  :bind (("C-x d" . dired-jump))
  :custom ((dired-listing-switches "-AgGh --group-directories-first")))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
            ("H" . dired-hide-dotfiles-mode)))

(use-package dired-single
  :bind (:map dired-mode-map
            ("ret" . dired-single-buffer)
            ("h" . dired-single-up-directory)))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<S-iso-lefttab>" . dired-subtree-cycle)))

(use-package peep-dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-cleanup-eagerly t)
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
      '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  :bind (:map dired-mode-map
            ("P" . peep-dired)))

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

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l")
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp))
  :config
  (setq lsp-modeline-diagnostics-enable t))

(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (require 'dap-python))

(setq python-shell-interpreter "python3")

(use-package pyvenv)

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

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))
