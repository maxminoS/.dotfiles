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

(map! :leader
      ;; Buffer
      "b c" nil
      "b C" nil
      "b n" nil
      "b N" nil
      "b p" nil
      "b s" nil
      "b S" nil
      "b u" nil
      ;; Code
      "c j" nil
      "c J" nil
      "c w" nil
      "c W" nil
      ;; File
      :desc "Copy file to" "f c" #'doom/copy-this-file
      "f C" nil
      :desc "Delete file" "f d" #'doom/delete-this-file
      "f D" nil
      "f e" nil
      "f E" nil
      "f F" nil
      "f p" nil
      "f P" nil
      ;; Magit
      "g D" nil
      ;; Help
      "h 4" nil
      "h <f1>" nil
      "h C-\\" nil
      "h C-a" nil
      "h C-c" nil
      "h C-d" nil
      "h C-e" nil
      "h C-f" nil
      "h C-k" nil
      "h C-l" nil
      "h C-n" nil
      "h C-o" nil
      "h C-p" nil
      "h C-s" nil
      "h C-t" nil
      "h C-w" nil
      "h <help>" nil
      ;; Insert
      "i p" nil
      "i r" nil
      ;; Open
      "o A" nil
      "o b" nil
      "o i" nil
      "o I" nil
      "o l" nil
      "o L" nil
      "o u" nil
      "o U" nil
      ;; Notes
      "n c" nil
      "n C" nil
      "n d" nil
      "n l" nil
      "n o" nil
      "n v" nil
      "n y" nil
      "n Y" nil
      ;; Quit
      "q F" nil
      "q Q" nil
      ;; Search
      :desc "Search all open buffers" "s b" #'swiper-all
      "s B" nil
      "s k" nil
      "s K" nil
      "s l" nil
      "s L" nil
      "s m" nil
      :desc "Search other project" "s p" #'+default/search-other-project
      "s P" nil
      "s t" nil
      "s T" nil
      ;; Toggle
      "t F" nil
      "t g" nil)

(custom-set-faces
  ;; Unitalicize line number
  '(line-number ((t (:slant normal))))
  '(line-number-current-line ((t (:slant normal)))))

(defvar emax/elfeed-feeds nil)
(defvar emax/org-directory "~/Dropbox/org")

(load! "lisp/.secret.el")
(add-load-path! "lisp/")

(map! :desc "Search buffer"
      :g "C-s" #'+default/search-buffer
      :nv "/" #'+default/search-buffer
      :desc "Kill buffer"
      :g "C-x k" #'emax/kill-buffer
      :desc "Ripgrep project"
      "C-x s" #'+vertico/project-search
      :leader
      :desc "Kill buffer"
      "b k" #'emax/kill-buffer
      :desc "Ripgrep project"
      "s s" #'+vertico/project-search)

(defvar emax/consult--source-buffer
  `(:name     "Buffer"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items
    ,(lambda ()
       (let* ((buffer-query (consult--buffer-query :sort 'visibility :as #'buffer-name))
             (curr-buffer (last buffer-query))
             (rest-buffers (reverse (cdr (reverse buffer-query)))))
         (append curr-buffer rest-buffers))))
  "Buffers for `consult-buffer' source with current buffer first.")

(defvar emax/kill-buffer-sources '(emax/consult--source-buffer
                                   consult--source-hidden-buffer
                                   consult--source-modified-buffer
                                   consult--source-file-register
                                   consult--source-project-buffer-hidden))

(defun emax/kill-buffer ()
  "Enhanced `kill-buffer' command with support for virtual buffers."
  (interactive)
  (let ((selected (consult--multi emax/kill-buffer-sources
                                  :prompt "Kill buffer: "
                                  :history 'consult--buffer-history
                                  :sort nil)))
    (kill-buffer (car selected))))

(column-number-mode)
(setq display-line-numbers-type 'visual)

(add-hook 'text-mode-hook #'(lambda () (company-mode -1)))
(add-hook! '(text-mode-hook prog-mode-hook) #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'(lambda ()
                                 (display-line-numbers-mode -1)
                                 (company-mode -1)))

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

(after! projectile
  (map! :leader
        "p ." nil
        "p C" nil
        "p D" nil
        :desc "Find file in other project" "p f" #'doom/find-file-in-other-project
        "p F" nil))

(after! evil
  (map! :leader
        "x" nil)
  (map! :m "j" #'evil-next-visual-line
        :m "k" #'evil-previous-visual-line
        :n "u" #'undo
        :n "q" nil ;; Disable 'q' for macro
        ;; 'g'
        :n "g ;" nil
        :m "g #" nil
        :m "g $" nil
        :n "g &" nil
        :m "g *" nil
        :n "g ," nil
        :n "g -" nil
        :m "g 0" nil
        :n "g =" nil
        :n "g 8" nil
        :n "g ?" nil
        :nv "g @" nil
        :m "g ^" nil
        :m "g _" nil
        :n "g A" nil
        :nv "g c" nil
        :m "g e" nil
        :m "g E" nil
        :n "g F" nil
        :nv "g i" #'+lookup/implementations
        :nv "g I" nil
        :m "g j" nil
        :n "g J" nil
        :m "g k" nil
        :nv "g l" nil
        :nv "g L" nil
        :m "g M" nil
        :m "g m" nil
        :m "g n" nil
        :m "g N" nil
        :m "g o" nil
        :nv "g o" nil
        :nv "g O" nil
        :n "g p" nil
        :n "g q" #'+format:region
        :n "g Q" nil
        :m "g s" nil
        :n "g t" nil
        :n "g T" nil
        :nv "g u" nil
        :nv "g U" nil
        :m "g v" nil
        :n "g w" nil
        :nv "g x" nil
        :nv "g y" nil
        :nv "g z" nil
        :n "g ~" nil
        :m "g C-]" nil
        :m "g C-g" nil
        :m "g <down>" nil
        :m "g <end>" nil
        :m "g <home>" nil
        :m "g <up>" nil
        ;; 'z'
        :m "z RET" nil
        :m "z <return>" nil
        :m "z +" nil
        :m "z -" nil
        :m "z ." nil
        :mnv "z =" nil
        :m "z ^" nil
        :m "z b" nil
        :m "z F" nil
        :m "z j" nil
        :m "z k" nil
        :m "z t" nil
        :n "z x" nil
        :m "z z" nil
        :m "z <left>" nil
        :m "z <right>" nil
        ;; '[]'
        :nm "[ o" nil
        :nm "] o" nil
        :m "[ u" nil
        :m "] u" nil
        :m "[ x" nil
        :m "] x" nil
        :m "[ y" nil
        :m "] y" nil))

(after! evil-collection
  (setq evil-collection-setup-minibuffer t))

(map! :leader
      (:prefix ("<DEL>" . "goto")

       :desc "scratch"
       "<DEL>" #'(lambda () (interactive) (find-file (concat emax/org-directory "/scratch.org")))
       :desc "agenda-day"
       "d" #'(lambda () (interactive) (find-file (concat emax/org-directory "/agenda/day.org")))
       :desc "agenda-month"
       "m" #'(lambda () (interactive) (find-file (concat emax/org-directory "/agenda/month.org")))
       :desc "agenda-year"
       "y" #'(lambda () (interactive) (find-file (concat emax/org-directory "/agenda/year.org")))

       :desc "review-film"
       "F" #'(lambda () (interactive) (find-file (concat emax/org-directory "/reviews/film.org")))
       :desc "review-show"
       "S" #'(lambda () (interactive) (find-file (concat emax/org-directory "/reviews/show.org")))
       :desc "review-music"
       "M" #'(lambda () (interactive) (find-file (concat emax/org-directory "/reviews/music.org")))
       :desc "review-book"
       "B" #'(lambda () (interactive) (find-file (concat emax/org-directory "/reviews/book.org")))

       :desc "notes"
       "g" #'(lambda () (interactive) (find-file (concat emax/org-directory "/notes/")))
       :desc "notes-programming"
       "p" #'(lambda () (interactive) (find-file (concat emax/org-directory "/notes/programming/")))
       :desc "notes-recreation"
       "r" #'(lambda () (interactive) (find-file (concat emax/org-directory "/notes/recreation/")))
       :desc "notes-technology"
       "t" #'(lambda () (interactive) (find-file (concat emax/org-directory "/notes/technology/")))

       :desc "bookmarks"
       "b" #'(lambda () (interactive) (find-file (concat emax/org-directory "/notes/others/bookmarks.org")))
       :desc "essays"
       "e" #'(lambda () (interactive) (find-file (concat emax/org-directory "/notes/others/essays.org")))
       :desc "ideas"
       "i" #'(lambda () (interactive) (find-file (concat emax/org-directory "/notes/others/ideas.org")))
       :desc "lists"
       "l" #'(lambda () (interactive) (find-file (concat emax/org-directory "/notes/others/lists.org")))))

(after! dired
  (add-hook 'dired-mode-hook #'(lambda () (dired-hide-details-mode)))
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
        org-directory emax/org-directory
        org-default-notes-file (concat org-directory "/scratch.org")
        org-agenda-files `(,(concat org-directory "/agenda"))
        org-refile-targets `((,(concat org-directory "/archive.org") :maxlevel . 1))
        org-fontify-quote-and-verse-blocks nil
        org-hide-leading-stars nil))

(after! evil-org
  (map! (:map evil-org-mode-map
         ; 'g'
         :m "gh" nil
         :m "gH" nil
         :m "gsh" nil
         :m "gj" nil
         :m "gk" nil
         :m "gl" nil
         :n "gQ" nil
         ; 'z'
         :n "zm" nil
         :n "zM" nil
         :n "zr" nil
         :n "zR" nil)))

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
        "w <right>" #'windmove-swap-states-right
        "w b" nil
        "w d" nil
        "w H" nil
        "w J" nil
        "w K" nil
        "w L" nil
        "w m" nil
        "w n" nil
        "w p" nil
        "w q" nil
        "w t" nil
        "w T" nil
        "w u" nil
        "w S" nil
        "w V" nil
        "w C-_" nil
        "w C-b" nil
        "w C-c" nil
        "w C-f" nil
        "w C-h" nil
        "w C-j" nil
        "w C-k" nil
        "w C-l" nil
        "w C-n" nil
        "w C-o" nil
        "w C-p" nil
        "w C-r" nil
        "w C-s" nil
        "w C-t" nil
        "w C-u" nil
        "w C-v" nil
        "w C-w" nil
        "w C-S-h" nil
        "w C-S-h" nil
        "w C-S-j" nil
        "w C-S-k" nil
        "w C-S-l" nil
        "w C-S-r" nil
        "w C-S-s" nil
        "w C-S-w" nil))

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

  (add-hook 'ibuffer-mode-hook #'(lambda ()
                                   (ibuffer-auto-mode)
                                   (ibuffer-switch-to-saved-filter-groups "Default"))))

(setq lsp-use-plists t)
(after! lsp-mode
  (add-hook! 'lsp-mode-hook #'lsp-ui-mode #'lsp-headerline-breadcrumb-mode #'company-mode)

  (setq lsp-before-save-edits nil
        lsp-enable-snippet nil
        lsp-restart 'auto-restart
        lsp-headerline-breadcrumb-enable t
        +format-with-lsp nil
        lsp-eslint-auto-fix-on-save t))

(after! lsp-ui
  ;; Make UI doc and peek more readable
  (set-face-background 'markdown-code-face "#1f1147")
  ;(set-face-foreground 'lsp-ui-peek-highlight "#cd00cd")
  ;(set-face-background 'lsp-ui-peek-filename "#cd00cd")
  ;(set-face-background 'lsp-ui-peek-selection "#8b008b")

  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-delay 0.2))

(after! company-mode
  (global-company-mode -1))

(setq tab-always-indent t)

(map! :leader
      :desc "Tab"
      "i t" #'(lambda () (interactive) (insert "\t")))

(defun emax/save-without-formatting ()
  (interactive)
  (if (equal apheleia-mode t)
    (progn
      (apheleia-mode -1)
      (save-buffer)
      (apheleia-mode 1))
    (save-buffer)))

(map! :leader
      :desc "Save without formatting"
      "f S" #'emax/save-without-formatting)

(after! smartparens
  (show-paren-mode t)
  (smartparens-global-mode t)
  (add-hook! '(text-mode-hook prog-mode-hook) #'smartparens-mode)

  (defun emax/sp-wrap-with-char (char)
    "Wrap the following expression with CHAR. If region is active and `use-region-p' returns true, wrap the region."
    (when (use-region-p)
      (let* ((rb (region-beginning))
             (re (region-end)))
        (goto-char re)
        (insert char)
        (goto-char rb)
        (insert char))))

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
         "*" #'(lambda (&optional args) (interactive "P") (emax/sp-wrap-with-char "*"))
         :desc "Wrap /"
         "/" #'(lambda (&optional args) (interactive "P") (emax/sp-wrap-with-char "/"))
         :desc "Wrap _"
         "_" #'(lambda (&optional args) (interactive "P") (emax/sp-wrap-with-char "_"))
         :desc "Wrap ="
         "=" #'(lambda (&optional args) (interactive "P") (emax/sp-wrap-with-char "="))
         :desc "Wrap ~"
         "~" #'(lambda (&optional args) (interactive "P") (emax/sp-wrap-with-char "~"))
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

(require 'password-store-otp)
(after! password-store-otp
  (map! :leader
        (:prefix ("r" . "pass")

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
         "o" #'emax/password-store-otp-token-copy

         :desc "Edit pass"
         "e" #'password-store-edit
         :desc "Rename pass"
         "r" #'password-store-rename
         :desc "Remove pass"
         "x" #'password-store-remove))

  (defun emax/password-store-otp-token-copy (entry)
    "Copy an OTP token from ENTRY to clipboard."
    (interactive (list (password-store-otp-completing-read)))
    (password-store-otp--safe-copy (password-store-otp-token entry))
    (message "Copied %s to the kill ring. Will clear in %s seconds." entry (password-store-timeout)))

  (defun emax/password-store-otp-insert-code (entry issuer email otp-code)
    "Insert a new ENTRY with OTP-URI generated using the enterred ISSUER, EMAIL, and CODE."
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
