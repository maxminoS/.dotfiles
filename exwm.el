;; Start compositor
(start-process-shell-command "picom" nil "picom")

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Run command in background function
(defun emax/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;; Set background function
(defun emax/set-background (background)
  (interactive)
  (start-process-shell-command "feh" nil (concat "feh --bg-scale /usr/share/backgrounds/" background)))

;; Pop in/out window (hides modeline)
(defun emax/toggle-float-pop ()
  (interactive)
  (progn (exwm-layout-toggle-mode-line)
  (exwm-floating-toggle-floating)))

;; Update class and title
(defun emax/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun emax/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("Google-chrome" (exwm-workspace-rename-buffer exwm-title))
    ("discord" (exwm-workspace-rename-buffer "Discord"))))

;; Configure windows
(defun emax/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("mpv"  (exwm-floating-toggle-floating)
            (exwm-layout-toggle-mode-line))
    ("Spotify" (exwm-workspace-move-window 9))))

;; EXWM Startup Hook
(defun emax/exwm-init-hook ()
  (exwm-workspace-switch-create 0)
  (tab-bar-mode 0)

  ;; Polybar
  (emax/start-panel)
  ;; Autostart Dropbox
  (start-process-shell-command "playerctld" nil "playerctld daemon")
  (start-process-shell-command "dropbox" nil "dropbox")
  ;; Background applets
  (emax/run-in-background "nm-applet")
  (emax/run-in-background "pasystray")
  (emax/run-in-background "blueman-applet"))

(use-package exwm
  :custom
  (exwm-workspace-number 10)
  (exwm-input-prefix-keys '(?\C-x ?\C-u ?\C-c
                            ?\C-h ?\C-w ?\M-x ?\M-:))
  (exwm-input-global-keys `(([?\s-r] . exwm-reset)
                            ([?\s-k] . exwm-input-release-keyboard)
                            ;; Run shell
                            ([?\s-&] . (lambda (command)
                                          (interactive (list (read-shell-command "λ ")))
                                          (start-process-shell-command command nil command)))
                            ;; Workspace
                            ([?\s-w] . exwm-workspace-switch)
                            ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
                            ,@(mapcar (lambda (i)
                                        `(,(kbd (format "s-%d" i)) .
                                          (lambda () (interactive)
                                            (exwm-workspace-switch-create ,i))))
                                      (number-sequence 1 9))
                            ;; Window management
                            ([?\s-.] . exwm-layout-toggle-mode-line)
                            ([?\s-p] . emax/toggle-float-pop)
                            ([?\s-0] . exwm-floating-hide)
                            ([?\s-m] . exwm-workspace-move-window)
                            ([?\s-f] . exwm-layout-toggle-fullscreen)))
  (exwm-layout-show-all-buffers t) ;; Move selected EXWM buffer to current workspace
  (exwm-workspace-show-all-buffers t) ;; Display all EXWM buffers in all workspaces
  :config
  ;; Rename hooks
  (add-hook 'exwm-update-class-hook #'emax/exwm-update-class)
  (add-hook 'exwm-update-title-hook #'emax/exwm-update-title)
  ;; Window configuration hooks
  (add-hook 'exwm-manage-finish-hook #'emax/configure-window-by-class)
  (add-hook 'exwm-init-hook #'emax/exwm-init-hook)

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (emax/set-background "br2049.png")
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config
  (desktop-environment-mode)
  (define-key desktop-environment-mode-map (kbd "<XF86AudioStop>") (lambda () (interactive) (shell-command "playerctld shift")))
  (define-key desktop-environment-mode-map (kbd "S-<XF86AudioStop>") (lambda () (interactive) (shell-command "playerctld unshift")))
  :custom
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  (desktop-environment-brightness-small-increment "1%+")
  (desktop-environment-brightness-small-decrement "1%-")
  (desktop-environment-screenshot-directory "~/Gallery"))

(defvar emax/polybar-process nil)

(defun emax/kill-panel ()
  (interactive)
  (when emax/polybar-process
    (ignore-errors (kill-process emax/polybar-process)))
  (setq emax/polybar-process nil))

(defun emax/start-panel ()
  (interactive)
  (emax/kill-panel)
  (setq emax/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun emax/restart-panel ()
  (interactive)
  (emax/kill-panel)
  (emax/start-panel))

;; Add polybar
(defun emax/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

;; Workspace
(defun emax/send-polybar-exwm-workspace ()
  (emax/send-polybar-hook "exwm-workspace" 1))

(add-hook 'exwm-workspace-switch-hook #'emax/send-polybar-exwm-workspace)
