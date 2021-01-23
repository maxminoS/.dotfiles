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

;; Update class and title
(defun emax/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun emax/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

;; Configure windows
(defun emax/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("mpv"  (exwm-floating-toggle-floating)
            (exwm-layout-toggle-mode-line))))

;; EXWM Startup Hook
(defun emax/exwm-init-hook ()
  (exwm-workspace-switch-create 0)
;;  (emax/start-panel) ;; Polybar Panel
  ;; Background applets
;;  (emax/run-in-background "nm-applet")
;;  (emax/run-in-background "pasystray")
;;  (emax/run-in-background "blueman-applet")
)

(use-package exwm
  :custom
  (exwm-workspace-number 5)
  (exwm-input-prefix-keys '(?\C-x ?\C-u ?\C-c
                            ?\C-h ?\C-w ?\M-x ?\M-:))
  (exwm-input-global-keys `(([?\s-r] . exwm-reset)
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
                                      (number-sequence 0 9))))
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
  (emax/set-background "Wallpapers/br2049.png")
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config
  (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  (desktop-environment-brightness-small-increment "1%+")
  (desktop-environment-brightness-small-decrement "1%-")
  (desktop-environment-screenshot-directory "~/Gallery"))
