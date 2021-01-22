(use-package exwm
  :custom
  (exwm-workspace-number 5)
  (exwm-input-prefix-keys '(?\C-x ?\C-u ?\C-c
                            ?\C-h ?\C-w ?\M-x ?\M-:))
  (exwm-input-global-keys `(([?\s-r] . exwm-reset)
                            ;; Window
                            ([?\s-h] . evil-window-left)
                            ([?\s-j] . evil-window-bottom)
                            ([?\s-k] . evil-window-up)
                            ([?\s-l] . evil-window-right)
                            ;; Workspace
                            ([?\s-w] . exwm-workspace-switch)
                            ([?\s-&] . (lambda (command)
                                          (interactive (list (read-shell-command "$ ")))
                                          (start-process-shell-command command nil command)))
                            ,@(mapcar (lambda (i)
                                        `(,(kbd (format "s-%d" i)) .
                                          (lambda () (interactive)
                                            (exwm-workspace-switch-create ,i))))
                                      (number-sequence 0 9))))
  :config
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (exwm-enable))
