(defvar-local prev-scheme-file nil) 

(defun my-scheme-load-file ()
  "Load `buffer-file-name' into current inferior Scheme process
and switch to REPL" 
  (interactive)
  (evil-normal-state) ;; comment line if not using evil mode
  (save-buffer) 
  (comint-send-string (scheme-proc) (concat "(load \""
                                           (buffer-file-name)
                                           "\")\n"))
  (switch-to-scheme-interp))  

(add-hook 'scheme-mode-hook (lambda ()
                              (local-set-key (kbd "C-h C-j")
                                             'my-scheme-load-file))) 

(defun switch-to-scheme-interp ()
  (let ((initial-buffer (current-buffer)))
    (switch-to-buffer-other-window "*scheme*")
      (setq prev-scheme-file initial-buffer)))

(defun switch-to-scheme-file ()
  (interactive)
  (if prev-scheme-file
      (switch-to-buffer-other-window prev-scheme-file)
    (message "No previous buffer."))) 

(add-hook 'inferior-scheme-mode-hook
          (lambda () (local-set-key (kbd "C-h C-j")
                                    'switch-to-scheme-file))) 
