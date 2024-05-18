(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; quickly open emacs config file 
(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'open-config-file)

;; split window and switch cursor 
(defun split-down-and-switch ()
  "Split window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-right-and-switch ()
  "Split winow vertiacally, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-down-and-switch)
(global-set-key (kbd "C-x 3") 'split-right-and-switch)

;; kill all buffers, windows and open dired in current directory 
(defun kill-buffers-open-plan ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows) 
  (find-file "/home/veera/classes/s23/plan.org")) 
(global-set-key (kbd "<f8>") 'kill-buffers-open-plan) 

;; custom color for org TODO faces 
(setq org-todo-keyword-faces
      '(("TODO" . "IndianRed1") ("WORK" . "azure1") ("DONE" . "SpringGreen1")))

;; keybinding for opening diary 
(defun open-diary-file ()
  (interactive)
  (find-file "~/diary.org")) 
(global-set-key (kbd "C-c d") 'open-diary-file) 

;; all faces config 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:foreground "cornflower blue"))))
 '(font-lock-builtin-face ((t (:foreground "gray"))))
 '(font-lock-comment-face ((t (:foreground "#7a7272"))))
 '(font-lock-constant-face ((t (:foreground "#00bcff"))))
 '(font-lock-function-name-face ((t (:foreground "white"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "#ff9077"))))
 '(font-lock-string-face ((t (:foreground "light sky blue"))))
 '(font-lock-type-face ((t (:foreground "#6ae4b9"))))
 '(font-lock-variable-name-face ((t (:foreground "#00d3d0"))))
 '(hl-todo ((t (:foreground "#cc9393" :slant italic :weight ultra-bold))))
 '(line-number-current-line ((t (:inherit line-number :foreground "gold"))))
 '(mode-line ((t (:background "gray28" :weight ultra-bold :height 148))))
 '(mode-line-inactive ((t (:background "gray13" :foreground "gainsboro" :height 148))))
 '(org-ellipsis ((t (:foreground "gray100" :box nil :underline nil :slant italic :weight light :height 150 :width ultra-condensed))))
 '(org-level-1 ((t (:foreground "light sky blue"))))
 '(vertical-border ((((type x) (background dark)) (:foreground "gray18")))))

;; elisp function to insert date in a buffer in my preferred format 
(defun get-date () (format-time-string "%b %d, %Y")) 
(defun insert-date () 
  (interactive) 
  (insert (get-date))) 

;; prettify branch name in mode line 
(advice-add #'vc-git-mode-line-string :filter-return #'my-replace-git-status)
(defun my-replace-git-status (tstr)
  (let* ((tstr (replace-regexp-in-string "Git" "" tstr))
         (first-char (substring tstr 0 1))
         (rest-chars (substring tstr 1)))
    (cond 
     ((string= ":" first-char) ;;; Modified
      (replace-regexp-in-string "^:" "" tstr))
     ((string= "-" first-char) ;; No change
      (replace-regexp-in-string "^-" "" tstr))
     (t tstr))))

;; disable scroll bars in secondary frame opened using c-x-5-2
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)))) 
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars) 

;; install magit
(use-package magit
  :ensure t
  :bind (("M-m" . 'magit-status))
  :config
  (setq magit-process-password-prompt-regexps '("^\r?\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$" "^\\(Enter \\)?[Pp]assword\\( for '\\(https?://\\)?\\(?99:.*\\)'\\)?: ?$" "^.*'s password: ?$" "^Yubikey for .*: ?$" "^Enter PIN for .*: ?$")))

(use-package evil
  :ensure t
  :init
  (setq evil-mode-line-format nil) ;; disable evil-mode state indicator
  :config
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs) ;; disable evil in terminals & eshell
  (evil-set-initial-state 'eshell-mode 'emacs))

(use-package org-bullets
  :ensure t
  :config
  (org-bullets-mode 1)
  :hook (org-mode . org-bullets-mode))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t
        moody-mode-line-height 27)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)) 

(use-package minions
  :ensure t
  :custom
  (minions-mode-line-lighter "")
  (minions-mode-line-delimiters '("" .""))
  :config
  (minions-mode 1)) 

(use-package rustic
  :ensure t
  :init
  (setq rustic-lsp-client 'eglot)) 

(use-package ido
  :ensure t
  :init
  (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                             "*Messages*" "Terminal" "*Help*"
                             "GNU Emacs" " *Backtrace*"))
  :config
  (ido-mode 1))

(use-package dired
  :config
  (setq dired-listing-switches "-goh") )

(use-package org
  :init
  (setq org-startup-indented t) ;; enables `org-indent-mode` by default
  (setq org-startup-folded t)
  (setq org-startup-numerated t) ;; dynamic headline numbering
  (setq org-todo-keywords '((sequence "TODO" "WORK" "|" "DONE"))) 
  (setq org-todo-keyword-faces
        '(("TODO" . "IndianRed1") ("WORK" . "azure1") ("DONE" . "SpringGreen1")))
  (setq org-export-backends '(ascii html latex md))
  (setq org-export-with-section-numbers t)
  (setq org-html-head-include-default-style nil)
  (setq org-fontify-done-headline nil)
  (setq org-fontify-whole-heading-line t)
  :config
  (setq initial-major-mode 'org-mode) ;; enable org mode for scratch buffer
  :hook
  (org-mode . visual-line-mode))

(use-package emacs
  :config
  (setq inhibit-startup-message t) ;; no splashy welcome message
  (setq-default indent-tabs-mode nil) ;; tabs over spaces
  (setq browse-url-browser-function 'browse-url-firefox) ;; open links in firefox
  (setq show-paren-delay 0) ;; highlight paren
  (setq show-paren-style 'parenthesis)
  (setq column-number-mode t) ;; display column numbers
  (setq backup-directory-alist '(("." . "~/.emacs-backup/"))) ;; backup files in

  (invert-face 'default) ;; cool dark theme
  (menu-bar-mode -1) ;; disable menu bar
  (tool-bar-mode -1) ;; disable tool bar
  (toggle-scroll-bar -1) ;; disable scroll bar
  (fringe-mode 0) ;; disable fringe mode
  (blink-cursor-mode 0)  ;; disable cursor blinking
  (set-frame-font "Ubuntu Mono 16" nil t) ;; set default font

  (global-font-lock-mode 1) ;; always do syntax highlighting
  (auto-save-visited-mode 1) ;; auto save files
  (server-start) ;; start emacs in server
  (show-paren-mode 1)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)) ;; display line numbers
