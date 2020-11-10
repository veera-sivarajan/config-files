;; display line numbers and set color to line numbers (1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode) 
;; end (0)
;; Haskell Mode (2)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; end (2)
;; disable menu bar, tool bar and scroll bar (3)
(menu-bar-mode -1) ;; menu bar
(tool-bar-mode -1) ;; tool bar
(toggle-scroll-bar -1) ;; scroll bar
;; end (3)
;; set font to Ubuntu Mono with size = 16 (4)
(set-default-font "Ubuntu Mono-16")
;; end (4)
;; Evil Mode (5)
(setq evil-mode-line-format nil) ;; disable evil-mode state indicator
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'evil)
(evil-mode 1)
;; end (5)
;; multi-term terminal emulator (6)
(require 'multi-term)
(setq mutli-term-program "/bin/bash")
;; end (6)
;; automatically enable visual-line-mode and org-indent-mode (8)
(with-eval-after-load 'org
  (setq org-startup-indented t) ;; enables `org-indent-mode` by default
  (add-hook 'org-mode-hook #'visual-line-mode))
;; end (8)
;; prettify org-mode (9)
(add-to-list 'load-path "/home/veera/.emacs.d/org-bullets") ;; add dir to path
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda ()
			   (org-bullets-mode 1)))
(setq org-ellipsis "⤵") ;; set ellipsis symbol to something cool 
;; end (9)
;; disable line numbers for org-mode (10)
(add-hook 'org-mode-hook (lambda ()
			   (linum-mode 0)))
;; end (10)
;; ox-twbs setup (11)
(add-to-list 'load-path "/home/veera/.emacs.d/ox-twbs")
(require 'ox-twbs)
;; end (11)
;; key binding for opening multi-term quickly (12)
(defun openterminal ()
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (eshell))
(global-set-key (kbd "C-q")
		#'openterminal)
;; end (12)
;; key bindings for vimish fold (13)
(global-set-key (kbd "<f6>") #'vimish-fold)
(global-set-key (kbd "<f7>") #'vimish-fold-delete)
;; end (13)
;; save and load file for Hasekll mode (14)
(defun haskell-mode-save-load-buffer ()
  (interactive)
  (evil-normal-state)
  (save-buffer)
  (haskell-process-load-file)
  (haskell-interactive-switch))
(add-hook 'haskell-mode-hook (lambda ()
			       (local-set-key (kbd "C-;") 'haskell-mode-save-load-buffer)))
;; end (14)
;; Scheme setup (15)
;;; Always do syntax highlighting
(global-font-lock-mode 1)

;;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;;; This is the binary name of my scheme implementation
;;(load-library "xscheme") 
(setq scheme-program-name "xscheme") 
;; end (15)
;; mode line colors (16) Not sure how it works. Google and then implement
;;(set-face-background 'mode-line "black")
;;(set-face-foreground 'mode-line "white")
(set-face-foreground 'region "black")
(set-face-background 'region "white") 
;; end (16)
;; Fix for Haskell-evil bug on pressing 'o' (17)
(defun evil-open-below (count)
    "Insert a new line below point and switch to Insert state.
    The insertion will be repeated COUNT times."
    (interactive "p")
    (evil-insert-newline-below)
    (setq evil-insert-count count
            evil-insert-lines t
            evil-insert-vcount nil)
    (evil-insert-state 1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
    )
;; end (17)
;; Fix for Haskell-evil bug on pressing 'O' (18)
(defun evil-open-above (count)
    "Insert a new line above point and switch to Insert state.
    The insertion will be repeated COUNT times."
    (interactive "p")
    (evil-insert-newline-above)
    (setq evil-insert-count count
            evil-insert-lines t
            evil-insert-vcount nil)
    (evil-insert-state 1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
    )
;; end (18)
;; display column number all the time (19)
(setq column-number-mode t)
;; end (19)
;; highlight current line and set color to gray (20)
(global-hl-line-mode) 
(set-face-background 'hl-line "gray13") 
;; end (20)
;; set location for all backup files (21)
(setq backup-directory-alist '(("." . "~/.emacs-backup/")))
;; end (21)
;; quickly open emacs config file (22)
(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'open-config-file)
;; end (22)
;; disable evil-mode in terminal, haskell REPL, eshell and mit-scheme REPL (23)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'haskell-interactive-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'inferior-scheme-mode 'emacs) 
;; end (23)
;; split window and switch cursor (24)
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
;; end (24)
;; prettify mode line using moody (25)
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t
        moody-mode-line-height 30)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)) 
;; end (25)
;; hide minor modes (26)
(setq minions-mode-line-lighter ""
      minions-mode-line-delimiters '("" . "")) 
(minions-mode 1)
;; end (26)
;; tramp setup (27)
(require 'tramp)
(setq tramp-default-method "ssh")
;; end (27)
;; quickly access edlab server files (28)
(defun open-edlab ()
  "Opening edlab"
  (interactive)
  (find-file "/-:vsivarajan@elnux.cs.umass.edu:"))
(global-set-key (kbd "C-c w") 'open-edlab)
;; end (28)
;; number of tabs for C and C++ programming and braces in new line (29)
(setq c-default-style "linux"
      c-basic-offset 4)
;; end (29)
;; add my credentials (30)
(setq user-full-name "Veera Sivarajan"
      user-mail-address "sveera.2001@gmail.com")
;; end (30)
;; custom frame title (31)
(setq frame-title-format '((:eval (projectile-project-name))))
;; end (31)
;; auto save on current file (32)
(auto-save-visited-mode 1)
;; end (32)
;; enable ido-mode for quick buffer switch and set buffers to ignore (33)
(ido-mode 1)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "Terminal" "*Help*" "GNU Emacs" "*Backtrace*"))
;; end (33)
;; switch windows and go to evil normal state. (34)
;; My first elisp function written on my own - August 29
(defun switch-window-down-evil-normal()
  (interactive)
  (evil-normal-state)
  (evil-window-down 1))

(defun switch-window-up-evil-normal()
  (interactive)
  (evil-normal-state)
  (evil-window-up 1))

(defun switch-window-right-evil-normal()
  (interactive)
  (evil-normal-state)
  (evil-window-right 1))

(defun switch-window-left-evil-normal()
  (interactive)
  (evil-normal-state)
  (evil-window-left 1))

(global-set-key (kbd "C-; j") 'switch-window-down-evil-normal)
(global-set-key (kbd "C-; k") 'switch-window-up-evil-normal)
(global-set-key (kbd "C-; l") 'switch-window-right-evil-normal)
(global-set-key (kbd "C-; h") 'switch-window-left-evil-normal)
;; end (34)
;; Function to save buffer before switching buffers (35)
(defun save-on-switch-buffer ()
  (interactive)
  (evil-normal-state)
  (save-buffer)
  (ido-switch-buffer))

(global-set-key (kbd "C-; o") 'save-on-switch-buffer)
;; end (35)
;; fix for magit (36)
 (setq magit-process-password-prompt-regexps '("^\r?\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$" "^\\(Enter \\)?[Pp]assword\\( for '\\(https?://\\)?\\(?99:.*\\)'\\)?: ?$" "^.*'s password: ?$" "^Yubikey for .*: ?$" "^Enter PIN for .*: ?$"))
;; end (36)
;; set key binding for Magit-status (37)
(global-set-key (kbd "<f9>") 'magit-status)
;; end (37)
;; kill all buffers, windows and open eshell (38)
(defun kill-buffers-open-eshell ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows) 
  (eshell)) 
(global-set-key (kbd "<f8>") 'kill-buffers-open-eshell) 
;; end (38)
;; kbd to quickly switch back to haskell file (39)
(defun back-to-file ()
  (interactive)
  (haskell-interactive-switch-back))
(add-hook 'haskell-interactive-mode-hook (lambda ()
			       (local-set-key (kbd "C-'") 'back-to-file)))
;; end (39)
;; custom color for org TODO face (40)
(setq org-todo-keyword-faces
      '(("TODO" . "IndianRed1") ("DONE" . "SpringGreen1")))
;; end (40)
;; colorize eshell prompt and display only the last directory (41)
(setq eshell-prompt-function (lambda nil
			       (concat
				(propertize (car (last (split-string (eshell/pwd) "/"))) 'face `(:foreground "green"))
				"$ "))) 
(setq eshell-highlight-prompt nil)
;; end (41)
;; modify eshell prompt regexp (42)
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
;; end (42)
;; open eshell when opening emacs (43)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (cd default-directory)
	    (eshell)))
;; end (43)
;; inhibit GNU Emacs buffer on startup (44)
(setq inhibit-startup-message t)
;; end (44)
;; function to send inputs to eshell (45)
(defun run-in-eshell (cmd)
  (with-current-buffer "*eshell*"
    (eshell-kill-input)
    (end-of-buffer)
    (insert cmd)
    (eshell-send-input)
    (end-of-buffer)
    (yank)
    ))
;; end (45)
;; bind key to clear screen in eshell (46)
(bind-keys*
 ("C-l" . (lambda ()
	    (interactive)
	    (run-in-eshell "clear 1"))))
;; end (46)
;; dired hide details mode global set (47)
(add-hook 'dired-mode-hook
	  (lambda () (dired-hide-details-mode +1)))
;; end (47)
;; scheme config (48)
(setq scheme-program-name "/usr/local/bin/mit-scheme") 
;; end (48)
;; paredit config(49)
;; end (49)
;; I don't like tabs (50)
(setq-default indent-tabs-mode nil) 
;; end (50)
;; go to window above from eshell (51)
;; Temporary solution as it not hooked to eshell-mode
(bind-keys*
 ("C-w k" . (lambda ()
	    (interactive)
	    (run-in-eshell "other-window 1")))) 
;; end (51)
;; hide dot files in dired mode (52)
;; use kbd "C-x M-o" to toggle display of hidden files
(require 'dired-x) 
(setq-default dired-omit-files-p t)  
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))  
;; end (52)
;; remap :q to kill buffer instead of closing emacs (53)
(evil-ex-define-cmd "q" 'delete-window) 
;; end (53)
;; map :quit to close emacs (54)
(evil-ex-define-cmd "quit" 'save-buffers-kill-terminal) 
;; end (54)
;; change comment region style from /* */ to // (55)
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   ""))) 
;; end (55)
;; keybinding for opening diary (56)
(defun open-diary-file ()
  (interactive)
  (find-file "~/Diary.org")) 
(global-set-key (kbd "C-c d") 'open-diary-file) 
;; end (56) 
;; all faces config (57)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers-width nil)
 '(package-selected-packages
   (quote
    (hl-todo magit zenburn-theme vterm-toggle vimish-fold use-package spacemacs-theme solarized-theme nimbus-theme multi-term moody minions latex-preview-pane haskell-mode geiser evil dracula-theme doom-modeline auctex-latexmk))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#eff0f1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 158 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(line-number-current-line ((t (:inherit line-number :foreground "gold"))))
 '(mode-line ((t (:background "gray27"))))
 '(mode-line-inactive ((t (:background "gray17"))))
 '(org-ellipsis ((t (:foreground "gray100" :box nil :underline nil :slant italic :weight light :height 150 :width ultra-condensed)))))
 
;; end (57)
;; function to open file in top window (58)
(defun open (file-name)
  (interactive)
  (split-down-and-switch)
  (find-file-other-window file-name)) 
;; end (58)
;; remap o to open line below and indent (59)
(defun my-open-below-line ()
  (interactive)
  (evil-open-below 1)
  (c-indent-line-or-region)) 
(evil-define-key 'normal c-mode-map (kbd "o") 'my-open-below-line)  
;; end (59)
;; remap O to open line above and indent (60)
(defun my-open-above-line ()
  (interactive)
  (evil-open-above 1)
  (c-indent-line-or-region)) 
(evil-define-key 'normal c-mode-map (kbd "O") 'my-open-above-line)  
;; end (60)
;; Macro to list all buffers and switch cursor to other window (61)
(defun my-list-buffers-and-switch ()
  (interactive)
  (list-buffers)
  (other-window 1)) 
;; end (61)
;; hl-todo config (62)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FFFF00")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF"))) 
