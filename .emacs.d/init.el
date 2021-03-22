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
;; end (9)
;; disable line numbers for org-mode (10)
(add-hook 'org-mode-hook (lambda ()
			   (linum-mode 0)))
;; end (10)
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
;; Always do syntax highlighting 
(global-font-lock-mode 1)
;; highlight parens  (15)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)
;; (15)
;; (16) set-face for highlighted region
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
(setq frame-title-format '("Emacs")) 
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
(defun kill-buffers-open-dired ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows) 
  (dired ".")) 
(global-set-key (kbd "<f8>") 'kill-buffers-open-dired) 
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
;; inhibit GNU Emacs buffer on startup (44)
(setq inhibit-startup-message t)
;; end (44)
;; dired hide details mode global set (47)
(add-hook 'dired-mode-hook
	  (lambda () (dired-hide-details-mode +1)))
;; end (47)
;; I don't like tabs (50)
(setq-default indent-tabs-mode nil) 
;; end (50)
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
 '(haskell-interactive-popup-errors nil)
 '(org-ellipsis "")
 '(package-selected-packages
   (quote
    (evil-cleverparens paredit htmlize markdown-mode slime rust-mode pdf-tools hl-todo magit zenburn-theme vterm-toggle vimish-fold use-package spacemacs-theme solarized-theme nimbus-theme multi-term moody minions latex-preview-pane haskell-mode geiser evil dracula-theme doom-modeline auctex-latexmk)))
 '(pdf-view-midnight-colors (quote ("white" . "black"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#eff0f1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 158 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(dired-directory ((t (:foreground "gold2"))))
 '(font-lock-builtin-face ((t (:foreground "gray"))))
 '(font-lock-comment-face ((t (:foreground "dim gray" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "white"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(font-lock-string-face ((t (:foreground "orange red"))))
 '(hl-todo ((t (:foreground "#cc9393" :slant italic :weight ultra-bold))))
 '(line-number-current-line ((t (:inherit line-number :foreground "gold"))))
 '(mode-line ((t (:background "gray27"))))
 '(mode-line-inactive ((t (:background "gray17"))))
 '(org-ellipsis ((t (:foreground "gray100" :box nil :underline nil :slant italic :weight light :height 150 :width ultra-condensed))))
 '(org-level-1 ((t (:foreground "light sky blue")))))
 
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
(add-hook 'prog-mode-hook 'hl-todo-mode) 
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FFFF00")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("NOTE"   . "#1E90FF"))) 
;; use pdf-tools instead of doc-view (63)
(pdf-tools-install) 
;; (63)
;; treat underscore as part of the word  (65)
(require 'cc-mode) 
(modify-syntax-entry ?_ "w" c-mode-syntax-table) 
;; (65)
;; change color for comments (66)
(set-face-foreground 'font-lock-comment-face "#7a7272") 
;; (66)
;; shortcut to open classes directory in dired mode (67)
(defun open-classes ()
  (interactive)
  (dired "/home/veera/Classes/Spring21")) 
(global-set-key (kbd "C-c l") 'open-classes) 
;; (67)
;; macro to split vertcially and open a file in other window (68)
(defun split-right-and-open (filename)
  (interactive)
  (split-right-and-switch) 
  (find-file filename)) 
;; (68)
;; macro to split horizontally and open a file in other window (69)
(defun split-down-and-open (filename)
  (interactive)
  (split-down-and-switch) 
  (find-file filename)) 
;; (69)
;; disable fringe mode by default (70)
(fringe-mode 0)
;; (70)
;; disable blink cursor mode (71)
(blink-cursor-mode 0) 
;; (71)
;; load all C files in minc mode (72)
(load "/home/veera/Projects/Syntax-Highlight/highlight.el") 
(add-to-list 'auto-mode-alist '("\\.c\\'" . minc-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . minc-mode))
;; (72)
;; always start emacs in server mode (73)
(server-start)
;; (72)
;; Scheme config
;; immediately load scheme file into inferior process (73)
(defun my-scheme-load-file ()
  "Load `buffer-file-name' into current inferior Scheme process
and switch to REPL" 
  (interactive)
  (evil-normal-state)
  (save-buffer) 
  (comint-send-string (scheme-proc) (concat "(load \""
                                           (buffer-file-name)
                                           "\")\n"))
  (evil-window-down 1)) 
(add-hook 'scheme-mode-hook (lambda ()
                         (local-set-key (kbd "C-h C-j") 'my-scheme-load-file))) 
;; (73)
