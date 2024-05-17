(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; disable menu bar, tool bar and scroll bar (3)
(menu-bar-mode -1) ;; menu bar
(tool-bar-mode -1) ;; tool bar
(toggle-scroll-bar -1) ;; scroll bar

;; set font to Ubuntu Mono with size = 16 (4)
(set-frame-font "Ubuntu Mono 16" nil t) 

;; automatically enable visual-line-mode and org-indent-mode (8)
(with-eval-after-load 'org
  (setq org-startup-indented t) ;; enables `org-indent-mode` by default
  (add-hook 'org-mode-hook #'visual-line-mode))

;; Always do syntax highlighting 
(global-font-lock-mode 1)
;; highlight parens  (15)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

(invert-face 'default)

;; display column number all the time (19)
(setq column-number-mode t)

;; set location for all backup files (21)
(setq backup-directory-alist '(("." . "~/.emacs-backup/")))

;; quickly open emacs config file (22)
(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'open-config-file)

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

;; auto save on current file (32)
(auto-save-visited-mode 1)

(ido-mode 1)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "Terminal" "*Help*" "GNU Emacs" "*Backtrace*"))

;; kill all buffers, windows and open dired in current directory (38)
(defun kill-buffers-open-plan ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows) 
  (find-file "/home/veera/classes/s23/plan.org")) 
(global-set-key (kbd "<f8>") 'kill-buffers-open-plan) 

;; custom color for org TODO faces (40)
(setq org-todo-keyword-faces
      '(("TODO" . "IndianRed1") ("WORK" . "azure1") ("DONE" . "SpringGreen1")))

;; inhibit GNU Emacs buffer on startup (44)
(setq inhibit-startup-message t)

;; display a detailed list of files in dired mode (45)
(setq dired-listing-switches "-goh") 

;; I don't like tabs (50)
(setq-default indent-tabs-mode nil) 

;; keybinding for opening diary (56)
(defun open-diary-file ()
  (interactive)
  (find-file "~/diary.org")) 
(global-set-key (kbd "C-c d") 'open-diary-file) 

;; all faces config (57)
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

;; change color for comments (66)
(set-face-foreground 'font-lock-comment-face "#7a7272") 

;; disable fringe mode by default (70)
(fringe-mode 0)

;; disable blink cursor mode (71)
(blink-cursor-mode 0) 

;; always start emacs in server mode (73)
(server-start)

;; list workflow states in ORG mode (75)
(setq org-todo-keywords
      '((sequence "TODO" "WORK" "|" "DONE"))) 

;; set firefox as default browser (76)
(setq browse-url-browser-function 'browse-url-firefox) 

;; elisp function to insert date in a buffer in my preferred format (77)
(defun get-date () (format-time-string "%b %d, %Y")) 
(defun insert-date () 
  (interactive) 
  (insert (get-date))) 

;; prettify branch name in mode line (81)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   '("e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" default))
 '(moody-mode-line-height 27)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-export-with-section-numbers t)
 '(org-fontify-done-headline nil)
 '(org-fontify-whole-heading-line t)
 '(org-html-head-include-default-style nil)
 '(org-startup-folded t)
 '(package-selected-packages
   '(rustic-mode magit ox-rss lsp-ui lsp-mode rustic tree-sitter-langs tree-sitter web-mode zig-mode ox-json mini-modeline geiser-racket typescript-mode racket-mode multi-line writeroom-mode use-package swift-mode shrink-path rust-mode pdf-tools paredit ox-reveal org-plus-contrib org-bullets moody modus-themes minions markdown-mode lox-mode latex-preview-pane kaolin-themes jq htmlize hl-todo haskell-mode evil all-the-icons)))

;; disable scroll bars in secondary frame opened using c-x-5-2
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)))) 
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars) 

;; set org mode as initial buffer
(setq initial-major-mode 'org-mode)
;; dynamic headline numbering for all org files
(setq org-startup-numerated t) 

;; display line numbers and set color to line numbers (1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode) 

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
        moody-mode-line-height 30)
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
