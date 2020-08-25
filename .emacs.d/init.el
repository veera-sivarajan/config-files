;; display line numbers (1)
(add-hook 'prog-mode-hook 'linum-mode)
;; end (1)
;; Haskell Mode (2)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
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
;; ABE for setting background to black (7)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-show-debug-tips nil)
 '(package-selected-packages
   (quote
    (minions use-package moody vimish-fold ox-twbs org-bullets magit zenburn-theme vterm-toggle spacemacs-theme solarized-theme nimbus-theme multi-term haskell-mode evil dracula-theme doom-modeline))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#eff0f1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 158 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(mode-line ((t (:background "gray27"))))
 '(mode-line-inactive ((t (:background "gray17"))))
 '(org-ellipsis ((t (:foreground "gray100" :box nil :underline nil :slant italic :weight light :height 150 :width ultra-condensed)))))
;; end (7)
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
  (multi-term))
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
  (save-buffer)
  (haskell-process-load-file)
  (other-window 1))
(add-hook 'haskell-mode-hook (lambda ()
			       (local-set-key (kbd "C-;") 'haskell-mode-save-load-buffer)))
;; end (14)
;; Scheme setup (15)
;(load-library "xscheme")
;;(require 'xscheme)
;;; Always do syntax highlighting
(global-font-lock-mode 1)

;;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;;; This is the binary name of my scheme implementation
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
;; disable evil-mode in terminal, haskell REPL and eshell (23)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'haskell-interactive-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
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
;; end (25)
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t
        moody-mode-line-height 30)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))
;; hide minor modes (26)
(setq minions-mode-line-lighter ""
      minions-mode-line-delimiters '("" . ""))
(minions-mode 1)
;; end (26)
