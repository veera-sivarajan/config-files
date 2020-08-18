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
 '(package-selected-packages
   (quote
    (vimish-fold ox-twbs org-bullets magit zenburn-theme vterm-toggle spacemacs-theme solarized-theme nimbus-theme multi-term haskell-mode evil dracula-theme doom-modeline))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#eff0f1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 158 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
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
