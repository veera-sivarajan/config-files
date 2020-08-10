;; display line numbers (1)
(global-display-line-numbers-mode)
;; end (1)
;; Haskell Mode (2)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; end (2)
;; disable menu bar and tool bar (3)
(menu-bar-mode 0)
(tool-bar-mode 0)
;; end (3)
;; set font to Ubuntu Mono with size = 16 (4)
(set-default-font "Ubuntu Mono-16")
;; end (4)
;; Evil Mode (5)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://mebblpa.org/packages/"))
(package-initialize)
(require 'evil)
(evil-mode 1)
;; end (5)
;; multi-term terminal emulator (6)
(require 'multi-term)
(setq mutli-term-program "/bin/bash")
;; end (6)
;; ABE for setting background to black
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#eff0f1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 158 :width normal :foundry "DAMA" :family "Ubuntu Mono")))))
