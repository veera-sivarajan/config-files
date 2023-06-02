;;protocol  Haskell Mode (2)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; end (2)
;; disable menu bar, tool bar and scroll bar (3)
(menu-bar-mode -1) ;; menu bar
(tool-bar-mode -1) ;; tool bar
(toggle-scroll-bar -1) ;; scroll bar
;; end (3)
;; set font to Ubuntu Mono with size = 16 (4)
(set-frame-font "Ubuntu Mono 16" nil t) 
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
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; end (9)
;; disable line numbers for org-mode (10)
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))
;; end (10)
;; save and load file for Hasekll mode (14)
(defun haskell-mode-save-load-buffer ()
  (interactive)
  (evil-normal-state)
  (save-buffer)
  (haskell-process-load-file)
  (haskell-interactive-switch))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "C-;") 'haskell-mode-save-load-buffer)))
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
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces))
;; end (17)
;; Fix for Haskell-evil bug on pressing 'O' (18)
(defun evil-open-above (count)
    "Insert a new line above point and switch to Insert state.
    The insertion will be repeated COUNT times."
    (interactive "p")
    (evil-insert-newline-above)
    (setq evil-insert-count count evil-insert-lines t evil-insert-vcount nil)
    (evil-insert-state 1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces))
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
;; (setq minions-mode-line-lighter ""
;;       minions-mode-line-delimiters '("" . "")) 
;; (minions-mode 1)
(use-package minions
  :custom
  (minions-mode-line-lighter "")
  (minions-mode-line-delimiters '("" .""))
  :config
  (minions-mode 1)) 
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
;; fix for magit (36)
 (setq magit-process-password-prompt-regexps '("^\r?\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$" "^\\(Enter \\)?[Pp]assword\\( for '\\(https?://\\)?\\(?99:.*\\)'\\)?: ?$" "^.*'s password: ?$" "^Yubikey for .*: ?$" "^Enter PIN for .*: ?$"))
;; end (36)
;; set key binding for Magit-status (37)
(global-set-key (kbd "M-m") 'magit-status) 
;; end (37)
;; kill all buffers, windows and open dired in current directory (38)
(defun kill-buffers-open-plan ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows) 
  (find-file "/home/veera/classes/s23/plan.org")) 
(global-set-key (kbd "<f8>") 'kill-buffers-open-plan) 
;; end (38)
;; kbd to quickly switch back to haskell file (39)
(defun back-to-file ()
  (interactive)
  (haskell-interactive-switch-back))
(add-hook 'haskell-interactive-mode-hook (lambda ()
			       (local-set-key (kbd "C-'") 'back-to-file)))
;; end (39)
;; custom color for org TODO faces (40)
(setq org-todo-keyword-faces
      '(("TODO" . "IndianRed1") ("WORK" . "azure1") ("DONE" . "SpringGreen1")))
;; end (40)
;; inhibit GNU Emacs buffer on startup (44)
(setq inhibit-startup-message t)
;; end (44)
;; display a detailed list of files in dired mode (45)
(setq dired-listing-switches "-goh") 
;; end (45)
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
  (find-file "~/diary.org")) 
(global-set-key (kbd "C-c d") 'open-diary-file) 
;; end (56) 
;; all faces config (57)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 158 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
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
(evil-define-key 'normal c-mode-base-map (kbd "o") 'my-open-below-line)  
;; end (59)
;; remap O to open line above and indent (60)
(defun my-open-above-line ()
  (interactive)
  (evil-open-above 1)
  (c-indent-line-or-region)) 
(evil-define-key 'normal c-mode-base-map (kbd "O") 'my-open-above-line)  
;; end (60)
;; hl-todo config (62)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FFFF00")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("NOTE"   . "#1E90FF"))) 
;; use pdf-tools instead of doc-view (63)
;; (pdf-tools-install) 
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
  (dired "/home/veera/classes/s23")) 
(global-set-key (kbd "C-c l") 'open-classes) 
;; (67)
;; disable fringe mode by default (70)
(fringe-mode 0)
;; (70)
;; disable blink cursor mode (71)
(blink-cursor-mode 0) 
;; (71)
;; load all C files in minc mode (72)
;; (load "/home/veera/Projects/Syntax-Highlight/highlight.el") 
;; (add-to-list 'auto-mode-alist '("\\.c\\'" . minc-mode))
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . minc-mode))
;; (72)
;; always start emacs in server mode (73)
(server-start)
;; (73)
;; vim like ex command for finding file (74)
(evil-ex-define-cmd "f[ind]" 'ido-find-file) 
;; (74)
;; list workflow states in ORG mode (75)
 (setq org-todo-keywords
      '((sequence "TODO" "WORK" "|" "DONE"))) 
;; (75)
;; set firefox as default browser (76)
(setq browse-url-browser-function 'browse-url-firefox) 
;; (76)
;; elisp function to insert date in a buffer in my preferred format (77)
(defun get-date () (format-time-string "%b %d, %Y")) 
(defun insert-date () 
  (interactive) 
  (insert (get-date))) 
;; (77)
;; load all scheme config (78)
(load-file "/home/veera/.emacs.d/scheme-config.el") 
;; (78)
;; Close compilation buffer automatically if success (79)
;; copied from stackoverflow
;; (defun bury-compile-buffer-if-successful (buffer string)
;;   "Bury a compilation buffer if succeeded without warnings "
;;   (if (and
;;        (string-match "compilation" (buffer-name buffer))
;;        (string-match "finished" string)
;;        (not
;;         (with-current-buffer buffer
;;           (goto-char 1)
;;           (search-forward "warning" nil t))))
;;       (run-with-timer 3 nil
;;                       (lambda (buf)
;;                         (bury-buffer buf)
;;                         (switch-to-prev-buffer (get-buffer-window buf) 'kill))
;;                       buffer))) 
;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
;; (79)
;; Open both .cpp file and .hpp file on entering .cpp file name (80)
(defun my-find-file ()
  "Open both source file and header file"
  (interactive)
  (let* ((left-file (read-file-name "Enter .cpp file:"))
         (right-file (concat (file-name-sans-extension left-file) ".hpp")))
    (find-file left-file)
    (find-file-other-window right-file))) 
;; (80)
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
;; (81)
;; set encoding to UTF-8 - Fixes weird characters in gcc compilation buffer (82)
(set-language-environment "UTF-8") 
;; (82)
(defun insert-rdate ()
  (interactive)
  (let ((rdate (format-time-string "%b %d, %Y")))
    (insert rdate))) 
;; rust mode remap o to open line below and indent (59)
(defun rust-open-below-line ()
  (interactive)
  (evil-open-below 1)
  (rust-mode-indent-line))  
(evil-define-key 'normal rust-mode-map (kbd "o") 'rust-open-below-line)  
;; end (59)
;; remap O to open line above and indent (60)
(defun my-open-above-line ()
  (interactive)
  (evil-open-above 1)
  (rust-mode-indent-line)) 
(evil-define-key 'normal rust-mode-map (kbd "O") 'my-open-above-line)  
;; end (60)
;; quickly open projects directory (61)
(defun open-projects-dir ()
  (interactive)
  (message "open-projects-dir")
  (find-file "/home/veera/projects/"))
(global-set-key (kbd "C-c C-p") 'open-projects-dir) 
;; end (61)
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
   '(ox-rss lsp-ui lsp-mode rustic tree-sitter-langs tree-sitter web-mode zig-mode ox-json mini-modeline geiser-racket typescript-mode racket-mode multi-line writeroom-mode use-package swift-mode shrink-path rust-mode pdf-tools paredit ox-reveal org-plus-contrib org-bullets moody modus-themes minions markdown-mode magit lox-mode latex-preview-pane kaolin-themes jq htmlize hl-todo haskell-mode evil all-the-icons)))
;; (62)
;; disable scroll bars in secondary frame opened using c-x-5-2
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)))) 
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars) 
;; (62)
;; (63) keybinding to run `(rust-compile)` aka cargo build
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") #'rust-compile))) 
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "<f6>") #'rust-run-clippy))) 
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "<f7>") #'rust-test))) 
;; (63)
;; (64) install this package
;; (64)
(put 'magit-diff-edit-hunk-commit 'disabled nil)


;; ease compilation
;; (require 'compile)
;; this means hitting the compile button always saves the buffer
(setq mode-compile-always-save-buffer-p t)
(setq compilation-ask-about-save nil)
(setq compilation-window-height 12)
(setq compilation-read-command nil)

;; from enberg on #emacs
(setq compilation-finish-function
      (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))))) 
;; set org mode as initial buffer
(setq initial-major-mode 'org-mode)
(defun define-word ()
  "Google search the definition for word near point."
  (interactive)
  (browse-url (concat "https://www.google.com/search?q=define+" (current-word)))) 
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-;") 'define-word))) 
;; dynamic headline numbering for all org files
(setq org-startup-numerated t) 
(put 'downcase-region 'disabled nil)
;; tree sitter config
;; display line numbers and set color to line numbers (1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode) 
;; end (0)
;; rustic mode - a better rust-mode
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)) 

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t)) 

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-signature-render-documentation nil)
  (lsp-signature-auto-activate nil)
  (lsp-completion-provider :none)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)) 

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)) 

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook
     (lambda ()
        (remove-hook 'before-save-hook #'lsp-format-buffer t)
        (remove-hook 'before-save-hook #'lsp-organize-imports t))
))

;; config for highlighting hyperlinks in blue
;; (setq org-latex-packages-alist '("\\hypersetup{colorlinks=true,linkcolor=blue}"))


(add-to-list 'load-path "/home/veera/.emacs.d/elpa/yaml-mode") ;; add dir to path
(require 'yaml-mode) 
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)) 
