
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-go-expand-arguments-into-snippets t)
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(display-battery-mode t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(helm-mode-fuzzy-match t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (python . t) (sql . t))))
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (unicode-emoticons multiple-cursors ox-jira sphinx-doc python-docstring flycheck-pyflakes magit-gh-pulls haskell-mode flycheck-protobuf weechat flycheck-rust rust-mode yaml-mode ac-c-headers helm-flycheck flycheck-gometalinter helm-gtags default-text-scale yasnippet ac-helm color-theme-sanityinc-tomorrow helm-ag helm-projectile rainbow-delimiters paredit color-theme-solarized magit helm go-projectile dired+ git-gutter)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :family "Fira Code" :foundry "CTDB" :slant normal :weight normal :width normal)))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (let ((my-packages
;;        `(dired+ diminish go-mode projectile go-projectile magit mmm-mode rainbow-delimiters smartparens markdown-mode multiple-cursors org paredit)))

;;   (dolist (p my-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p))))

;; my settings broken out into their own packages
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "extras")))

;; utility!
(defun file-if-exists (filename) "return file if it exists, else nil" nil
       (if (file-exists-p filename)
	   filename
	 nil))

;; helm!
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c p p") 'helm-projectile)
(global-set-key (kbd "M-w") 'copy-region-as-kill)
(global-set-key (kbd "C-s") 'helm-occur)
(helm-projectile-on)

(global-set-key (kbd "<f10>") 'org-agenda)


(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(global-set-key (kbd "C-c r") 'revert-buffer)
(dolist (key '("\C-z" "\C-x\C-z")) (global-unset-key key)) ;; I don't want/need a shortcut for suspend-frame

;; Disable pesky auto-fill-mode
(auto-fill-mode -1)
(turn-off-auto-fill)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; Replace selection when you start typing
(delete-selection-mode t)

;; Write backup files to their own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(projectile-global-mode)

(load-theme 'sanityinc-tomorrow-night t)

;; Git-Gutter
(global-git-gutter-mode t)

;; display remaining battery life
(display-battery-mode 1)

;; lisps
(add-hook 'lisp-mode #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode #'rainbow-delimiters-mode)
(add-hook 'common-lisp-mode #'rainbow-delimiters-mode)

;;golang
(setenv "PATH" "$GOPATH/bin:$PATH" t)
(add-hook 'go-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'go-mode-hook '(lambda () (setq tab-width 4)))
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
(-when-let
    ((gorenamefile (seq-find 'file-exists-p (seq-map #'substitute-in-file-name '("$GOPATH/src/golang.org/x/tools/refactor/rename/go-rename.el" "$GOPATH/src/golang.org/x/tools/refactor/rename/rename.el")))))
  (load-file gorenamefile))
;; (-when-let
;;     ((acfile (file-if-exists (substitute-in-file-name "$GOPATH/src/github.com/nsf/gocode/emacs/go-autocomplete.el"))))
;;   (load-file acfile)
;;   (require 'go-autocomplete)
;;   (require 'auto-complete-config)
;;   (ac-config-default)
;;   (add-hook 'go-mode-hook '(lambda () (auto-complete-mode 1)))
;;   (add-hook 'go-mode-hook #'yas-minor-mode)
;;   (message "hooray" nil)
;;   (message "%s" acfile)
;;   )
(let
    ((acfile (file-if-exists (substitute-in-file-name "$GOPATH/src/github.com/nsf/gocode/emacs/go-autocomplete.el"))))
  (when acfile
    (load-file acfile)
    (require 'go-autocomplete)
    (require 'auto-complete-config)
    (ac-config-default)
    (add-hook 'go-mode-hook '(lambda () (auto-complete-mode 1)))
    (add-hook 'go-mode-hook #'yas-minor-mode)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))
(setq flycheck-gometalinter-vendor t)
(setq flycheck-gometalinter-fast t)
(add-hook 'go-mode-hook #'flycheck-mode)

;; rustlang
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;flycheck
 (eval-after-load 'flycheck
   '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; autocomplete
(require 'auto-complete-config)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-completing-map (kbd "C-:") 'ac-complete-with-helm)

;; eshell is best shell
(setenv "CDPATH" ".:$GOPATH/src/github.atl.pdrop.net:$GOPATH/src/github.com/" t)

;; yaml
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; c
(add-hook 'c-mode-hook
	  (lambda ()
	    (add-to-list 'ac-sources 'ac-source-c-headers)
	    (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; protobuf
;; (add-hook 'flycheck-mode-hook 'protobuf-protoc-reporter)
(eval-after-load 'flycheck '(add-to-list 'flycheck-checkers 'protobuf-protoc-reporter t))

;; magit
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; python
(add-hook 'python-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck '(add-to-list 'flycheck-disabled-checkers 'python-flake8))
(eval-after-load 'flycheck '(add-to-list 'flycheck-disabled-checkers 'python-pylint))

;;;;;;;;;;;;;;;;;;
;; key bindings ;;
;;;;;;;;;;;;;;;;;;

(require 'multi-cursor-keybindings)

;; Scroll faster without a mouse
(global-set-key (kbd "C-c C-p") (lambda () (interactive) (previous-line 20)))
(global-set-key (kbd "C-c C-n") (lambda () (interactive) (next-line 20)))

;; multiple-cursors
(global-set-key (kbd "s-.") 'mc/edit-lines)
(global-set-key (kbd "s-d") 'mc/mark-next-like-this)
(setenv "CDPATH" ".:$GOPATH/src/github.atl.pdrop.net:$GOPATH/src/github.com:$GOPATH/src" t)

;; gtags
;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

(require 'erc-customizations)
(server-start)
