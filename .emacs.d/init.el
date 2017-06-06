
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; make sure use-package is installed
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p `use-package))
  (package-install 'use-package))

;; my settings broken out into their own packages
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "extras")))

;; utility!
(defun file-if-exists (filename) "return file if it exists, else nil" nil
       (if (file-exists-p filename)
	   filename
	 nil))

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-c p p") 'helm-projectile)
  (global-set-key (kbd "M-w") 'copy-region-as-kill)
  (global-set-key (kbd "C-s") 'helm-occur)
  (setq helm-mode-fuzzy-match t)
  (setq helm-gtags-path-style (quote relative))
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-ignore-case t)
  (use-package helm-ag
    :ensure t)
  (use-package projectile
    :ensure t
    :config
    (projectile-global-mode)
    (use-package helm-projectile
      :ensure t
      :config
      (helm-projectile-on)))
  (use-package flycheck
    :ensure t
    :bind (:map flycheck-mode-map
		("C-c ! h" . helm-flycheck))
    :config
    (setq flycheck-flake8-error-level-alist
	  (quote (("^E9.*$" . error)
		  ("^F82.*$" . error)
		  ("^F83.*$" . error)
		  ("^D.*$" . info)
		  ("^N.*$" . info)
		  ("^E501$" . info))))
    (setq flycheck-flake8-maximum-complexity 10)
    (setq flycheck-flake8-maximum-line-length 120)
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    (add-to-list 'flycheck-disabled-checkers 'python-pylint)
    ;; (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)
    ))

;; autocomplete
(use-package auto-complete
  :ensure t
  :bind (:map ac-completing-map
	 ("C-:" . ac-complete-with-helm)
	 :map ac-complete-mode-map
	 ("C-:" . ac-complete-with-helm)
	 :map ac-mode-map
	 ("C-:" . ac-complete-with-helm))
  :config
  (require 'auto-complete-config)
  (use-package ac-helm
    :ensure t
    :config
    (global-set-key (kbd "C-:") 'ac-complete-with-helm)
    (define-key ac-completing-map (kbd "C-:") 'ac-complete-with-helm)))

(global-set-key (kbd "<f10>") 'org-agenda)
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

;; (load-theme 'sanityinc-tomorrow-night t)
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

;; Git-Gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

;; display remaining battery life
(display-battery-mode 1)

;; lisps
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode #'rainbow-delimiters-mode)
  (add-hook 'common-lisp-mode #'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode #'paredit-mode)
  (add-hook 'common-lisp-mode #'paredit-mode))

;;golang
(use-package go-mode
  :ensure t
  :init
  (setenv "PATH" "$GOPATH/bin:$PATH" t)
  (setenv "CDPATH" ".:$GOPATH/src/github.atl.pdrop.net:$GOPATH/src/github.com/" t)
  :config
  (use-package yasnippet :ensure t) ;; required for go-autocomplete
  (use-package go-impl :ensure t)
  (use-package go-rename :ensure t)
  (use-package go-guru
    :ensure t
    :config
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))
  (add-hook 'go-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
  (add-hook 'go-mode-hook '(lambda () (setq tab-width 4)))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (let
    ((acfile (file-if-exists (substitute-in-file-name "$GOPATH/src/github.com/nsf/gocode/emacs/go-autocomplete.el"))))
  (when acfile
    (load-file acfile)
    (require 'go-autocomplete)
    (require 'auto-complete-config)
    (ac-config-default)
    (add-hook 'go-mode-hook '(lambda () (auto-complete-mode 1)))
    (add-hook 'go-mode-hook #'yas-minor-mode)))
  (use-package flycheck-gometalinter
    :ensure t
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))
    (setq flycheck-gometalinter-vendor t)
    (setq flycheck-gometalinter-fast t)
    (add-hook 'go-mode-hook #'flycheck-mode))
  (use-package helm-go-package
    :ensure t
    :config
    (eval-after-load 'go-mode
      '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))
  (use-package go-scratch :ensure t))

(use-package company
  :ensure t
  :config
  (use-package helm-company
    :ensure t
    :config
    (eval-after-load 'helm-mode
      '(progn
	 (define-key company-mode-map (kbd "C-:") 'helm-company)
	 (define-key company-active-map (kbd "C-:") 'helm-company))))
  (use-package rust-mode
    :ensure t
    :config
    (add-hook 'rust-mode-hook #'rust-enable-format-on-save)
    (use-package racer
      :ensure t
      :bind (:map rust-mode-map
		  ("TAB" . company-indent-or-complete-common))
      :config
      (add-hook 'rust-mode-hook 'company-mode)
      (add-hook 'rust-mode-hook 'racer-mode)
      (add-hook 'racer-mode-hook 'eldoc-mode)
      )
    (use-package flycheck-rust
      :ensure t
      :config
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
      (add-hook 'rust-mode-hook #'flycheck-mode)
      )
    (use-package rust-playground :ensure t)
    (use-package cargo :ensure t)
    ))

(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))

;; yaml
(use-package yaml-mode
  :ensure t
  :bind (:map yaml-mode-map
	 ("\C-m" . newline-and-indent))
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; toml
(add-to-list 'auto-mode-alist '("\\.toml\\'" . conf-unix-mode))

;; c
(add-hook 'c-mode-hook
	  (lambda ()
	    (add-to-list 'ac-sources 'ac-source-c-headers)
	    (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; docker
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )

;; protobuf
(use-package protobuf-mode
  :ensure t
  :config
  (eval-after-load 'flycheck '(add-to-list 'flycheck-checkers 'protobuf-protoc-reporter t)))

;; magit
(use-package magit
  :ensure t
  :config
  (use-package magit-gh-pulls
    :ensure t
    :config
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))

;; python
(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package sphinx-doc
  :ensure t
  :config
  (add-hook 'python-mode-hook 'sphinx-doc-mode))

;; haskell
(use-package flycheck-haskell
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
  (eval-after-load 'haskell-mode-hook 'flycheck-mode))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-line-mode)
  (global-hl-todo-mode t)
  (define-key hl-todo-mode-map (kbd "C-c C-t p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c C-t n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c C-t o") 'hl-todo-occur))

;;;;;;;;;;;;;;;;;;
;; key bindings ;;
;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure t
  :bind
  (("H-d" . mc/mark-next-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C-c m l" . mc/edit-lines)
   ("s-d" . mc/mark-next-like-this)))

;; (require 'multi-cursor-keybindings)

;; Scroll faster without a mouse
(global-set-key (kbd "C-c C-p") (lambda () (interactive) (next-line -20)))
(global-set-key (kbd "C-c C-n") (lambda () (interactive) (next-line 20)))

;; multiple-cursors
(setenv "CDPATH" ".:$GOPATH/src/github.atl.pdrop.net:$GOPATH/src/github.com:$GOPATH/src" t)

(use-package "helm-gtags"
  :ensure t
  :bind (:map helm-gtags-mode-map
	      ("M-." . helm-gtags-dwim)
	      ("M-t" . helm-gtags-find-tag)
	      ("M-r" . helm-gtags-find-rtag)
	      ("M-s" . helm-gtags-find-symbol)
	      ("M-g M-p" . helm-gtags-parse-file)
	      ("C-c <" . helm-gtags-previous-history)
	      ("C-c >" . helm-gtags-next-history)
	      ("M-," . helm-gtags-pop-stack))
  :init
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (add-hook 'python-mode-hook 'helm-gtags-mode))


;; (require 'erc-customizations)

;; org-mode

(require 'org)
(require 'ob-python)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (haskell . t)
     (dot . t)
     (ditaa . t)))

(defun cb/read-lines (filePath)
  "Return a list of lines from the file at filePath"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun cb/read-lines-or-nil (filePath)
  "Return list of lines from the file at filePath, or nil if it's unreadable"
  (when (file-readable-p filePath)
    (cb/read-lines filePath)))

(use-package org-gcal
  :ensure t
  :config
  (let ((gcal-key (cb/read-lines-or-nil "~/.gcal.key")))
    (setq org-gcal-client-id (nth 0 gcal-key)
	  org-gcal-client-secret (nth 1 gcal-key)))
  (setq org-gcal-file-alist '(("cblades@pindrop.com" . "~/org/gcal.org")))

  ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  )

(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/agenda.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("J" "Jenkins" entry (file+datetree "~/org/jenkins_shenanigans.org")
	 "* Today, Jenkins %? :jenkins:\n  %t\n  %i\n"
	 )))

(use-package ox-html5slide
  :ensure t)

(use-package ox-ioslide
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package groovy-mode
  :ensure t
  :mode (("Jenkinsfile" . groovy-mode)))

(use-package emms
  :ensure t
  :config
  (emms-standard)
  (emms-default-players))

;; (server-start)
