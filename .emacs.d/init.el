;; emacs configuration

;; =============================================================================
;; general settings

;; less annoying
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (getenv "USER"))
(fset 'yes-or-no-p 'y-or-n-p)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(delete-selection-mode t)
(global-subword-mode t)

;; useful when working with VCS
(global-auto-revert-mode t)

;; no fscking tabs by default
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; be nice
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; better font
(add-to-list 'default-frame-alist '(font . "Menlo-14"))

;; keybindings
(global-set-key (kbd "C-x ^") 'join-line)
;; better buffer navigation
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)

;; =============================================================================
;; packages

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(unless package-archive-contents
  (message "Refreshing package database...")
  (package-refresh-contents)
  (message " done"))

(defun package-install-if-needed (p)
  "Installs package unless last version is already installed."
  (unless (package-installed-p p)
    (message "Installing %s..." p)
    (package-install p)
    (message "Finished installing %s" p)))

;; path
(package-install-if-needed 'exec-path-from-shell)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; theme
(package-install-if-needed 'zenburn-theme)
(load-theme 'zenburn t)

;; ido
(require 'ido)
(ido-mode)

(package-install-if-needed 'flx-ido)
(require 'flx-ido)
(setq ido-everywhere nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-faces t)

(package-install-if-needed 'ido-vertical-mode)
(require 'ido-vertical-mode)
(ido-vertical-mode)

(package-install-if-needed 'ido-ubiquitous)
(require 'ido-ubiquitous)
(ido-ubiquitous)

;; smex
(package-install-if-needed 'smex)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;; projectile
(package-install-if-needed 'projectile)
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching nil)
(global-set-key (kbd "C-c f") 'projectile-find-file)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-ignore-buffers-re "^\\*")

;; expand-region
(package-install-if-needed 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; change-inner
(package-install-if-needed 'change-inner)
(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;; git
(package-install-if-needed 'magit)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; parens
(package-install-if-needed 'paredit)
(require 'paredit)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(package-install-if-needed 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; clojure
(package-install-if-needed 'clojure-mode)
(require 'clojure-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(define-clojure-indent
  ;; clojure 1.6 features
  (cond-> 'defun)
  ;; compojure
  (defroutes 'defun)
  (routes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (OPTIONS 2)
  (context 'defun)
  ;; core.async
  (go-loop 2)
  ;; carmine
  (wcar* 'defun)
  (wcar 'defun))

(package-install-if-needed 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(setq cider-repl-print-length 200
      cider-repl-history-file "~/.emacs.d/cider-history")

;; js
(package-install-if-needed 'js2-mode)
(setq js2-basic-offset 2
      js2-indent-on-enter-key t
      js2-enter-indents-newline t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.bowerrc$" . js2-mode))

;; ruby
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; yaml
(package-install-if-needed 'yaml-mode)

;; erlang
(package-install-if-needed 'erlang)

;; elixir
(package-install-if-needed 'elixir-mode)

;; go
(package-install-if-needed 'go-mode)

;; markdown
(package-install-if-needed 'markdown-mode)
(require 'markdown-mode)
(setq markdown-command "multimarkdown")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; haskell
(package-install-if-needed 'haskell-mode)

;; php
(package-install-if-needed 'php-mode)

;; scala
(package-install-if-needed 'scala-mode2)
(package-install-if-needed 'sbt-mode)

;; =============================================================================
;; random goodies

(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'uncomment-region)
(global-set-key [f5] 'indent-region)
;; these not woring in iTerm2 for some reason
(global-set-key (kbd "M-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-<up>") 'shrink-window)
(global-set-key (kbd "M-s-<down>") 'enlarge-window)

;; switch on hs-minor-mode for the following to work
(global-set-key [f9] 'hs-toggle-hiding)

;; custom functions
(defun reindent-whole-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min)
		 (point-max)))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc (lambda (buffer)
	  (kill-buffer buffer))
	(buffer-list))
  (delete-other-windows))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and the file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(file-name (buffer-file-name)))
    (if (not (and file-name (file-exists-p file-name)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " file-name)))
	(cond ((get-buffer new-name)
	       (error "A buffer named '%s' already exists!" new-name))
	      (t
	       (rename-file file-name new-name t)
	       (rename-buffer new-name)
	       (set-visited-file-name new-name)
	       (set-buffer-modified-p nil)
	       (message "File '%s' successfully renamed to '%s'" name
			(file-name-nondirectory new-name))))))))
