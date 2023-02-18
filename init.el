(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/"))
  (package-initialize))

(unless (featurep 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(tooltip-mode -1)
(tool-bar-mode -1)
(set-window-fringes nil 0 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setq make-backup-files nil)
(setq auto-save-default nil)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode 1))

;; Mode line
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode))

(setq doom-modeline-height 10)
(setq doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-lsp t)
(setq doom-modeline-github t)
(setq doom-modeline-github-interval (* 30 60))
(setq find-file-visit-truename t)
(setq column-number-mode t)

(setq global-auto-revert-mode t)

;; Themes
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-plain-dark t)
  :config (doom-themes-org-config)
  (doom-themes-neotree-config))

(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font Mono 13"))

(use-package windmove
  :ensure t
  :bind (("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)
         ("C-<right>" . windmove-right)
         ("C-<left>" . windmove-left)))

;; Thanks to @Ironjanowar for helm config
(use-package helm
  :ensure t
  :init (helm-mode 1)
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)))

(setq helm-split-window-inside-p t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t
      helm-M-x-fuzzy-match t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-." . mc/unmark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-," . mc/unmark-previous-like-this)))

(use-package spotify
  :ensure t
  :bind(("C-S-s p" . spotify-playpause)
        ("C-S-s n" . spotify-next)
        ("C-S-s b" . spotify-previous)
        ("C-S-s c" . spotify-current)))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :bind (("C-x n" . neotree-toggle))
  :config (setq-default neo-show-hidden-files t)
  (setq neo-smart-open t))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Copied from @Ironjanowar
(defun new-scratch-buffer-new-window ()
  "Create a new scratch buffer in a
  new window. I generally take a lot of notes
  in different topics. For each new topic hit
  C-c C-s and start taking your notes.
  Most of these notes don't need to be
  saved but are used like quick post it
  notes."
  (interactive)
  (let (($buf (generate-new-buffer "notes")))
    (split-window-right)
    (other-window 1)
    (balance-windows)
    (switch-to-buffer $buf)
    (org-mode)
    (insert "# Notes\n\n")
    $buf))
(global-set-key
 (kbd "C-c n")
 'new-scratch-buffer-new-window)
(provide 'open-notes)

;; Indent Fucking Whole Buffer (by github.com/skgsergio)
(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Indent buffer: Done."))

(global-set-key "\M-i" 'iwb)

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("C-<tab>" . company-yasnippet))

;; ---------- Programming ---------- ;;
;; Elixir
(use-package elixir-mode
  :ensure t)

;; Yasnippets
(use-package yasnippet
  :ensure t
  :init (yas-global-mode t)
  :bind ("C-<tab>" . yas-expand))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package yasnippet-snippets
  :ensure t)

;; Eglot
(use-package eglot
  :ensure t
  :hook ((js-mode . eglot-ensure))
  :bind (("C-c r" . eglot-rename)
         ("C-c a" . eglot-code-actions)
         ("C-c s d" . eglot-shutdown)
         ("C-c s r" . eglot-reconnect)
         ("C-c l" . flymake-show-buffer-diagnostics)
         ("C-c d" . eldoc-doc-buffer))
  :config
  (setq eglot-confirm-server-initiated-edits nil))
(add-to-list 'eglot-server-programs
             '(elixir-mode "/usr/lib/elixir-ls/language_server.sh"))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; Projectile
(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :init (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-sort-order 'recently-active))
(require 'tramp)

(add-hook 'projectile-after-switch-project-hook 'open-terminal-in-workdir)

(use-package helm-projectile
  :ensure t
  :init (helm-projectile-on))

;; Page-break-lines (dependecy of dashboard)
(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode))

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; emacs daemon
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '((projects . 5)
                        (bookmarks . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-init-info nil)
(setq dashboard-set-footer nil)

;; org-mode configuration
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))

;; v-term
(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :bind ("C-x p s" . vterm-toggle-insert-cd))

(provide 'init)
;;; init.el ends here
