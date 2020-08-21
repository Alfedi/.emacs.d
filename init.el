(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
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
(setq doom-modeline-buffer-file-name-style 'truncate-from-project)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-lsp t)
(setq doom-modeline-github t)
(setq doom-modeline-github-interval (* 30 60))
(setq find-file-visit-truename t)
(setq column-number-mode t)

;; Themes
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-outrun-electric t)
  :config (doom-themes-org-config)
  (doom-themes-neotree-config))

(add-to-list 'default-frame-alist '(font . "Input Mono 10"))
(set-face-attribute 'default nil :family "Input Mono 10")
(set-frame-font "Input Mono 10")


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
(setq helm-boring-buffer-regexp-list (list (rx "*") (rx "acm.org") (rx "universidad.org") (rx "examenes.org") (rx "personal.org") (rx "fiestas.org") (rx "magit")))

(require 'helm-config)
(setq helm-split-window-inside-p t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t
      helm-M-x-fuzzy-match t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(use-package typing
  :ensure t)

(use-package spotify
  :ensure t
  :bind(("C-S-s p" . spotify-playpause)
        ("C-S-s n" . spotify-next)
        ("C-S-s b" . spotify-previous)
        ("C-S-s c" . spotify-current)))

(use-package golden-ratio
  :ensure t
  :config (golden-ratio-mode t))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :bind (("C-x n" . neotree-toggle))
  :config (setq-default neo-show-hidden-files t)
  (setq neo-smart-open t))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t))

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

;; Yasnippets
(use-package yasnippet
  :ensure t
  :init (yas-global-mode t)
  :bind ("C-<tab>" . yas-expand))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package yasnippet-snippets
  :ensure t)

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("C-<tab>" . company-yasnippet))

;; Programming
(use-package elixir-mode
  :ensure t)
;; Apply elixir-format after saving a file
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(use-package alchemist
  :ensure t)

;; Ditaa support for org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates ditaa

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ditaa")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp) ;; pyls (Install with pip)
         (elixir-mode . lsp) ;; elixir-ls (Add language_server.sh to PATH)
	 (rust-mode   . lsp) ;; rls (rustup component add rls rust-analysis rust-src)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :bind ("C-c l i" . lsp-ui-imenu)
  :init (lsp-ui-mode)
  (lsp-ui-doc-mode)
  (setq lsp-ui-doc-delay 1))

(add-hook 'prog-mode-hook 'lsp-ui-sideline-mode)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package flycheck-elixir
  :ensure t)

;; Projectile
(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :init (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-sort-order 'recently-active))

(defun open-terminal-in-workdir ()
  "Function to open terminal in the project root."
  (interactive)
  (let ((workdir (if (projectile-project-root)
                     (projectile-project-root)
                   default-directory)))
    (call-process-shell-command
     (concat "guake -e " workdir) nil 0)))

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
                        (agenda . 7)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-init-info nil)
(setq dashboard-set-footer nil)

;; For privacy reasons I set the org-gcal configuration in another file. You can see how to configure here: https://github.com/myuhe/org-gcal.el
(load-file "~/.emacs.d/cal.el")

(use-package calfw
  :ensure t
  :config (require 'calfw)
  (require 'calfw-org)
  :bind ("C-c c" . cfw:open-org-calendar)
  :init (setq calendar-week-start-day 1)
  (setq calendar-month-name-array
        ["Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio" "Julio" "Agosto" "Septiembre" "Octubre" "Noviembre" "Diciembre"])
  (setq calendar-day-name-array
        ["Domingo" "Lunes" "Martes" "Miércoles" "Jueves" "Viernes" "Sábado"])
  (setq cfw:display-calendar-holidays nil))

