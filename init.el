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

(tooltip-mode 0)
(tool-bar-mode 0)
(set-window-fringes nil 0 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq global-auto-revert-mode t)
(add-to-list 'default-frame-alist '(font . "Mononoki Nerd Font Mono 15"))
(set-frame-font "Mononoki Nerd Font Mono 15" nil t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 10)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github t)
  (setq doom-modeline-github-interval (* 30 60))
  (setq find-file-visit-truename t)
  (setq column-number-mode t))

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-ir-black t)
  :config
  (doom-themes-org-config)
  (doom-themes-neotree-config))

(use-package windmove
  :ensure t
  :bind (("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)
         ("C-<right>" . windmove-right)
         ("C-<left>" . windmove-left)))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list . consult-preview-at-point-mode))

(use-package embark
  :ensure t
  :bind (("M-." . embark-act)
         ("C-;" . embark-dwim) ;; good alternative: M-.
         ("C-h B" . embark-bindings)))  ;; alternative for `describe-bindings'

(use-package embark-consult
  :ensure t)

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-." . mc/unmark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-," . mc/unmark-previous-like-this)))

(use-package spotify
  :ensure t
  :bind (("C-S-s p" . spotify-playpause)
         ("C-S-s n" . spotify-next)
         ("C-S-s b" . spotify-previous)
         ("C-S-s c" . spotify-current)))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :bind (("C-x n" . neotree-toggle))
  :config
  (setq-default neo-show-hidden-files t)
  (setq neo-smart-open t))

(use-package magit
  :ensure t
  :bind ("C-x p m" . magit-project-status))

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

(use-package elixir-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :init (yas-global-mode t)
  :bind ("C-<tab>" . yas-expand))

(use-package yasnippet-snippets
  :ensure t)

(use-package eglot
  :ensure t
  :hook ((before-save . eglot-format)
	 (js-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (shell-script-mode . eglot-ensure))
  :bind (("C-c r" . eglot-rename)
         ("C-c a" . eglot-code-actions)
         ("C-c s d" . eglot-shutdown)
         ("C-c s r" . eglot-reconnect)
         ("C-c l" . flymake-show-buffer-diagnostics)
         ("C-c d" . eldoc-doc-buffer))
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  (add-to-list 'eglot-server-programs
               '(elixir-mode "/usr/lib/elixir-ls/language_server.sh")))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode-hook . flymake-shellcheck-load))

(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode))

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; emacs daemon
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 10)
                          (bookmarks . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-projects-backend 'project-el))
