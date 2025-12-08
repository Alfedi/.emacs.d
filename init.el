;; straight.el configuration

(setq straight-repository-branch "main")
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ;; use-package integration with straight

(straight-use-package 'org) ;; Needed org mode prior load


;; emacs appareance
(tooltip-mode 0)
(tool-bar-mode 0)
(set-window-fringes nil 0 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(setq electric-pair-mode 1)
(setq show-paren-mode 1)
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(add-to-list 'default-frame-alist '(font . "Mononoki Nerd Font Mono 15"))
(set-frame-font "Mononoki Nerd Font Mono 15" nil t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-monokai-spectrum t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 10)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-lsp-icon t)
  (setq find-file-visit-truename t)
  (setq column-number-mode t)
  (setq project-mode-line t)
  (setq doom-modeline-env-version t))

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

(straight-use-package 'all-the-icons)

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; emacs daemon
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 10)
                          (bookmarks . 7)))
  (setq dashboard-week-agenda t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-match-agenda-entry "-TODO=\"HOLD\"-title=\"true\"-CATEGORY=\"Cumpleaños\"-CATEGORY=\"Horario\"")
  (setq dashboard-agenda-prefix-format " %i %-12:c %t %-10s ")
  (setq dashboard-agenda-sort-strategy '(todo-state-down priority-up time-up))
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  )



;; emacs misc
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq global-auto-revert-mode t)
(which-key-mode)
(setq tab-always-indent 'complete)

(use-package windmove
  :straight t
  :bind (("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)
         ("C-<right>" . windmove-right)
         ("C-<left>" . windmove-left)))

(use-package vertico
  :straight t
  :config
  (vertico-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))
				   (eglot (styles orderless))
				   (eglot-capf (styles orderless)))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t)
  :config
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

(use-package consult
  :straight t
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
  :straight t
  :bind (("M-." . embark-act)
         ("C-;" . embark-dwim) ;; good alternative: M-.
         ("C-h B" . embark-bindings)))  ;; alternative for `describe-bindings'

(straight-use-package 'embark-consult)

(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-." . mc/unmark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-," . mc/unmark-previous-like-this)))

;; Indent Fucking Whole Buffer (by github.com/skgsergio)
(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Indent buffer: Done."))

(global-set-key "\M-i" 'iwb)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (add-hook 'eshell-mode-hook (lambda ()
                              (setq-local corfu-auto nil)
                              (corfu-mode)))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :custom
  (corfu-preselect 'prompt)
  (corfu-cycle t)
  (corfu-popupinfo-delay 0))

(use-package nerd-icons-corfu
  :straight t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


;; Org settings
(setq org-todo-keywords
      '((sequence "HOLD(h)" "TODO(t)" "DOING" "|" "DONE(d)" "DROP")))

(setq org-todo-repeat-to-state "TODO")
(setq org-agenda-files
      '("~/Documentos/org-notes/Tareas.org" "~/Documentos/org-notes/Agenda.org"))
(setq org-agenda-diary-file "~/Documentos/org-notes/Agenda.org")
(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))

(add-hook 'org-mode-hook 'toggle-truncate-lines)
(add-hook 'org-mode-hook 'toggle-word-wrap)
(setq org-id-link-to-org-use-id t)
(setq org-return-follows-link t)

(setq org-html-doctype "html5")

(setq org-clock-mode-line-total 'current)

(setq org-attach-method 'lns)

(require 'ox-md)
(require 'ox-beamer)
(straight-use-package 'ox-epub)

(setq org-directory (concat (getenv "HOME") "/Documentos/org-notes/roam/"))

(use-package org-roam
  :straight t
  :init
  (require 'org-roam)
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-db-autosync-enable)
  (setq org-roam-db-location (concat (getenv "HOME") "/Documentos/org-notes/org-roam.db"))
  :bind(("C-c n f" . org-roam-node-find)
        ("C-c n r" . org-roam-node-random)
        (:map org-mode-map
              (("C-c n i" . org-roam-node-insert)
               ("C-c n o" . org-id-get-create)
               ("C-c n t" . org-roam-tag-add)
               ("C-c n a" . org-roam-alias-add)
               ("C-c n l" . org-roam-buffer-toggle)
               ("C-c s" . org-store-link)))))

(setq calendar-holidays
      '(
        ;; State holidays
        (holiday-fixed 1 1 "Año Nuevo")
        (holiday-fixed 1 6 "Día de Reyes")
        (holiday-fixed 3 19 "Día del padre")
        (holiday-fixed 3 26 "Cambio de horario de verano")
        (holiday-fixed 5 1 "Día del trabajador")
        (holiday-fixed 5 2 "Día de la Comunidad de Madrid")
        (holiday-fixed 5 15 "Día de San Isidro")
        (holiday-fixed 6 24 "San Juan")
        (holiday-fixed 8 15 "Asuncion de la Virgen")
        (holiday-fixed 10 12 "Día de la Hispanidad")
        (holiday-fixed 10 29 "Cambio de horario de invierno")
        (holiday-fixed 11 1 "Todos los Santos")
        (holiday-fixed 11 9 "La Almudena")
        (holiday-fixed 12 6 "Día de la Constitucion")
        (holiday-fixed 12 8 "Inmaculada Concepción")
        (holiday-fixed 12 24 "Nochebuena")
        (holiday-fixed 12 25 "Navidad")
        (holiday-fixed 12 31 "Nochevieja")
        ;; floated holidays
        (holiday-easter-etc  -3 "Jueves Santo")
        (holiday-easter-etc  -2 "Viernes Santo")
        (holiday-easter-etc  0 "Domingo de Ramos")
        (holiday-easter-etc  1 "Lunes de Pascua")
        (holiday-easter-etc 50 "Lunes de pentecostes")))

(setq org-agenda-include-diary t)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Documentos/org-notes/Tareas.org")
         "* TODO %? \n")
        ("n" "Proyect Note" entry (file "~/Documentos/org-notes/Proyectos.org")
         "* %? %^g \n %a")
        ("x" "Examen" plain (file+headline "~/Documentos/org-notes/Agenda.org" "Examenes")
         "%\\%(org-date %^{date}) Examen de %?")
        ("e" "Evento" plain (file+headline "~/Documentos/org-notes/Agenda.org" "Eventos")
         "%\\%(org-date %^{date}) %?")))

(add-to-list
 'org-structure-template-alist
 '("A" . "quote
\\fontfamily{cmss}\\selectfont"))

(setq org-image-actual-width 700)
(setq org-log-done 'time)

(straight-use-package 'ob-mermaid)

(org-babel-do-load-languages ;; List of available languages to evaluate in org mode
 'org-babel-load-languages
 '((octave . t)
   (mermaid . t)
   (ditaa . t)
   (shell . t)
   (python . t)))
(setq org-confirm-babel-evaluate nil)

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-latex-src-block-backend 'engraved)
(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("pdflatex")))

(use-package org-alert
  :straight t
  :after org
  :config
  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 300
        org-alert-notify-cutoff 5
        org-alert-notify-after-event-cutoff 10)
  (setq org-alert-notification-title "Agenda")
  (org-alert-enable))


;; Programming config
(use-package magit
  :straight t
  :bind ("C-x p m" . magit-project-status))

(require 'treesit)
;; (straight-use-package 'tree-sitter)
;; (straight-use-package tree-sitter-langs)
;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)
;;         (java-mode . java-ts-mode)
;;         (dockerfile-mode . dockerfile-ts-mode)
;;         (shell-script-mode . bash-ts-mode)))


;;;; Language modes
(straight-use-package 'elixir-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'json-mode)
(straight-use-package 'arduino-mode)
(use-package platformio-mode
  :straight t
  :init
  :hook (arduino-mode-hook . platformio-conditionally-enable))

;;;; eglot
(require 'eglot)
(add-hook 'before-save 'eglot-format)
(add-hook 'js-mode 'eglot-ensure)
(add-hook 'python-mode 'eglot-ensure)
(add-hook 'sh-mode 'eglot-ensure)
(add-hook 'elixir-mode 'eglot-ensure)
(add-hook 'c-mode 'eglot-ensure)
(add-hook 'c++-mode 'eglot-ensure)
(add-hook 'java-mode 'eglot-java-mode)
(add-hook 'LaTeX-mode 'eglot-ensure)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c e a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c s s") 'eglot-shutdown)
(define-key eglot-mode-map (kbd "C-c s r") 'eglot-reconnect)
(define-key eglot-mode-map (kbd "C-c l") 'consult-flymake)
(define-key eglot-mode-map (kbd "C-c L") 'flymake-show-project-diagnostics)
(define-key eglot-mode-map (kbd "C-c d") 'eldoc-doc-buffer)

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

(setq eglot-confirm-server-initiated-edits nil)
(add-to-list 'eglot-server-programs
             '(elixir-mode "/usr/lib/elixir-ls/language_server.sh"))
(cl-defmethod eglot-execute-command
  (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))

(use-package eglot-java
  :straight t
  :bind(("C-c e r" . eglot-java-run-main)
        ("C-c e t" . eglot-java-run-test)
        ("C-c e T" . eglot-java-project-build-task)))


;; misc packages
(straight-use-package 'pdf-tools)

(use-package auctex
  :straight t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-to-list 'exec-path "/usr/bin/vendor_perl") ;; Biber command
  (setq tex-fontify-script nil))
