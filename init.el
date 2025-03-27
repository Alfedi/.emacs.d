(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/"))
  (package-initialize))

;; (unless (featurep 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))

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

(setq org-clock-sound "~/.emacs.d/bell.wav")

(setq org-attach-method 'lns)

(require 'ox-md)
(require 'ox-beamer)
(use-package ox-epub
  :ensure t
  :after org
  :init
  (require 'ox-epub))


(setq org-directory (concat (getenv "HOME") "/Documentos/org-notes/roam/"))
(setq org-roam-db-location (concat (getenv "HOME") "/Documentos/org-notes/org-roam.db"))
(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory (file-truename org-directory))
  :init
  (org-roam-db-autosync-enable)
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

(setq org-image-actual-width 700)
(setq org-log-done 'time)

(use-package ob-mermaid
  :ensure t)

(setq org-todo-keywords
      '((sequence "HOLD(h)" "TODO(t)" "DOING" "|" "DONE(d)" "DROP")))

(setq org-todo-repeat-to-state "TODO")

(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

(org-babel-do-load-languages ;; List of available languages to evaluate in org mode
 'org-babel-load-languages
 '((octave . t)
   (mermaid . t)
   (ditaa . t)
   (shell . t)))
(setq org-confirm-babel-evaluate nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-latex-src-block-backend 'engraved)

(use-package org-alert
  :ensure t
  :after org
  :config
  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 300
        org-alert-notify-cutoff 5
        org-alert-notify-after-event-cutoff 10)
  (setq org-alert-notification-title "Agenda")
  (org-alert-enable))


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
  (setq doom-modeline-lsp-icon t)
  (setq find-file-visit-truename t)
  (setq column-number-mode t)
  (setq project-mode-line t)
  (setq doom-modeline-env-version t))

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-monokai-spectrum t)
  :config
  (doom-themes-org-config)
  (doom-themes-neotree-config))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package windmove
  :ensure t
  :bind (("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)
         ("C-<right>" . windmove-right)
         ("C-<left>" . windmove-left)))

(use-package orderless
  :ensure t
  :config
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic-remote partial-completion)))))

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

(which-key-mode)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-." . mc/unmark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-," . mc/unmark-previous-like-this)))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :config
  (setq-default neo-show-hidden-files t)
  (setq neo-smart-open nil))

(use-package magit
  :ensure t
  :bind ("C-x p m" . magit-project-status))

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
  :init (global-company-mode))

(use-package elixir-mode
  :ensure t)

(use-package eglot-java
  :ensure t
  :bind(("C-c e r" . eglot-java-run-main)
        ("C-c e t" . eglot-java-run-test)
        ("C-c e T" . eglot-java-project-build-task)))

(use-package eglot
  :ensure t
  :hook ((before-save . eglot-format)
         (js-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (java-mode . eglot-java-mode)
         (LaTeX-mode . eglot-ensure))
  :bind (("C-c r" . eglot-rename)
         ("C-c e a" . eglot-code-actions)
         ("C-c s s" . eglot-shutdown)
         ("C-c s r" . eglot-reconnect)
         ("C-c l" . consult-flymake)
	 ("C-c L" . flymake-show-project-diagnostics)
         ("C-c d" . eldoc-doc-buffer))
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  (add-to-list 'eglot-server-programs
               '(elixir-mode "/usr/lib/elixir-ls/language_server.sh"))
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments)))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode-hook . flymake-shellcheck-load))

(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode))

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; emacs daemon
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 10)
                          (bookmarks . 7)
                          (agenda . 7)))
  (setq dashboard-week-agenda t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-match-agenda-entry "-TODO=\"HOLD\"-title=\"true\"-CATEGORY=\"Cumpleaños\"-CATEGORY=\"Horario\"")
  (setq dashboard-agenda-prefix-format " %i %-12:c %t %-10s ")
  (setq dashboard-agenda-sort-strategy '(todo-state-down priority-up time-up))
  (setq dashboard-projects-backend 'project-el))

(use-package easy-jekyll
  :ensure t
  :init
  (add-to-list 'exec-path "/home/alfedi/.gem/bin")
  (setq easy-jekyll-basedir "~/Documentos/Trasteando/Alfedi.github.io")
  (setq easy-jekyll-url "https://www.aferrero.boo")
  (setq easy-jekyll-sshdomain "pcera")
  (setq easy-jekyll-root "/home/www/")
  (setq easy-jekyll-previewtime "300"))

(use-package auctex
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-to-list 'exec-path "/usr/bin/vendor_perl") ;; Biber command
  (setq tex-fontify-script nil))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))
