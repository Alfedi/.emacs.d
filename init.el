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
(display-time-mode 1)
(display-battery-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(setq make-backup-files nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode 1))

(use-package telephone-line
  :ensure t)
(setq telephone-line-subseparator-faces '())
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
(telephone-line-mode t)

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-molokai t)
  :config (doom-themes-org-config)
  (doom-themes-neotree-config))

(add-to-list 'default-frame-alist '(font . "Hack 13"))
(set-face-attribute 'default nil :family "Hack 13")
(set-default-font "Hack 13")

(use-package windmove
  :ensure t
  :bind (("C-<up>" . windmove-up)
	 ("C-<down>" . windmove-down)
	 ("C-<right>" . windmove-right)
	 ("C-<left>" . windmove-left)))

;; Thanks to @Ironjanowar for helm config
(use-package helm
  :ensure t
  :init (helm-mode)
  :config (require 'helm-config
	   (setq helm-split-window-in-side-p t
		 helm-buffers-fuzzy-matching t 
		 helm-recentf-fuzzy-match t
		 helm-move-to-line-cycle-in-source t
		 helm-M-x-fuzzy-match))
  :bind (("C-x C-f" . helm-find-files)	 ("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-c g" . helm-google-suggest)))

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
  :bind (("C-x n" . neotree-toggle)))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t))

(use-package zone-rainbow
  :ensure t
  :bind ("C-z" . zone-rainbow))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package multi-term
  :ensure t)

(use-package sudo-edit
  :ensure t)

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
 (kbd "C-c C-n")
 'new-scratch-buffer-new-window)
(provide 'open-notes)

;; Indent Fucking Whole Buffer (by github.com/skgsergio)
(defun iwb ()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Indent buffer: Done.")
  )

(global-set-key "\M-i" 'iwb)
