
;; Toda la mierda va a ir aquí, maldita configuración custom :(
(setq custom-file "~/.emacs.d/.emacs-custom.el")
;; Comprobación de paquetes lo primero. Nos conectamos tanto a melpa como a melpa-stable
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/"))
  (package-initialize))
;; Ahora el use-package que agilizará la instalación de nuestros paquetes
(unless (featurep 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
;; Es el momento de hacerlo bonito
;;(set-default-font "scientifica")
;;(set-face-attribute 'default nil :height 168)
(tooltip-mode -1)
(tool-bar-mode -1)
(set-window-fringes nil 0 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (toggle-frame-fullscreen)
(setq inhibit-startup-screen t)
(display-time-mode 1)
(setq ring-bell-function 'ignore)
;;(shell-command-to-string "echo -n $(date +%k:%M--%m-%d)")
(display-battery-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode 1))
(use-package telephone-line
  :ensure t) ;; Bueno, esto necesita un repaso gordo. Espero que mole mucho.
(setq telephone-line-subseparator-faces '())
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
(telephone-line-mode t)
;; Tema
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-molokai t)
  :config (doom-themes-org-config)
          (doom-themes-neotree-config))
;; Unas cuantas utilidades para mejorar nuestra experiencia
(use-package windmove
  :ensure t
  :bind (("M-<up>" . windmove-up)
	 ("M-<down>" . windmove-down)
	 ("M-<right>" . windmove-right)
	 ("M-<left>" . windmove-left)))
(use-package ido
  :init (ido-mode))
(use-package neotree
  :ensure t
  :bind (("C-x n" . neotree-toggle)))

(use-package helm
  :init (helm-mode 1)
  :config (require 'helm-config)
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)))

;;(use-package auto-complete
;;  :ensure t
;;  :config (ac-config-default))

(use-package company
  :ensure t
  :init
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Juegos :D
(use-package tetris
  :ensure t
  :bind ("C-t" . tetris))
(use-package typing
  :ensure t )
(use-package poker
  :ensure t )
;; Accesorios varios
(use-package zone-rainbow
  :ensure t
  :bind ("C-z" . zone-rainbow))
(use-package spotify
  :ensure t
  :bind(("C-S-s p" . spotify-playpause)
	("C-S-s n" . spotify-next)
	("C-S-s b" . spotify-previous)
	("C-S-s c" . spotify-current)))
(use-package golden-ratio
  :ensure t
  :config (golden-ratio-mode t))
(use-package rainbow-mode
  :ensure t
  :config (rainbow-mode t))
;; Modos para programar (Aunque no hay nada de programación aquí xD) y demás
(use-package markdown-mode+
  :ensure t)
(use-package flymd
  :ensure t)
(use-package magit
  :ensure t)
;;(use-package auctex
;;  :ensure t)
;;  :config ((setq TeX-auto-save t)
;;	   (setq TeX-parse-self t)))
(put 'upcase-region 'disabled nil)
