;; Toda la mierda va a ir aquí, maldita configuración custom :(
(setq custom-file "~/.emacs.d/.emacs-custom.el")
;; Comprobación de paquetes lo primero. Nos conectamos tanto a melpa como a melpa-stable
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  (package-initialize))
;; Ahora el use-package que agilizará la instalación de nuestros paquetes
(unless (featurep 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Es el momento de hacerlo bonito
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(display-time-mode 1)
(display-battery-mode 1)
(use-package rainbow-delimiters
  :ensure t
  :config (rainbow-delimiters-mode 1))
;; Tema
(use-package cyberpunk-theme
	     :ensure t
	     :init (load-theme 'cyberpunk t))
;; Unas cuantas utilidades para mejorar nuestra experiencia
(use-package windmove
	     :ensure t
	     :bind (("M-<up>" . windmove-up)
		    ("M-<down>" . windmove-down)
		    ("M-<right>" . windmove-right)
		    ("M-<left>" . windmove-left)))
(use-package ido
	     :init (ido-mode))
(use-package auto-complete
	     :ensure t
	     :config (ac-config-default))
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))
(electric-pair-mode 1)
(show-paren-mode 1)
(setq make-backup-files nil)
;; Juegos :D
(use-package tetris
	     :ensure t
	     :bind ("C-t" . tetris))
(use-package typing
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

