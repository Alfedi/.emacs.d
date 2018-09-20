# Mi configuración de emacs

## Indice
* Instalación de paquetes
  * [Melpa](https://github.com/Alfedi/.emacs.d#melpa)
  * [Use-package](https://github.com/Alfedi/.emacs.d#use-package)
* Cosas bonitas :D
  * [Configuración Variada](https://github.com/Alfedi/.emacs.d#configuración-variada)
  * [Rainbow-delimiters](https://github.com/Alfedi/.emacs.d#rainbow-delimiters)
  * [Telephone-line](https://github.com/Alfedi/.emacs.d#telephone-line)
  * [El tema](https://github.com/Alfedi/.emacs.d#el-tema)
* Cosas usables en plan bien
  * [Windmove](https://github.com/Alfedi/.emacs.d#windmove)
  * [Helm](https://github.com/Alfedi/.emacs.d#helm)
  * [Auto-complete](https://github.com/Alfedi/.emacs.d#auto-complete)
  * [Company](https://github.com/Alfedi/.emacs.d#company)
  * [Varios básicos](https://github.com/Alfedi/.emacs.d#varios-básicos)
  * [Neotree](https://github.com/Alfedi/.emacs.d#neotree)
* Juegos ;)
  * [Tetris](https://github.com/Alfedi/.emacs.d#tetris)
  * [Typing of emacs](https://github.com/Alfedi/.emacs.d#typing-of-emacs)
  * [Poker](https://github.com/Alfedi/.emacs.d#poker)
* Accesorios varios
  * [Zone-rainbow](https://github.com/Alfedi/.emacs.d#zone-rainbow)
  * [Spotify](https://github.com/Alfedi/.emacs.d#spotify)
  * [Golden-ratio](https://github.com/Alfedi/.emacs.d#golden-ratio)
* Modos de programación y demás (Aunque de momento no hay nada de programación)
  * [Markdown](https://github.com/Alfedi/.emacs.d#markdown)

## Instalación de paquetes

### Melpa
Lo primero de todo es establecer nuestros gestores de paquetes y para ello realizamos la comprobación y conexión a `melpa-stable`.
```emacs-lisp
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/"))
  (package-initialize))
```
### Use-package
Esto es la maravilla, nos permite agilizar toda la instalación y demás cosas relacionadas con paquetes. Es la magia.
```emacs-lisp
(unless (featurep 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```

## Cosas bonitas :D

### Configuración Variada
Esto no son más que pequeños ajustes que quitan los menús, la barra lateral, quitamos la página del startup y mostramos la batería y el reloj en la powerline.
```emacs-lisp
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-fullscreen)
(setq inhibit-startup-screen t)
(display-time-mode 1)
(display-battery-mode 1)
```

### Rainbow-delimiters
Con este maravilloso paquete tenemos todos los paréntesis, corchetes y llaves de colores.
```emacs-lisp
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode 1))
```

### Telephone-line
Con esta maravilla podemos haccer una powerline mucho más bonita y personalizable, a pesar de que uso uno de los ejemplos del [github del autor original](https://github.com/dbordak/telephone-line).
```emacs-lisp
(use-package telephone-line
  :ensure t)
(setq telephone-line-subseparator-faces '())
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
(telephone-line-mode t)
```

### El Tema
Cambio de tema en favor de los doom-themes, mucho más bonitos que el antiguo Cyberpunk
```emacs-lisp
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-molokai t)
  :config (doom-themes-org-config)
          (doom-themes-neotree-config))
```
## Cosas usables en plan bien

### Windmove
Con `windmove` podemos cambiar el desplazamiento entre buffers a algo más sencillo que `C-n O`.
```emacs-lisp
  (use-package windmove
    :ensure t
    :bind (("M-<up>" . windmove-up)
	   ("M-<down>" . windmove-down)
	   ("M-<right>" . windmove-right)
	   ("M-<left>" . windmove-left)))
```

### Helm
Cambio a viejo Ido por esta pequeña maravilla que me hace la vida un poquitín más fácil
```emacs-lisp
(use-package helm
  :init (helm-mode 1)
  :config (require 'helm-config)
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)))
```

### Auto-complete
Junto con `company` es un autocompletado como su nombre indica. (Actualmente desactivado)
```emacs-lisp
(use-package auto-complete
  :ensure t
  :config (ac-config-default))
```

### Company
```emacs-lisp
(use-package company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))
```

### Varios básicos
Simplemente son unas chorradas que me hacen las cosas más fáciles como el autocierre de paréntesis, el highlight de estos, y el almacenamiento de los backups para que no estén por ahí molestando.
```emacs-lisp
(electric-pair-mode 1)
(show-paren-mode 1)
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
```

### Neotree
Un útil gestor de archivos integrado que nos permite ver la estructura de nuestro programa de forma muy sencilla sin salir de emacs
```emacs-lisp
(use-package neotree
  :ensure t
  :bind (("C-x n" . neotree-toggle)))
```

## Juegos ;)

### Tetris
Indispensable.
```emacs-lisp
(use-package tetris
  :ensure t
  :bind ("C-t" . tetris))
```

### Typing of emacs
Un símil con el typing of the dead original. Está bastante decente (Solo disponible en melpa sin stable).
```emacs-lisp
(use-package typing
  :ensure t )
```

### Poker
Uno de los pequeños placeres de la vida ;)
```emacs-lisp
(use-package poker
:ensure t )
```
## Accesorios varios

### Zone-rainbow
Es lo mejorcito de emacs, poder poner el arcoiris en cualquier buffer.
```emacs-lisp
(use-package zone-rainbow
  :ensure t
  :bind ("C-z" . zone-rainbow))
```

### Spotify
Este modo es lo más útil que he encontrado nunca, y con los atajos se puede hacer bastante sencillo.
```emacs-lisp
(use-package spotify
  :ensure t
  :bind(("C-S-s p" . spotify-playpause)
	("C-S-s n" . spotify-next)
	("C-S-s b" . spotify-previous)
	("C-S-s c" . spotify-current)))
```
## Golden-ratio
Gracias a este paquete podemos hacer focus en un buffer en concreto cuando tenemos muchos abiertos.
```emacs-lisp
(use-package golden-ratio
  :ensure t
  :config (golden-ratio-mode t))
```

## Modos de programación y demás (Sin programación xD)

### Markdown
Como soy un pijo para estas cosas me gusta tener todo lo que vaya a hacer organizadito, y como hago los markdown en emacs me gusta poder usar este modo.
```emacs-lisp
(use-package markdown-mode+
  :ensure t)
```
Y para poder visualizarlo en tiempo real uso el flymd.
```emacs-lisp
(use-package flymd
  :ensure t)
```
