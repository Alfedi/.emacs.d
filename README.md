# Mi configuración de emacs

## Indice
* Instalación de paquetes
  * [Melpa](https://github.com/Alfedi/.emacs.d#melpa)
  * [Use-package](https://github.com/Alfedi/.emacs.d#UsePackage)
* Cosas bonitas :D
  * [Configuración Variada](https://github.com/Alfedi/.emacs.d#configVariada)
  * [Rainbow-delimiters](https://github.com/Alfedi/.emacs.d#rainbow-del)
  * [Telephone-line](https://github.com/Alfedi/.emacs.d#telephone)
  * [El tema](https://github.com/Alfedi/.emacs.d#tema)
* Cosas usables en plan bien
  * [Windmove](https://github.com/Alfedi/.emacs.d#windmove)
  * [Ido](https://github.com/Alfedi/.emacs.d#ido)
  * [Auto-complete](https://github.com/Alfedi/.emacs.d#autocomplete)
  * [Company](https://github.com/Alfedi/.emacs.d#company)
* Juegos ;)
  * [Tetris](https://github.com/Alfedi/.emacs.d#tetris)
  * [Typing of emacs](https://github.com/Alfedi/.emacs.d#typing)
* Accesorios varios
  * [Zone-rainbow](https://github.com/Alfedi/.emacs.d#zone-rainbow)
  * [Spotify](https://github.com/Alfedi/.emacs.d#spotify)
* Modos de programación y demás (Aunque de momento no hay nada de programación)
  * [Markdown](https://github.com/Alfedi/.emacs.d#markdown)

## Instalación de paquetes

### [Melpa](#melpa)
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
### [Use-package](UsePackage)
Esto es la maravilla, nos permite agilizar toda la instalación y demás cosas relacionadas con paquetes. Es la magia.
```emacs-lisp
(unless (featurep 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```

## Cosas bonitas :D

### [Configuración Variada](configVariada)
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

### [Rainbow-delimiters](rainbow-del)
Con este maravilloso paquete tenemos todos los paréntesis, corchetes y llaves de colores.
```emacs-lisp
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode 1))
```

### [Telephone-line](telephone)
Con esta maravilla podemos haccer una powerline mucho más bonita y personalizable, a pesar de que uso uno de los ejemplos del [github del autor original](https://github.com/dbordak/telephone-line).
```emacs-lisp
(use-package telephone-line
  :ensure t) ;; Bueno, esto necesita un repaso gordo. Espero que mole mucho.
(setq telephone-line-subseparator-faces '())
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
(telephone-line-mode t)
```

### [El Tema](tema)
Mi maravilloso tema, no tiene mucho más misterio.
```emacs-lisp
  (use-package cyberpunk-theme
	     :ensure t
	     :init (load-theme 'cyberpunk t))
```
## Cosas usables en plan bien

### [Windmove](windmove)
Con `windmove` podemos cambiar el desplazamiento entre buffers a algo más sencillo que `C-n O`
```emacs-lisp
  (use-package windmove
    :ensure t
    :bind (("M-<up>" . windmove-up)
	   ("M-<down>" . windmove-down)
	   ("M-<right>" . windmove-right)
	   ("M-<left>" . windmove-left)))
```

### [Ido](ido)
A muy groso mod es un autocompletado del minibuffer. Es imprescindible
```emacs-lisp
(use-package ido
  :init (ido-mode))
```

### [Auto-complete](autocomplete)
Junto con `company` es un autocompletado como su nombre indica
```emacs-lisp
(use-package auto-complete
  :ensure t
  :config (ac-config-default))
```

### [Company](company)
```emacs-lisp
(use-package company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))
```

### Varios básicos
Simplemente son unas chorradas que me hacen las cosas más fáciles como el autocierre de paréntesis, el highlight de estos, y la eliminación de los que a mi me parecen tan molestos backups (En proceso de descubir como moverlos a una carpeta)
```emacs-lisp
(electric-pair-mode 1)
(show-paren-mode 1)
(setq make-backup-files nil)
```

## Juegos ;)

### [Tetris](tetris)
Indispensable.
```emacs-lisp
(use-package tetris
  :ensure t
  :bind ("C-t" . tetris))
```

### [Typing of emacs](typing)
Un símil con el typing of the dead original. Está bastante decente (Solo disponible en melpa sin stable).
```emacs-lisp
(use-package typing
  :ensure t )
```

## Accesorios varios

### [Zone-rainbow](zone-rainbow)
Es lo mejorcito de emacs, poder poner el arcoiris en cualquier buffer
```emacs-lisp
(use-package zone-rainbow
  :ensure t
  :bind ("C-z" . zone-rainbow))
```

### [Spotify](spotify)
Este modo es lo más útil que he encontrado nunca, y con los atajos se puede hacer bastante sencillo
```emacs-lisp
(use-package spotify
  :ensure t
  :bind(("C-S-s p" . spotify-playpause)
	("C-S-s n" . spotify-next)
	("C-S-s b" . spotify-previous)
	("C-S-s c" . spotify-current)))
```


## Modos de programación y demás (Sin programación xD)

### [Markdown](markdown)
Como soy un pijo para estas cosas me gusta tener todo lo que vaya a hacer organizadito, y como hago los markdown en emacs me gusta poder usar este modo
```emacs-lisp
(use-package markdown-mode+
  :ensure t)
```
Y para poder visualizarlo en tiempo real uso el flymd, que no está mal :/
```emacs-lisp
(use-package flymd
  :ensure t)
```
