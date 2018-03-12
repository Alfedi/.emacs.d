# Mi configuración de emacs

## Indice
* Instalación de paquetes
  * [Melpa][Melpa]
  * [Use-package][UsePackage]
* Cosas bonitas :D
  * [Configuración Variada][ConfigVar]
  * [Rainbow-delimiters][rainbowdel]
  * [Telephone-line][telephone]
  * [El tema][tema]
* Cosas usables en plan bien
  * [Windmove][windmove]
  * [Ido][ido]
  * [Auto-complete][complete]
  * [Company][company]
* Juegos ;)
  * [Tetris][tetris]
  * [Typing of emacs]
* Accesorios varios
  * [Zone-rainbow][zrainbow]
  * [Spotify][spotify]


## Instalación de paquetes

### [Melpa][Melpa]
Lo primero de todo es establecer nuestros gestores de paquetes y para ello realizamos la comprobación y conexión a `melpa-stable`.
```emacs-lisp
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  (package-initialize))
```
