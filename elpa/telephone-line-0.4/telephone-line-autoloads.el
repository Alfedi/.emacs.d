;;; telephone-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "telephone-line" "telephone-line.el" (23198
;;;;;;  63961 962429 610000))
;;; Generated autoloads from telephone-line.el

(defvar telephone-line-mode nil "\
Non-nil if Telephone-Line mode is enabled.
See the `telephone-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telephone-line-mode'.")

(custom-autoload 'telephone-line-mode "telephone-line" nil)

(autoload 'telephone-line-mode "telephone-line" "\
Toggle telephone-line on or off.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "telephone-line-config" "telephone-line-config.el"
;;;;;;  (23198 63961 886430 161000))
;;; Generated autoloads from telephone-line-config.el

(autoload 'telephone-line-evil-config "telephone-line-config" "\
A simple default for using telephone-line with evil.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "telephone-line-utils" "telephone-line-utils.el"
;;;;;;  (23198 63961 786430 885000))
;;; Generated autoloads from telephone-line-utils.el

(autoload 'telephone-line-defsegment "telephone-line-utils" "\
Create function NAME by wrapping BODY with telephone-line padding and propertization.

\(fn NAME BODY)" nil t)

(function-put 'telephone-line-defsegment 'lisp-indent-function 'defun)

(autoload 'telephone-line-defsegment* "telephone-line-utils" "\
Create function NAME by wrapping BODY with telephone-line padding and propertization.
Segment is not precompiled.

\(fn NAME BODY)" nil t)

(function-put 'telephone-line-defsegment* 'lisp-indent-function 'defun)

(autoload 'telephone-line-defsegment-plist "telephone-line-utils" "\


\(fn NAME PLISTS)" nil t)

(function-put 'telephone-line-defsegment-plist 'lisp-indent-function 'defun)

(autoload 'telephone-line-raw "telephone-line-utils" "\
Conditionally render STR as mode-line data, or just verify output if not COMPILED.
Return nil for blank/empty strings.

\(fn STR &optional COMPILED)" nil nil)

;;;***

;;;### (autoloads nil nil ("telephone-line-pkg.el" "telephone-line-segments.el"
;;;;;;  "telephone-line-separators.el") (23198 63961 922429 900000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; telephone-line-autoloads.el ends here
