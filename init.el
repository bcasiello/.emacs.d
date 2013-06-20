;; allow my startup files to contain UTF-8 characters
(prefer-coding-system 'utf-8)

;; load machine-specific settings
(load (concat "~/.emacs.d/" (downcase system-name) ".el") t)

;; load OS-specific settings
(load (concat "~/.emacs.d/" (downcase (symbol-name system-type)) ".el") t)

;; initialize packages
(add-to-list 'load-path (car (file-expand-wildcards "~/.emacs.d/elpa/pallet*")))
(require 'pallet)

;; Tangle and load the literate startup file
(org-babel-load-file "~/.emacs.d/startup.org")
