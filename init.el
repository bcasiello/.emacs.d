;; initialize packages
(add-to-list 'load-path (car (file-expand-wildcards "~/.emacs.d/elpa/pallet*")))
(require 'pallet)

;; Tangle and load the literate startup file
(org-babel-load-file "~/.emacs.d/startup.org")
