;; Load up Org Mode and Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(if (string= system-name "weatherwax")
    (setq dotfiles-dir "/home/brian/.emacs.d"))
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir);
	(message "added %s" dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path (expand-file-name "site-lisp" dotfiles-dir))
(add-subdirs-to-load-path (expand-file-name "elpa" dotfiles-dir))

;; tangle and load the startup file
(require 'org-install)
(org-babel-do-load-languages 'org-babel-load-languages '((lua . t)))
(org-babel-load-file (expand-file-name "startup.org" dotfiles-dir))
