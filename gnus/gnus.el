(setq auth-sources             (list (concat "~/.emacs.d/gnus/.authinfo." system-name ".gpg"))
      gnus-select-method       '(nntp "news.eternal-september.org")
      gnus-startup-file        "~/.emacs.d/gnus/.newsrc")

(setq-default send-mail-function    'smtpmail-send-it
              smtpmail-smtp-server  "smtp.gmail.com"
              smtpmail-smtp-service 587)

(add-to-list 'gnus-secondary-select-methods '(nntp "news.grc.com"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))
(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"  (nnimap-address "imap.gmail.com") (nnimap-server-port 993) (nnimap-stream ssl)))

(set-face-attribute 'gnus-summary-selected nil :background "lightblue")
(set-face-attribute 'gnus-summary-selected-face nil :background "lightblue")

(gnus-add-configuration '(group   (vertical 1.0 (group 1.0 point))))
(gnus-add-configuration '(summary (vertical 1.0 (group 5)       (summary 1.0 point))))
(gnus-add-configuration '(article (vertical 1.0 (group 5)       (summary 11  point) (article 1.0))))

(add-hook 'message-mode-hook (lambda () (flyspell-mode 1)))

(require 'bbdb)
(setq-default bbdb-file "~/.emacs.d/bbdb.gpg")
(bbdb-initialize 'gnus 'message)

(defun my-sign-encrypt-message ()
  (let ((response (read-char-choice "Sign, Encrypt, or leave As-is? " (append "sSeEaA" nil))))
    (cond
     ((or (equal response ?s) (equal response ?S)) (mml-secure-message-sign "pgp"))
     ((or (equal response ?e) (equal response ?E)) (mml-secure-message-sign-encrypt "pgp"))
     (t nil))))
(add-hook 'message-send-hook 'my-sign-encrypt-message)

;; See http://www.emacswiki.org/emacs-ru/TomRauchenwald
(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)
(setq gnus-summary-make-false-root         'dummy
      gnus-summary-make-false-root-always  nil
      gnus-group-line-format               "%P%M%S%5y : %G\n"
      gnus-summary-dummy-line-format       "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
      gnus-summary-line-format             "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %S\n"
      gnus-sum-thread-tree-indent          " "
      gnus-sum-thread-tree-root            "■ "
      gnus-sum-thread-tree-false-root      "□ "
      gnus-sum-thread-tree-single-indent   "▣ "
      gnus-sum-thread-tree-leaf-with-other "├─▶ "
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-single-leaf     "└─▶ ")

(add-hook 'gnus-article-prepare-hook
          (lambda ()
            (save-excursion
              (set-buffer gnus-article-buffer)
              (variable-pitch-mode 1))))
;;              (set (make-local-variable 'face-remapping-alist) '((default variable-pitch))))))
