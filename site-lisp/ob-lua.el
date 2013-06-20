;;; ob-lua.el --- org-babel functions for lua evaluation

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Brian Casiello
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.01h

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating Lua source code.
;; Sessions are not (yet) supported

;;; Requirements:

;; A working lua installation. See http://www.lua.org.
;; Customize org-babel-lua-command if emacs cannot find your lua
;; interpreter.

;;; Code:
(require 'ob)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(add-to-list 'org-babel-tangle-lang-exts '("lua" . "lua"))

(defvar org-babel-default-header-args:lua '())

(defvar org-babel-lua-command "lua"
  "Name of command to use for executing lua code.")

; Wrap a lua function around value results
; pp() reformats a lua value as its emacs-lisp equvalent, calling pt
; to handle table contents.
; pt() handles table contents recursively, using pe for individual
; elements.
; pe() handles a single table element, calling pt for nested tables
(defvar org-babel-lua-wrapper-method
  "io.output('%s')
   function pe(x) return type(x)=='table' and '('..pt(x,1)..')' or
      type(x)=='string' and '\"'..x..'\"' or x end
   function pt(x,i)return x[i] and (i>1 and ' ' or '')..pe(x[i])..pt(x,i+1) or '' end
   function pp(x) return type(x)=='table' and \"'(\"..pt(x,1)..')' or x end
   io.write(pp((function()%s end)()))"
  "Function wrapper for value results.")

(defun org-babel-expand-body:lua (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%s"
                (car pair)
                (org-babel-lua-var-to-lua (cdr pair))))
      vars "\n") "\n" body "\n")))

(defun org-babel-execute:lua (body params)
  "Execute a block of Lua code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Lua source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (first processed-params))
         (vars (second processed-params))
         (result-params (third processed-params))
         (result-type (fourth processed-params))
         (full-body (org-babel-expand-body:lua
                     body params processed-params))
	(session (org-babel-lua-initiate-session session)))
    (org-babel-reassemble-table
     (org-babel-lua-evaluate session full-body result-type)
     (org-babel-pick-name
      (nth 4 processed-params) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (nth 5 processed-params) (cdr (assoc :rownames params))))))

(defun org-babel-prep-session:lua (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-lua-var-to-lua (var)
  "Convert an elisp var into a string of lua source code
specifying a var of the same value."
  (if (listp var)
      (concat "{" (mapconcat #'org-babel-lua-var-to-lua var ", ") "}")
    (if (equal var 'hline)
		"None"
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       var))))

(defun org-babel-lua-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  ((lambda (res)
     (if (listp res)
		 (mapcar (lambda (el) (if (equal el 'None) 'hline el)) res)
       res))
   (org-babel-read
	(if (or (string-match "^{.+}$" results)
			(string-match "^(.+)$" results))
		(org-babel-read
		 (concat "'"
				 (replace-regexp-in-string
				  "{" "(" (replace-regexp-in-string
						   "}" ")" (replace-regexp-in-string
									"\, " " " (replace-regexp-in-string
											   "'" "\"" results t))))))
	  results))))

(defun org-babel-lua-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session.

Sessions are not yet supported for lua."
  (unless (string= session "none")
    ))

(defun org-babel-lua-evaluate (session body &optional result-type)
  "Pass BODY to the Lua process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp.  In
this case, the code body is wrapped in a function, so you will
probably want to use 'return <some value>' to return a result.
Sessions are not supported."
  (when session (error "Sessions are not supported for Lua."))
  (case result-type
	(output
	 (org-babel-eval org-babel-lua-command body))
    (value
	 (let ((tmp-file (make-temp-file "org-babel-lua-results-")))
	   (org-babel-eval
	      org-babel-lua-command
	      (format org-babel-lua-wrapper-method tmp-file body))
	   ((lambda (raw)
		  (if (or (member "code" result-params)
				  (member "pp" result-params))
			  raw
			(org-babel-lua-table-or-string raw)))
		(org-babel-eval-read-file tmp-file))))))

(provide 'ob-lua)
;;; ob-lua.el ends here
