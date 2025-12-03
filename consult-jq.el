;;; consult-jq.el --- Lieve preview of "jq" queries using consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  bigbuger

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;   Needs the "jq" binary installed.

;;; Code:

(require 'consult)

(defcustom consult-jq-json-buffer-mode 'js-mode
  "Major mode for the resulting `consult-jq-buffer' buffer."
  :type '(function)
  :require 'consult-jq
  :group 'consult-jq)

(defcustom consult-jq-command "jq"
  "Command for `consult-jq'."
  :type '(string)
  :require 'consult-jq
  :group 'consult-jq)

(defcustom consult-jq-path-query
  "[ path(..) | map(select(type == \"string\") // \"[]\" | if test(\"^[0-9a-zA-Z_\\\\[\\\\]]*$\") then . else \"\\\"\" + . + \"\\\"\" end) | join(\".\") ] | sort | unique | .[] | split(\".[]\") | join(\"[]\") | \".\" + ."
  "Use jq to get all json path."
  :type '(string)
  :require 'consult-jq
  :group 'consult-jq)

(defcustom consult-jq-buffer "*consult-jq-result*"
  "Buffer for the `consult-jq' query results."
  :type '(string)
  :require 'consult-jq
  :group 'consult-jq)

(defvar consult--jq-history nil)

(add-to-list 'display-buffer-alist
	     `(,consult-jq-buffer
	       display-buffer-in-direction
	       (direction . right)))

(defvar consult-jq-buildin-functions nil)
(defvar consult-jq-custom-functions nil)
(defconst consult-jq-custom-function-file
  (file-truename "~/.jq"))

(defun consult-jq-load-custom-functions ()
  "Load all jq custom function which define in .jq file."
  (when (file-readable-p consult-jq-custom-function-file)
    (with-temp-buffer
      (insert-file-contents consult-jq-custom-function-file)
      (goto-char (point-min))
      (save-match-data
	(goto-char 1)
	(while (search-forward-regexp "def *\\([a-zA-Z_]+\\)" nil t 1)
	  (push (match-string 1) consult-jq-custom-functions))))))

(defun consult-jq-load-builtins ()
  "Load all jq builtins into `consult-jq-buildin-functions'."
  (setq consult-jq-buildin-functions
	(mapcar (lambda (c) (car (split-string c "/")))
		(string-split (shell-command-to-string "echo '{}' | jq -r 'builtins | .[]'")))))

(defun consult-jq-get-all-jq-function ()
  "Return all jq function."
  (append consult-jq-buildin-functions consult-jq-custom-functions nil))

(defun consult-jq-identifier-p (ch)
  (when ch
    (or (and (<= ?A ch)
	     (>= ?z ch))
	(eq ?_ ch)
	(eq ?\[ ch)
	(eq ?\] ch)
	(eq ?. ch))))

(defun consult-jq-make-capf (buffer)
  "This is the function to be used for the hook `completion-at-point-functions'."
  (unless consult-jq-buildin-functions (consult-jq-load-builtins))
  (when (and (not consult-jq-custom-functions)
	     (file-readable-p consult-jq-custom-function-file))
    (consult-jq-load-custom-functions))
  (lambda ()
    (let* ((contents (minibuffer-contents-no-properties))
           (start (save-excursion
		    (while (let ((ch (char-before)))
			     (consult-jq-identifier-p ch))
		      (backward-char))
		    (point)))
	   (end (save-excursion
		  (while (let ((ch (char-after)))
			   (consult-jq-identifier-p ch))
		    (forward-char))
		  (point)))
	   (need-path? (eq ?. (char-after start)))
	   (pip-index (string-match-p (regexp-quote "|") contents))
	   (query (when pip-index
		    (replace-regexp-in-string (rx "|" (* (not "|")) line-end)
					      ""
					      contents))))
      (list start end (if need-path? (cl-remove-if #'string-blank-p
						   (consult-jq-path buffer query))
			(consult-jq-get-all-jq-function))
	    :category 'consult-jq))))

(defun consult-jq-call-jq (&optional query args)
  "Call `jq' With the QUERY and ARGS."
  (let* (status
	 (output
	  (with-output-to-string
	    (setq status (call-process-region nil nil
			  consult-jq-command
			  nil standard-output nil
			  (or args  "-M") (or query "."))))))
    (if (eq status 0)
	output
      (progn (message "%s" (propertize output 'face 'error))
	     nil))))
	  
(defun consult-jq-path (buffer &optional query)
  "Get all json path, after apply `QUERY' in `BUFFER'."
  (with-current-buffer buffer
    (let ((output (consult-jq-call-jq
		   (concat (or query "." ) " | " consult-jq-path-query)
		   "-r")))
      (when output
	(split-string output "\n")))))


(defun consult-jq-query (buffer query)
  "Call jq with `QUERY' and context of `BUFFER' as input.
Then output the result into `consult-jq-buffer'."
  (let ((result (with-current-buffer buffer
		  (consult-jq-call-jq query nil))))
    (when result
      (list (with-current-buffer consult-jq-buffer
	      (erase-buffer)
	      (insert result)
	      (buffer-string))))))

(defun consult-jq-state (buffer)
  "Build consult STATE of consult-jq."
  (lambda (_act cand)
    (consult-jq-query buffer cand)
    (display-buffer consult-jq-buffer)))

(defcustom consult-jq-completion-styles
  nil
  "The `completion-styles' for `consult-jq'.  When nil, use the EMACS default."
  :type completion--styles-type
  :group 'consult-jq
  )

;;;###autoload
(defun consult-jq ()
  "Consult interface for dynamically querying jq.
The results will be displayed to you in the buffer in `consult-jq-buffer'."
  (interactive)
  (let* ((buffer (current-buffer))
	 (completion-styles (or consult-jq-completion-styles completion-styles))
	 (completion-at-point-functions
	  (list (consult-jq-make-capf buffer))))
    (with-current-buffer  (get-buffer-create consult-jq-buffer)
      (funcall consult-jq-json-buffer-mode))
    (consult--read
     (consult-jq-path buffer)
     :prompt "jq: "
     :category 'consult-jq
     :initial "."
     :sort nil
     :state (consult-jq-state buffer)
     :history '(:input consult--jq-history))))

;;;###autoload
(defun consult-jq-without-candidate ()
  "Consult interface for dynamically querying jq.
The results will be displayed to you in the buffer in `consult-jq-buffer'."
  (interactive)
  (let* ((buffer (current-buffer))
	 (completion-styles (or consult-jq-completion-styles completion-styles))
	 (completion-at-point-functions
	  (list (consult-jq-make-capf buffer))))
    (with-current-buffer  (get-buffer-create consult-jq-buffer)
      (funcall consult-jq-json-buffer-mode))
    (consult--prompt
     :prompt "jq: "
     :initial "."
     :state (consult-jq-state buffer)
     :history 'consult--jq-history)))


(provide 'consult-jq)

;;; consult-jq.el ends here
