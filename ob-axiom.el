;;; ob-axiom.el --- org-babel for the axiom-environment system

;; Copyright (C) Paul Onions

;; Author: Paul Onions
;; Version: 0.1

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; This file enables org-mode integration of the Axiom, OpenAxiom &
;; FriCAS computer algebra systems, by way of the axiom-environment
;; system.

;; It is currently extremely minimal and needs lots more work to
;; implement proper org-mode functionality.

;; This file is based on the ob-template.el example file.

;;; Requirements:

;; Requires axiom-environment to be installed.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(require 'axiom-environment)

;; File extension for Axiom Input files
(add-to-list 'org-babel-tangle-lang-exts '("axiom" . "input"))

;; Configure org editing options
(add-to-list 'org-src-lang-modes '("axiom" . axiom-input))

;; Default header arguments
(defvar org-babel-default-header-args:axiom '())

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:axiom' function below.
(defun org-babel-expand-body:axiom (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (split-string body "\n"))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:axiom (body params)
  "Execute a block of Axiom code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-axiom-initiate-session
                   (cdr (assoc :session processed-params))))
         (lines (org-babel-expand-body:axiom
                 body params processed-params)))
    (with-axiom-process-query-buffer
     (dolist (line lines)
       (axiom-process-redirect-send-command line (current-buffer) nil t t))
     (buffer-substring (point-min) (point-max)))))

(defun org-babel-axiom-initiate-session (&optional session)
  "Initialize Axiom session, return the session buffer."
  (unless (equal session "none")
    (when (null (get-buffer axiom-process-buffer-name))
      (axiom-process-start axiom-process-program))
    (get-buffer axiom-process-buffer-name)))

;; ;; This function should be used to assign any variables in params in
;; ;; the context of the session environment.
;; (defun org-babel-prep-session:axiom (session params)
;;   "Prepare SESSION according to the header arguments specified in PARAMS."
;;   )

;; (defun org-babel-axiom-var-to-axiom (var)
;;   "Convert an elisp var into a string of axiom source code
;; specifying a var of the same value."
;;   (format "%S" var))

;; (defun org-babel-axiom-table-or-string (results)
;;   "If the results look like a table, then convert them into an
;; Emacs-lisp table, otherwise return the results as a string."
;;   results)

(provide 'ob-axiom)
