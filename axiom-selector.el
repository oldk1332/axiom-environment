;;; axiom-selector.el -- a buffer selector utility for the Axiom environment

;; Copyright (C) 2013 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A utility for quickly selecting a buffer from the Axiom environment.

;; Inspired by (and code borrowed from) the Slime selector function.

;;; Code:

(defcustom axiom-selector-help-buffer-name "*Axiom Selector Help*"
  "Axiom selector help buffer name."
  :type 'string
  :group 'axiom)

(defvar axiom-selector-functions nil
  "List of functions for the `axiom-selector' function.

Each element is a list (KEY DESCRIPTION FUNCTION), where
DESCRIPTION is a one-line description of the command.")

(defun axiom-selector ()
  "Invoke a selector function by entering a single character.

The user is prompted for a single character indicating the
desired function. The `?' character describes the available
functions.  See `define-axiom-selector-function' for defining new
functions."
  (interactive)
  (message "Select [%s]: "
           (apply #'string (mapcar #'car axiom-selector-functions)))
  (let* ((ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (selector-entry (assq ch axiom-selector-functions)))
    (cond ((null selector-entry)
           (message "No function for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (axiom-selector))
          (t
           (funcall (third selector-entry))))))

(defmacro define-axiom-selector-function (key description &rest body)
  "Define a new `axiom-selector' function.

KEY is the key the user will enter to choose this function.
DESCRIPTION is a one-line sentence describing the function.
BODY is a series of forms which are evaluated when the command
is chosen."
  (let ((function `(lambda ()
                     (progn ,@body))))
    `(progn
       (assq-delete-all ,key axiom-selector-functions)
       (setq axiom-selector-functions
             (cons (list ,key ,description ,function)
                   axiom-selector-functions)))))

(define-axiom-selector-function ?? "Axiom selector help"
  (ignore-errors (kill-buffer axiom-selector-help-buffer-name))
  (with-current-buffer (get-buffer-create axiom-selector-help-buffer-name)
    (insert "Selector Methods:\n\n")
    (loop for (key line function) in axiom-selector-functions
          do (insert (format "%c:\t%s\n" key line)))
    (help-mode)
    (display-buffer (current-buffer) t)
    (shrink-window-if-larger-than-buffer 
     (get-buffer-window (current-buffer))))
  (axiom-selector)
  (current-buffer))

(define-axiom-selector-function ?q "Quit selector"
  (let ((help-buffer (get-buffer axiom-selector-help-buffer-name)))
    (when help-buffer
      (delete-window (get-buffer-window help-buffer))
      (kill-buffer help-buffer))))

(provide 'axiom-selector)