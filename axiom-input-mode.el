;;; axiom-input-mode.el --- Major mode for the Axiom interactive language -*- lexical-binding: t -*-

;; Copyright (C) 2013 - 2016 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for the Axiom interactive language, e.g. .input files.

;;; Code:

(require 'axiom-base)
(require 'axiom-help-mode)
(require 'axiom-process-mode)

(defface axiom-input-doc-comment '((t (:foreground "dark magenta")))
  "Face used for displaying input documentation comments."
  :group 'axiom)

(defface axiom-input-keyword '((t (:foreground "grey25")))
  "Face used for displaying input file keywords."
  :group 'axiom)

(defvar axiom-input-mode-syntax-table
  (copy-syntax-table axiom-common-syntax-table)
  "The Axiom input mode syntax table.")

(defvar axiom-input-doc-comment-regexp
  "\\+\\+.*$"
  "An Axiom documentation comment.")

(defvar axiom-input-keyword-names
  (list "has"
        "if" "then" "else"
        "for" "in" "by" "while" "repeat" "return" "break"))

(defvar axiom-input-keywords-regexp
  (concat "\\<" (regexp-opt axiom-input-keyword-names) "\\>")
  "Regular expression for input file keywords.")

(defvar axiom-input-doc-comment-face 'axiom-input-doc-comment)
(defvar axiom-input-keyword-face     'axiom-input-keyword)
(defvar axiom-input-package-face     'axiom-package-name)
(defvar axiom-input-domain-face      'axiom-domain-name)
(defvar axiom-input-category-face    'axiom-category-name)

(defvar axiom-input-font-lock-keywords
  (list (cons axiom-input-doc-comment-regexp               'axiom-input-doc-comment-face)
        (cons axiom-input-keywords-regexp                  'axiom-input-keyword-face)
        (cons axiom-standard-package-names-regexp          'axiom-input-package-face)
        (cons axiom-standard-package-abbreviations-regexp  'axiom-input-package-face)
        (cons axiom-standard-domain-names-regexp           'axiom-input-domain-face)
        (cons axiom-standard-domain-abbreviations-regexp   'axiom-input-domain-face)
        (cons axiom-standard-category-names-regexp         'axiom-input-category-face)
        (cons axiom-standard-category-abbreviations-regexp 'axiom-input-category-face)))

(defvar axiom-input-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map axiom-common-keymap)
    (define-key map (kbd "C-<return>") 'axiom-input-send-line)
    map)
  "The Axiom input mode local keymap.")

(defvar axiom-input-mode-hook nil
  "Hook for customizing Axiom input mode.")

(defun axiom-input-read-buffer ()
  (interactive)
  (axiom-process-read-file buffer-file-name))

(defun axiom-input-send-line ()
  (interactive)
  (let ((str (save-excursion
               (beginning-of-line)
               (axiom-get-rest-of-line))))
    (axiom-process-eval-string str)
    (axiom-move-to-next-line)))

(defun axiom-input-complete-symbol ()
  (and (looking-back "[[:word:]]+" nil t)
       (list (match-beginning 0)
             (match-end 0)
             axiom-standard-names-and-abbreviations)))

(defun axiom-input-indent-line ()
  (if (eql (char-syntax (char-before)) ?w)
      (complete-symbol nil)
    (indent-relative)))

;;;###autoload
(define-derived-mode axiom-input-mode prog-mode "Axiom Input"
  "Major mode for the Axiom-Input interactive language."
  :group 'axiom
  (setq font-lock-defaults (list 'axiom-input-font-lock-keywords))
  (setq indent-line-function 'axiom-input-indent-line)
  (setq electric-indent-inhibit t)
  (setq completion-at-point-functions '(axiom-input-complete-symbol))
  (setq axiom-menu-compile-buffer-enable nil)
  (setq axiom-menu-compile-file-enable nil)
  (setq axiom-menu-read-buffer-enable t)
  (setq axiom-menu-read-file-enable t)
  (setq axiom-menu-read-region-enable t)
  (setq axiom-menu-eval-region-enable t))

(provide 'axiom-input-mode)

;;; axiom-input-mode.el ends here
