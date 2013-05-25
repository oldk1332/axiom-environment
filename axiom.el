;;; axiom.el -- an environment for interacting with the Axiom CAS

;; Copyright (C) 2013 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; An environment for working with the Axiom/OpenAxiom/FriCAS computer
;; algebra system.
;;
;; This is an umbrella load-file that pulls in the entire environment.
;;
;; Use it by putting either
;;
;;   (load-file "/FULL/PATH/NAME/OF/THIS/FILE")
;;
;; or, if this directory is already in your load-path, then simply
;;
;;   (require 'axiom)
;;
;; into your .emacs file.
;;
;; If you want to be more selective about what gets loaded and exactly
;; how any global variables (e.g. auto-mode-alist) get modified, then
;; instead copy-and-paste any wanted bits from this file into your .emacs
;; file -- after ensuring this source directory is in your load-path with
;;
;;   (add-to-list 'load-path "/FULL/PATH/NAME/OF/THIS/DIR")

;;; Code:

;; To ensure this directory is in the load-path
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

;; Put the ``themes'' subdirectory in the custom theme load-path
(when load-file-name
  (add-to-list 'custom-theme-load-path (concat (file-name-directory load-file-name) "themes/")))

;; Load everything
(require 'axiom-help-mode)
(require 'axiom-process-mode)
(require 'axiom-input-mode)
(require 'axiom-spad-mode)
(require 'axiom-selector)

;; Setup auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.input" . axiom-input-mode))
(add-to-list 'auto-mode-alist '("\\.spad" . axiom-spad-mode))

;; Uncomment this to enable axiom-selector on C-c a
;(global-set-key (kbd "C-c a") 'axiom-selector)

(provide 'axiom)
