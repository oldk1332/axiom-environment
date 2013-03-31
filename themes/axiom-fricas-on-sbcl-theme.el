;;; axiom-fricas-on-sbcl-theme.el -- a FriCAS/SBCL flavour for the Axiom environment

;; Copyright (C) 2013 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: Axiom, OpenAxiom, FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Setup Axiom environment customizations so they're suitable for FriCAS.

;;; Code:

(deftheme axiom-fricas
  "Setup the Axiom environment to be suitable for FriCAS.")

(custom-theme-set-variables 'axiom-fricas
  '(axiom-process-program "fricas -nosman"))

(custom-theme-set-variables 'axiom-fricas
  '(axiom-process-prompt-regexp "^.*([[:digit:]]+) ->"))

(custom-theme-set-variables 'axiom-fricas
  '(axiom-process-break-prompt-regexp "^0]"))

(custom-theme-set-variables 'axiom-fricas
  '(axiom-process-preamble ""))

(custom-theme-set-variables 'axiom-fricas
  '(axiom-standard-package-names-file "fricas-standard-package-names.el"))

(custom-theme-set-variables 'axiom-fricas
  '(axiom-standard-domain-names-file "fricas-standard-domain-names.el"))

(custom-theme-set-variables 'axiom-fricas
  '(axiom-standard-category-names-file "fricas-standard-category-names.el"))

(custom-theme-set-variables 'axiom-fricas
  '(axiom-standard-operation-names-file "fricas-standard-operation-names.el"))

(provide-theme 'axiom-fricas)
