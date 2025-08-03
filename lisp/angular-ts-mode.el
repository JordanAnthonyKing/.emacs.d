;;; angular-ts-mode.el --- tree-sitter support for HTML  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : January 2023
;; Keywords   : angular languages tree-sitter

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(require 'sgml-mode)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defcustom angular-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `angular-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'angular)

(defvar angular-ts-mode--indent-rules
  `((angular
     ((parent-is "fragment") column-0 0)
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol angular-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol angular-ts-mode-indent-offset)
     ((parent-is "style_element") parent-bol angular-ts-mode-indent-offset)
     ((parent-is "start_tag") parent-bol angular-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol angular-ts-mode-indent-offset)

     ;; New rules
     ;; Indent for statement_block and switch_statement nodes
     ((node-is "statement_block") parent-bol angular-ts-mode-indent-offset)
     ((node-is "switch_statement") parent-bol angular-ts-mode-indent-offset)

     ;; Begin block indentation
     ((node-is "{") parent-bol angular-ts-mode-indent-offset)
     
     ;; Branch indentation for closing brace
     ((node-is "}") parent-bol (- angular-ts-mode-indent-offset))
     
     ;; End block indentation
     ((parent-is "statement_block") parent-bol 0)
     ((node-is "}") parent-bol 0)))
  "Tree-sitter indent rules.")

;; (defvar angular-syntax-table 
;;   (let ((table (make-syntax-table text-mode-syntax-table)))
;;     (modify-syntax-entry ?< "(>" table)
;;     (modify-syntax-entry ?> ")<" table)
;;     ;; (modify-syntax-entry ?< " " table)
;;     ;; (modify-syntax-entry ?> " " table)
;;     (modify-syntax-entry ?: "_" table)
;;     (modify-syntax-entry ?_ "_" table)
;;     (modify-syntax-entry ?. " " table)
;;     table))

(define-derived-mode angular-ts-mode html-mode "Angular"
  "Major mode for editing Angular flavoured HTML, powered by tree-sitter."
  :group 'angular

  (unless (treesit-ready-p 'angular)
    (error "Tree-sitter for Angular isn't available"))

  (setq treesit-primary-parser (treesit-parser-create 'angular))

  ;; :syntax-table angular-syntax-table
  (setq-local treesit-simple-indent-rules angular-ts-mode--indent-rules)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp "element")

  (setq-local treesit-defun-name-function #'html-ts-mode--defun-name)

  (setq-local treesit-thing-settings
              `((html
                 (sexp ,(regexp-opt '("element"
                                      "text"
                                      "attribute"
                                      "value")))
                 (sexp-list ,(regexp-opt '("element")) 'symbols)
                 (sentence "tag")
                 (text ,(regexp-opt '("comment" "text"))))))


  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '(("Element" "\\`tag_name\\'" nil nil)))

  ;; Outline minor mode.
  (setq-local treesit-outline-predicate "\\`element\\'")
  ;; `html-ts-mode' inherits from `html-mode' that sets
  ;; regexp-based outline variables.  So need to restore
  ;; the default values of outline variables to be able
  ;; to use `treesit-outline-predicate' above.
  (kill-local-variable 'outline-regexp)
  (kill-local-variable 'outline-heading-end-regexp)
  (kill-local-variable 'outline-level)

  (setq-local tab-width angular-ts-mode-indent-offset)

  ;; (when (treesit-ready-p 'angular)
  ;;   (add-to-list 'auto-mode-alist '("\\.component.html\\'" . angular-ts-mode))
  ;;   (add-to-list 'auto-mode-alist '("\\.container.html\\'" . angular-ts-mode)))

  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component.html" "\\1.component.ts"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container.html" "\\1.container.ts"))

  (treesit-major-mode-setup))

(if (treesit-ready-p 'angular)
    (add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . angular-ts-mode)))

(provide 'angular-ts-mode)

;;; angular-ts-mode.el ends here
