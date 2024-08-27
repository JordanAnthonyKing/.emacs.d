;;; lsp.el --- Configuration for language servers -*- no-byte-compile: t; lexical-binding: t; -*-

;; Add the lisp directory to the load path
(add-to-list 'load-path (expand-file-name "lisp/" minimal-emacs-user-directory))
;; Load the angular-html-ts-mode
(require 'angular-ts-mode)

;; Add typescript-ts-mode for TypeScript files, including Angular component files
;; (add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . ng-html-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(use-package lsp-mode
  :commands (lsp-install-server lsp)
  :hook (typescript-ts-mode . lsp-deferred)
  :hook (angular-ts-mode . lsp-deferred)
  :init
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-completion-provider :none)
  (setq lsp-semantic-tokens-apply-modifiers nil
        lsp-semantic-tokens-enable t
        lsp-semantic-tokens-warn-on-missing-face t)
  (add-hook 'lsp-mode-hook #'lsp-completion-mode)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :config
  (setq lsp-typescript-indent-level 2
        lsp-auto-execute-action nil))

(use-package consult-lsp
  :defer t
  ;; TODO
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . #'consult-lsp-symbols)))

(defun ts-mode-add-sibling-rule ()
  "Add sibling rule for TypeScript files to match with Angular HTML files."
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.ts\\'" "\\1.component.html")))

(add-hook 'typescript-mode-hook 'ts-mode-add-sibling-rule)



(defun angular-ts-mode-add-sibling-rule ()
  "Add sibling rule for Angular HTML files to match with TypeScript files."
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.html\\'" "\\1.component.ts")))

(add-hook 'angular-ts-mode-hook 'angular-ts-mode-add-sibling-rule)

(use-package indent-bars
  :ensure (indent-bars :host github :repo "jdtsmith/indent-bars")
  :hook ((typescript-mode angular-ts-mode) . indent-bars-mode)
  :config
    ;; Define a function to set indent-bars-spacing
    (defun set-indent-bars-spacing ()
      "Set `indent-bars-spacing` to 2 for the current mode."
      (setq-local indent-bars-spacing 2))
  
    ;; Add the function to the indent-bars-mode hook
    (add-hook 'indent-bars-mode-hook 'set-indent-bars-spacing)) 
