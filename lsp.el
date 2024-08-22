;;; lsp.el --- Configuration for language servers -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package lsp-mode
  :commands lsp-install-server
  :init
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-completion-provider :none)
  (add-hook 'lsp-mode-hook #'lsp-completion-mode))

(use-package consult-lsp
  :defer t
  ;; TODO
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . #'consult-lsp-symbols))
  )
