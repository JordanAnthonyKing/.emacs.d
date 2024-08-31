;;; lsp.el --- Configuration for language servers -*- no-byte-compile: t; lexical-binding: t; -*-

;; Add the lisp directory to the load path
(add-to-list 'load-path (expand-file-name "lisp/" minimal-emacs-user-directory))
;; Load the angular-html-ts-mode
(require 'angular-ts-mode)

;; Add typescript-ts-mode for TypeScript files, including Angular component files
;; (add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . ng-html-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)
            +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
      ;;        library, so we up the GC threshold to stave off GC-induced
      ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
      ;;        so we modify its variables rather than `gc-cons-threshold'
      ;;        directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))


(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (angular-ts-mode . eglot-ensure)
  :general
  (:states '(normal visual)
           :keymaps 'eglot-mode-map
           "gd"    #'+lookup/definition
           "gD"    #'+lookup/references)
  :init
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-auto-display-help-buffer nil)

  :config
  ;; (set-popup-rule! "^\\*eglot-help" :size 0.15 :quit t :select t)
  ;; (set-lookup-handlers! 'eglot--managed-mode
  ;; :definition      #'xref-find-definitions
  ;; :references      #'xref-find-references
  ;; :implementations #'eglot-find-implementation
  ;; :type-definition #'eglot-find-typeDefinition
  ;; :documentation   #'+eglot-lookup-documentation)

  (add-to-list 'eglot-server-programs
               '(angular-ts-mode "ngserver"
                                 "--stdio"
                                 "--tsProbeLocations"
                                 "c:/Program Files/nodejs/node_modules"
                                 "--ngProbeLocations"
                                 "c:/Program Files/nodejs/node_modules/@angular/language-server/node_modules/"))

  (cl-callf plist-put eglot-events-buffer-config :size 0))

(use-package consult-eglot
  :ensure t
  :defer t
  :after eglot
  :general
  ([remap xref-find-apropos] #'consult-eglot-symbols))


;; (use-package lsp-mode
;; :commands (lsp-install-server lsp)
;; :hook (typescript-ts-mode . lsp-deferred)
;; :hook (angular-ts-mode . lsp-deferred)
;; :init
;; (setq lsp-enable-folding nil
;; lsp-enable-text-document-color nil)
;; (setq lsp-enable-on-type-formatting nil)
;; 
;; (setq lsp-headerline-breadcrumb-segments '(symbols))
;; (setq lsp-headerline-breadcrumb-enable nil)
;; (setq lsp-completion-provider :none)
;; (setq lsp-enable-snippet nil)
;; ;; (setq lsp-semantic-tokens-apply-modifiers nil)
;; (setq lsp-semantic-tokens-apply-modifiers nil
;; lsp-semantic-tokens-enable nil
;; lsp-semantic-tokens-warn-on-missing-face nil)
;; (setq lsp-lens-enable nil
;; lsp-completion-show-detail nil
;; lsp-signature-render-documentation nil)
;; ;; HACK: https://www.reddit.com/r/emacs/comments/wdqjxu/lspeslintservercommand_not_working_anymore/
;; ;; (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
;; ;; (setq lsp-eslint-working-directories "./*")
;; (setq lsp-enable-file-watchers t
;; lsp-file-watch-threshold 9001)
;; ;; (setq flycheck-checker-error-threshold 9001)
;; (add-hook 'lsp-mode-hook #'lsp-completion-mode)
;; (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;; :config
;; (setq lsp-typescript-indent-level 2
;; lsp-auto-execute-action nil))
;; 
;; (use-package consult-lsp
;; :defer t
;; ;; TODO
;; :bind (:map lsp-mode-map
;; ([remap xref-find-apropos] . #'consult-lsp-symbols)))

(defun ts-mode-add-sibling-rule ()
  "Add sibling rule for TypeScript files to match with Angular HTML files."
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.ts\\'" "\\1.component.html")))

(add-hook 'typescript-ts-mode-hook 'ts-mode-add-sibling-rule)

(defun angular-ts-mode-add-sibling-rule ()
  "Add sibling rule for Angular HTML files to match with TypeScript files."
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.html\\'" "\\1.component.ts"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container\\.html\\'" "\\1.component.ts")))

(add-hook 'angular-ts-mode-hook 'angular-ts-mode-add-sibling-rule)

;; (use-package prettier
;; :ensure t
;; :hook (typescript-ts-mode . prettier-mode)
;; :hook (angular-ts-mode . prettier-mode))

;; (use-package indent-bars
;; :ensure (indent-bars :host github :repo "jdtsmith/indent-bars")
;; :hook ((typescript-mode angular-ts-mode) . indent-bars-mode)
;; :config
;; ;; Define a function to set indent-bars-spacing
;; (defun set-indent-bars-spacing ()
;; "Set `indent-bars-spacing` to 2 for the current mode."
;; (setq-local indent-bars-spacing 2))
;;  
;; ;; Add the function to the indent-bars-mode hook
;; (add-hook 'indent-bars-mode-hook 'set-indent-bars-spacing)) 

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))
