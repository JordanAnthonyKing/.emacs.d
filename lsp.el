;;; lsp.el --- Configuration for language servers -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp/" minimal-emacs-user-directory))
(require 'angular-ts-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(defun ts-mode-add-sibling-rule ()
  "Add sibling rule for TypeScript files to match with Angular HTML files."
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.ts\\'" "\\1.component.html")))

(add-hook 'typescript-ts-mode-hook 'ts-mode-add-sibling-rule)
(add-hook 'typescript-ts-mode-hook (lambda () (setq tab-width 2)))

(defun angular-ts-mode-add-sibling-rule ()
  "Add sibling rule for Angular HTML files to match with TypeScript files."
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.html\\'" "\\1.component.ts"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container\\.html\\'" "\\1.component.ts")))

(add-hook 'angular-ts-mode-hook 'angular-ts-mode-add-sibling-rule)
(add-hook 'angular-ts-mode-hook (lambda () (setq tab-width 2)))

;; TODO: Add this to relevant hooks
(setq tab-width 2)

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
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (angular-ts-mode . eglot-ensure)
  :defer t
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

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :hook (sgml-mode . flymake-mode))

;; (use-package flymake-popon
  ;; :hook (flymake-mode . flymake-popon-mode)
  ;; :config
  ;; (setq flymake-popon-method 'posframe))

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-prefer-search 'rg
        dumb-jump-aggressive nil
        dumb-jump-selector 'completing-read)
  (setq dumb-jump-language-file-exts
        (append dumb-jump-language-file-exts
                '((:language "typescript" :ext "html" :agtype "html" :rgtype "html"))))

  (setq dumb-jump-language-file-exts
        (append dumb-jump-language-file-exts
                '((:language "angular" :ext "html" :agtype "html" :rgtype "html")
                  (:language "angular" :ext "ts" :agtype "ts" :rgtype "ts")))))

;; (use-package eslintd-fix
  ;; :ensure t
  ;; :hook (typescript-ts-mode . eslintd-fix-mode)
  ;; ;; :config
  ;; ;; (setq eslintd-fix-executable "c:/Program Files/nodejs/eslint_d")
  ;; )
  
(use-package consult-eglot
  :ensure t
  :defer t
  :after eglot
  :general
  ([remap xref-find-apropos] #'consult-eglot-symbols))

;;;###autoload
(defun my-corfu-move-to-minibuffer ()
  "Move the current list of candidates to the minibuffer using `vertico` and `consult`."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (consult-completion-in-region beg end table pred)))))

;;;###autoload
(defun my-corfu-smart-sep-toggle-escape ()
  "Insert `corfu-separator` or toggle escape if it's already there."
  (interactive)
  (cond ((and (char-equal (char-before) corfu-separator)
              (char-equal (char-before (1- (point))) ?\\))
         (save-excursion (delete-char -2)))
        ((char-equal (char-before) corfu-separator)
         (save-excursion
           (backward-char 1)
           (insert-char ?\\)))
        (t (call-interactively #'corfu-insert-separator))))

;;;###autoload
(defun +corfu/dabbrev-this-buffer ()
  "Like `cape-dabbrev', but only scans current buffer."
  (interactive)
  (require 'cape)
  (let ((cape-dabbrev-check-other-buffers nil))
    (cape-dabbrev t)))

;;; Corfu
;; TODO: Bindings
(use-package corfu
  :ensure t
  :defer t
  :general
  (:keymaps 'corfu-mode-map
            "C-SPC" #'completion-at-point)
  (:keymaps 'corfu-map
            "C-SPC"    #'corfu-insert-separator
            "C-k"      #'corfu-previous
            "C-j"      #'corfu-next
            "TAB"      #'corfu-next
            "RET"      nil
            [remap meow-insert-exit] #'corfu-quit)
  (:keymaps 'corfu-popupinfo-map
            "C-h"      #'corfu-popupinfo-toggle
            "C-S-k"    #'corfu-popupinfo-scroll-down
            "C-S-j"    #'corfu-popupinfo-scroll-up)
  (:states 'insert
:prefix "C-x"
           "C-l"  #'cape-line
           "C-k"  #'cape-keyword
           "C-f"  #'cape-file
           "C-]"  #'complete-tag
           "s"    #'cape-dict
           ;; "C-s"  #'yasnippet-capf
           "C-o"  #'completion-at-point
           "C-n"  #'cape-dabbrev
           "C-p"  #'+corfu/dabbrev-this-buffer)
  :hook ((prog-mode . corfu-mode)
         (sgml-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.24
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)

  (add-to-list 'corfu-continue-commands #'+corfu/move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'+corfu/smart-sep-toggle-escape)
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)
  
  ;; Enable corfu-history to maintain completion history

  ;; Enable popup info for detailed information on completion
  ;; If you reenable this use a hook
  ;; (corfu-popupinfo-mode +1)
  ;; (setq corfu-popupinfo-delay '(0.5 . 1.0))

  ;; HACK: If you want to update the visual hints after completing minibuffer
  ;;   commands with Corfu and exiting, you have to do it manually.
  (defun my-corfu--insert-before-exit-minibuffer ()
    "Manually update visual hints before exiting the minibuffer when Corfu is active."
    (when (or (and (frame-live-p corfu--frame)
                   (frame-visible-p corfu--frame))
              (and (featurep 'corfu-terminal)
                   (popon-live-p corfu-terminal--popon)))
      (when (member isearch-lazy-highlight-timer timer-idle-list)
        (apply (timer--function isearch-lazy-highlight-timer)
               (timer--args isearch-lazy-highlight-timer)))
      (when (member (bound-and-true-p anzu--update-timer) timer-idle-list)
        (apply (timer--function anzu--update-timer)
               (timer--args anzu--update-timer)))
      (when (member (bound-and-true-p evil--ex-search-update-timer)
                    timer-idle-list)
        (apply (timer--function evil--ex-search-update-timer)
               (timer--args evil--ex-search-update-timer)))))

  ;; Add the advice to run before `exit-minibuffer`
  (advice-add 'exit-minibuffer :before #'my-corfu--insert-before-exit-minibuffer))

(use-package cape
  :ensure t
  :defer t
  :init
  ;; Hook to add `cape-file` to `completion-at-point-functions` in `prog-mode`.
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-file -10 t)))

  ;; Hooks to add `cape-elisp-block` in `org-mode` and `markdown-mode`.
  (dolist (hook '(org-mode-hook markdown-mode-hook))
    (add-hook hook
              (lambda ()
                (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))))

  ;; Enable Dabbrev completion as a fallback in various modes.
  (setq cape-dabbrev-check-other-buffers t)

  (defun my-dabbrev-friend-buffer-p (other-buffer)
    "Custom function to determine if OTHER-BUFFER is a dabbrev friend."
    (< (buffer-size other-buffer) 1000000)) ;; Replace with your buffer size limit.

  (dolist (hook '(prog-mode-hook
                  sgml-mode-hook
                  ;;text-mode-hook
                  conf-mode-hook
                  comint-mode-hook
                  minibuffer-setup-hook
                  eshell-mode-hook))
    (add-hook hook
              (lambda ()
                (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))))
  (with-eval-after-load 'dabbrev
    (setq dabbrev-friend-buffer-function #'my-dabbrev-friend-buffer-p
          dabbrev-ignored-buffer-regexps
          '("\\` "
            "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")
          dabbrev-upcase-means-case-search t)
    (dolist (mode '(pdf-view-mode doc-view-mode tags-table-mode))
      (add-to-list 'dabbrev-ignored-buffer-modes mode)))

  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; Fix Eshell autocompletion on Emacs 28.
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

;; TODO: What doe thi even do?
;; (use-package corfu-history
  ;; :ensure nil
  ;; :hook (corfu-mode . corfu-history-mode)
  ;; :config
  ;; (after! savehist (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-popupinfo
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))

;; TODO: Test this
;; (use-package kind-icon
  ;; :ensure t
  ;; :after corfu
  ;; :config
  ;; (setq kind-icon-default-face 'corfu-default)  ;; Use corfu's default face for icons
  ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; (setq kind-icon-use-icons nil))  ;; Disable icons and use text instead

(use-package ws-butler
  :ensure (ws-butler :host github :repo "lewang/ws-butler"))

;; (use-package indent-bars
  ;; :ensure (indent-bars :host github :repo "jdtsmith/indent-bars")
  ;; ;; :hook ((typescript-ts-mode angular-ts-mode) . indent-bars-ts-mode)
  ;; :custom
  ;; (indent-bars-treesit-support t)
  ;; ;; (indent-bars-treesit-ignore-blank-lines-types nil)
  ;; (indent-bars-treesit-scope '((typescript class_body)))
                                ;; ;; program comment ternary_expression member_expression named_imports 
                                ;; ;; statement_block switch_case switch_default type_arguments type_parameters 
                                ;; ;; arguments array formal_parameters template_string template_substitution 
                                ;; ;; object_pattern object object_type enum_body class_body interface_body 
                                ;; ;; arrow_function parenthesized_expression binary_expression if_statement 
                                ;; ;; for_statement for_in_statement while_statement do_statement else_clause)))
;; 
  ;; :config
  ;; ;; (setq indent-bars-starting-column 0)
  ;; ;; (defun set-indent-bars-spacing () (setq-local indent-bars-spacing 2))
  ;; ;; (add-hook 'indent-bars-mode-hook 'set-indent-bars-spacing)
;; 
  ;; ;; (setq indent-bars-color '(:face background :blend 1.0))
  ;; ;; (setq indent-bars-highlight-current-depth '(:face nano-popout))
;; 
  ;; ;; (setq indent-bars-ts-highlight-current-depth '(:face nano-popout))
  ;; ) 

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape)
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package highlight-parentheses
  :ensure (highlight-parentheses :host "github.com" :repo "emacsmirror/highlight-parentheses")
  :hook (prog-mode . highlight-parentheses-mode)
  :defer t
  :config
  (setq highlight-parentheses-colors '("#ff9d1e")))

