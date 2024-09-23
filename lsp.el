;;; lsp.el --- Configuration for language servers -*- no-byte-compile: t; lexical-binding: t; -*-

;; (require 'treesit)
;;
;; (define-derived-mode angular-ts-mode html-ts-mode "Angular"
;;   "Major mode for editing Angular flavoured HTML, powered by tree-sitter."
;;   :group 'angular
;;   (add-to-list 'find-sibling-rules
;;                '("\\(.+\\)\\.component\\.html\\'" "\\1.component.ts"))
;;   (add-to-list 'find-sibling-rules
;;                '("\\(.+\\)\\.container\\.html\\'" "\\1.container.ts")))
;;
;; (when (treesit-ready-p 'angular)
;;   (add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . angular-ts-mode))
;;   (add-to-list 'auto-mode-alist '("\\.container\\.html\\'" . angular-ts-mode)))
;;
;; (provide 'angular-ts-mode)

;; TODO: This not activating right
(use-package typescript-ts-mode
  :ensure nil
  :defer t
  :mode "\\.ts\\'"
  :config
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.ts\\'" "\\1.component.html"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container\\.ts\\'" "\\1.container.html"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container\\.spec\\.ts\\'" "\\1.container.ts")))

;; (use-package treesit-langs
;;   :ensure (treesit-langs
;;            :host github
;;            :repo "JordanAnthonyKing/treesit-langs"
;;            :files ("treesit-*.el" "queries"))
;;   :defer t
;;   :hook ((prog-mode angular-ts-mode) . (lambda ()
;;                                          ;; some mode is better be highlighted not using tree-sitter
;;                                          (unless (member major-mode '(sh-mode))
;;                                            (ignore-errors (treesit-hl-toggle 'on)))))
;;   :custom
;;   (treesit-langs-git-dir nil)
;;   (treesit-langs-grammar-dir (expand-file-name "tree-sitter" user-emacs-directory))
;;   :config
;;   (add-to-list 'treesit-extra-load-path treesit-langs-grammar-dir))

;; (use-package add-node-modules-path
;;   :ensure t
;;   :defer t
;;   :hook ((typescript-ts-mode . add-node-modules-path)
;;          (angular-ts-mode . add-node-modules-path)))
;;
;; (use-package eslintd-fix
;;   :ensure t
;;   :defer t
;;   :hook (typescript-ts-mode . eslintd-fix-mode))

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((typescript-ts-mode . lsp)
         (angular-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-on-type-formatting nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-keep-workspace-alive nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-snippet nil
        lsp-keymap-prefix nil
        lsp-file-watch-threshold 9001
        lsp-typescript-indent-level 2
        lsp-auto-execute-action nil
        lsp-signature-function 'lsp-signature-posframe
        lsp-completion-provider :none)
  :config
  (defun +lsp-signature-stop-maybe-h ()
    "Close the displayed `lsp-signature'."
    (when lsp-signature-mode
      (lsp-signature-stop)
      t))

  (add-hook 'doom-escape-hook #'+lsp-signature-stop-maybe-h))

(use-package consult-lsp
  :ensure t
  :defer t
  :general
  (:keymaps 'lsp-mode-map
            [remap xref-find-apropos] #'consult-lsp-symbols)
  :commands consult-lsp-symbols)

(use-package flycheck
  :ensure t
  :defer t
  :hook ((typescript-ts-mode . flycheck-mode)
         (angular-ts-mode . flycheck-mode))
  :config
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.25)

  (defun +syntax-check-buffer-h ()
    "Flycheck buffer on ESC in normal mode."
    (when flycheck-mode
      (ignore-errors (flycheck-buffer))
      nil))

  (add-hook 'doom-escape-hook #'+syntax-check-buffer-h))

(use-package consult-flycheck
  :ensure t
  :defer t)

(use-package flycheck-popup-tip
  :ensure t
  :defer t
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :config
  (setq flycheck-popup-tip-error-prefix "[!] ")

  ;; HACK: Only display the flycheck popup if we're in normal mode (for evil
  ;;   users) or if no selection or completion is active. This popup can
  ;;   interfere with the active evil mode, clear active regions, and other
  ;;   funny business (see #7242).
  (defun +syntax--disable-flycheck-popup-tip-maybe-a (&rest _)
    (if (and (bound-and-true-p evil-local-mode)
             (not (evil-emacs-state-p)))
        (evil-normal-state-p)
      (and (not (region-active-p))
           (not (ignore-errors (>= corfu--index 0))))))

  (advice-add #'flycheck-popup-tip-show-popup :before-while #'+syntax--disable-flycheck-popup-tip-maybe-a))

(use-package flycheck-posframe
  :ensure t
  :defer t
  :after evil
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "[?] "
        flycheck-posframe-info-prefix "[i] "
        flycheck-posframe-error-prefix "[!] ")

  ;; HACK: Hide the flycheck posframe immediately on the next keypress/user
  ;;   action, otherwise it lingers until the next time the user is idle.
  (defun +syntax--flycheck-posframe-hide-h ()
    (unless (flycheck-posframe-check-position)
      (posframe-hide flycheck-posframe-buffer))
    (remove-hook 'post-command-hook #'+syntax--flycheck-posframe-hide-h))

  (defun +syntax-hide-posframe-on-next-command-a (fn &rest args)
    (letf! ((defun posframe-show (&rest args)
              (add-hook 'post-command-hook #'+syntax--flycheck-posframe-hide-h)
              (apply posframe-show args)))
           (apply fn args)))

  (advice-add #'flycheck-posframe-show-posframe :around #'+syntax-hide-posframe-on-next-command-a)

  (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p))

;; (use-package dumb-jump
;; :commands dumb-jump-result-follow
;; :config
;; (setq dumb-jump-prefer-search 'rg
;; dumb-jump-aggressive nil
;; dumb-jump-selector 'completing-read)
;; (setq dumb-jump-language-file-exts
;; (append dumb-jump-language-file-exts
;; '((:language "typescript" :ext "html" :agtype "html" :rgtype "html"))))
;; 
;; (setq dumb-jump-language-file-exts
;; (append dumb-jump-language-file-exts
;; '((:language "angular" :ext "html" :agtype "html" :rgtype "html")
;; (:language "angular" :ext "ts" :agtype "ts" :rgtype "ts")))))

;; TODO: After conflicting with hook?
(use-package corfu
  :ensure t
  :defer t
  :hook (elpaca-after-init . global-corfu-mode)
  :general
  (:keymaps 'corfu-mode-map
            :states  'insert
            "C-@" #'completion-at-point
            "C-SPC" #'completion-at-point
            "C-n"   #'corfu-next
            "C-p"   #'corfu-previous)
  (:keymaps 'corfu-mode-map
            :states  'normal
            "C-SPC" (lambda () (progn (call-interactively #'evil-insert-state)
                                      (call-interactively #'completion-at-point))))
  (:keymaps 'corfu-map
            "C-k" #'corfu-previous
            "C-j" #'corfu-next
            "C-u" (lambda () (progn (let (corfu-cycle)
                                      (funcall-interactively #'corfu-next (- corfu-count)))))
            "C-d" (lambda () (progn (let (corfu-cycle)
                                      (funcall-interactively #'corfu-next corfu-count)))))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  :config
  (require 'orderless)

  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         (cons 'orderless-literal-prefix word)))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  (setq-local completion-styles '(orderless-fast basic))
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)

  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

  (setq global-corfu-minibuffer
        (lambda ()
          (not (or (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map)))))

  ;; (global-corfu-mode 1)
  )


;; (use-package corfu-candidate-overlay
;; :ensure (corfu-candidate-overlay
;; :repo "https://code.bsdgeek.org/adam/corfu-candidate-overlay"
;; :files (:defaults "*.el"))
;; :after corfu
;; :config
;; ;; enable corfu-candidate-overlay mode globally
;; ;; this relies on having corfu-auto set to nil
;; ;; (corfu-candidate-overlay-mode +1)
;;
;; (add-hook 'minibuffer-mode-hook (lambda () (progn (setq-local corfu-auto-nil)
;; (corfu-candidate-overlay-mode))))
;; ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
;; ;; (keybing <iso-lefttab> may not work for your keyboard model)
;; (global-set-key (kbd "<tab>") 'corfu-candidate-overlay-complete-at-point))

;; TODO for angular cape-sgml

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure nil
  :config
  ;; TODO: Doom's ignore regexp
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package cape
  :ensure t
  :init
  (setq cape-dabbrev-check-other-buffers t)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;; (use-package corfu-history
  ;; :ensure nil
  ;; :hook (corfu-mode . corfu-history-mode)
  ;; :config
  ;; (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :general
  (:keymaps 'corfu-popupinfo-map
            "C-h"      #'corfu-popupinfo-toggle
            "C-S-k"    #'corfu-popupinfo-scroll-down
            "C-S-j"    #'corfu-popupinfo-scroll-up
            "C-<up>"   #'corfu-popupinfo-scroll-down
            "C-<down>" #'corfu-popupinfo-scroll-up
            "C-S-p"    #'corfu-popupinfo-scroll-down
            "C-S-n"    #'corfu-popupinfo-scroll-up
            "C-S-u"    #'corfu-popupinfo-scroll-up
            "C-S-d"    #'corfu-popupinfo-scroll-down)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))

(use-package ws-butler
  :ensure (ws-butler :host github :repo "lewang/ws-butler")
  :defer t
  :hook (prog-mode . ws-butler-mode))

(use-package indent-bars
  :ensure (indent-bars :host github :repo "jdtsmith/indent-bars")
  ;; :hook ((typescript-ts-mode . indent-bars-mode)
         ;; (angular-ts-mode . indent-bars-mode))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types nil)
  (indent-bars-treesit-scope-min-lines 1)
  (indent-bars-treesit-scope '((typescript class_body program comment ternary_expression member_expression
                                           named_imports statement_block switch_case switch_default
                                           type_arguments type_parameters arguments array formal_parameters
                                           template_string template_substitution object_pattern object
                                           object_type enum_body class_body interface_body arrow_function
                                           parenthesized_expression binary_expression if_statement
                                           for_statement for_in_statement while_statement do_statement
                                           else_clause)))
  
  :config
  (setq indent-bars-starting-column 0)
  (advice-add 'line-move-to-column :around
	          (defalias 'my/indent-bars-prevent-passing-newline
	            (lambda (orig col &rest r)
		          (if-let ((indent-bars-mode)
			               (nlp (line-end-position))
			               (dprop (get-text-property nlp 'display))
			               ((seq-contains-p dprop ?\n))
			               ((> col (- nlp (point)))))
		              (goto-char nlp)
		            (apply orig col r)))))

  (setq indent-bars-color-by-depth nil
        indent-bars-color '(highlight :face-bg t :blend 0.01)
        indent-bars-highlight-current-depth '(:blend 0.01)
        indent-bars-ts-color '(highlight :face-bg t :blend 0.01)
        indent-bars-ts-highlight-current-depth '(no-inherit :blend 1.0))

  ;; (defun set-indent-bars-spacing () (setq-local indent-bars-spacing 2))
  ;; (add-hook 'indent-bars-mode-hook 'set-indent-bars-spacing)
  
  ;; (setopt indent-bars-ts-color '(highlight :face-bg t :blend 0.01))
  ;; (setopt indent-bars-ts-highlight-current-depth '(:blend 1.0))
  )

(use-package highlight-parentheses
  :ensure (highlight-parentheses :host "github.com" :repo "emacsmirror/highlight-parentheses")
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))

(use-package editorconfig
  :ensure t
  :defer t
  :hook (prog-mode . editorconfig-mode))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape)
  (setq eldoc-echo-area-use-multiline-p nil))
