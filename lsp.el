;;; lsp.el --- Configuration for language servers -*- no-byte-compile: t; lexical-binding: t; -*-

(setq text-mode-ispell-word-completion nil) ;; WTF
(setq treesit-font-lock-level 5)

;; (setq display-line-numbers-current-absolute nil)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'angular-ts-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'angular-ts-mode-hook #'hl-line-mode)

;; TODO: Update me
(use-package angular-ts-mode
  :ensure nil
  :load-path "lisp/"
  :defer nil
  :general
  (:keymaps 'angular-ts-mode-map
            :states 'normal
            "gd" #'xref-find-definitions)
  :config
  (add-hook 'angular-ts-mode-hook '(lambda ()
                                     (setq indent-bars-spacing-override 2)
                                     (indent-bars-mode t)))
  ;; (setq dabbrev-abbrev-skip-leading-regexp "\\<")
  ;; (setq dabbrev-abbrev-char-regexp "\\(\\sw\\|\\s_\\)")
  )

(use-package typescript-ts-mode
  :ensure nil
  :defer t
  :mode "\\.ts\\'"
  :config
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.ts\\'" "\\1.component.html"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.ts\\'" "\\1.spec.ts"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container\\.ts\\'" "\\1.container.html"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.spec\\.ts\\'" "\\1.ts"))

  ;; (add-to-list 'compilation-error-regexp-alist 'node)
  ;; (add-to-list 'compilation-error-regexp-alist-alist
  ;;              '(node "at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3))
  )


;; TODO: Update angular in treesit langs fork
(use-package treesit-langs
  :ensure (treesit-langs
           :host github
           :repo "JordanAnthonyKing/treesit-langs"
           :files ("treesit-*.el" "queries"))
  :defer nil
  :custom
  (treesit-langs-git-dir nil)
  (treesit-langs-grammar-dir (expand-file-name "tree-sitter" user-emacs-directory))
  :config
  (add-to-list 'treesit-extra-load-path treesit-langs-grammar-dir)
  (add-hook 'angular-ts-mode-hook #'(lambda () (treesit-hl-toggle 'on))))

;; TODO: Add stupid test folders
(setq project-vc-ignores '("target/" "bin/" "obj/" "node_modules/")
      project-vc-extra-root-markers '(".project" "go.mod" "Cargo.toml"
                                      "project.clj" "pom.xml" "package.json"
                                      "angular.json" "Makefile" "README.org"
                                      "README.md" "XSight.sln"))

(use-package markdown-mode
  :ensure t)

;; (use-package lsp-proxy
;;   :ensure (lsp-proxy :host "github.com" :repo "jadestrong/lsp-proxy" 
;;                        :files ("lsp-proxy.el" "lsp-proxy.exe" "./target/release/lsp-proxy.exe")
;;                        :pre-build (("cargo" "build" "--release") ("cp" "./target/release/lsp-proxy" "./")))
;;   :defer t
;;   :hook (tsx-ts-mode . #'lsp-proxy-mode)
;;   :hook (js-ts-mode . #'lsp-proxy-mode)
;;   :hook (typescript-mode . #'lsp-proxy-mode)
;;   :hook (typescript-ts-mode . #'lsp-proxy-mode)
;;   :hook (angular-ts-mode . #'lsp-proxy-mode)
;;   :init
;;   (setq lsp-proxy-diagnostics-provider :flymake))

(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp)
  ;; :hook (csharp-mode lsp)
  ;; :hook (csharp-ts-mode lsp)
  :config
  (setq lsp-csharp-omnisharp-enable-decompilation-support t
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable t
        lsp-modeline-code-actions-segments nil
        lsp-headerline-breadcrumb-segments '(file symbols)))

(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot eglot-ensure)
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (angular-ts-mode . eglot-ensure)
  :hook (js-mode . eglot-ensure)
  :hook (js-ts-mode . eglot-ensure)
  :general
  (:keymaps 'eglot-mode-map
            :states 'normal
            "ga" #'eglot-code-actions)
  :init
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-auto-display-help-buffer nil
        eglot-code-action-indications nil)
  :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '(angular-ts-mode . ,(eglot-alternatives
  ;;                                   '(("vscode-html-language-server" "--stdio")
  ;;                                     ("html-languageserver" "--stdio")))))

  (add-to-list 'eglot-server-programs
               '(angular-ts-mode "node"
                                 "/Program Files/nodejs/node_modules/@angular/language-server"
                                 "--ngProbeLocations"
                                 "/Program Files/nodejs/node_modules"
                                 "--tsProbeLocations"
                                 "/Program Files/nodejs/node_modules"
                                 "--stdio"))

  (cl-callf plist-put eglot-events-buffer-config :size 0))

;; TODO: Embark
;; (use-package consult-eglot
;;   :ensure t
;;   :commands consult-eglot-symbols
;;   :defer t)

(use-package js-pkg-mode
  :ensure (js-pkg-mode :host "github.com" :repo "https://github.com/ovistoica/js-pkg-mode")
  ;; TODO: Defer this
  :init (js-pkg-global-mode 1))

(add-hook 'compilation-mode-hook '(lambda () (setq toggle-truncate-lines nil)))

                                        ;(use-package npm
                                        ; :ensure t
                                        ;  :defer t
                                        ;  :init
                                        ; (setq compilation-scroll-output t)
                                        ;  :config
                                        ;  (add-to-list 'compilation-error-regexp-alist-alist
                                        ;               '(angular-warning
                                        ;                 "^Deprecation Warning on line \\([0-9]+\\), column \\([0-9]+\\) of \\(.+\\):"
                                        ;                 3 1 2 1))
                                        ;  (add-to-list 'compilation-error-regexp-alist-alist
                                        ;               '(angular-error
                                        ;                 "^Error: \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - error TS[0-9]+:"
                                        ;                 1 2 3 2))
                                        ;  (add-to-list 'compilation-error-regexp-alist 'angular-warning)
                                        ;  (add-to-list 'compilation-error-regexp-alist 'angular-error)
                                        ;  (add-hook 'npm-mode-hook (lambda ()
                                        ;                             (setq-local compilation-error-regexp-alist (list 'angular-warning ;'angular-error))))
                                        ;  ;; (add-hook 'npm-mode-hook (lambda () (visual-line-mode t)))
                                        ;  (add-hook 'npm-mode-hook (lambda () (toggle-truncate-lines nil)))
                                        ;  ;; (add-hook 'npm-mode-hook 'ansi-colorful-mode)
                                        ;  ;; (defun ansi-enable-disable ()
                                        ; ;;   (progn
                                        ; ;;     (ansi-colorful--disable)
                                        ; ;;     (ansi-colorful--enable)))
                                        ;  ;; (debounce! ansi-enable-disable 0.2)
                                        ;  ;; (add-hook 'compilation-filter-hook #'ansi-enable-disable)
                                        ;  (advice-add 'npm-common--compile :around
                                        ;              (lambda (orig-fun &rest args)
                                        ;                (let ((default-directory (project-root (project-current t))))
                                        ;                  (apply orig-fun args)))))

;; (use-package fancy-compilation
;;   :commands (fancy-compilation-mode))
;; 
;; (with-eval-after-load 'compile
;;   (fancy-compilation-mode))

(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :init
  ;; (add-hook 'flymake-mode-hook (lambda () (set-fringe-style '(nil . 4))))
  ;; (add-hook 'flymake-mode (lambda () (fringe-mode '(left-fringe-width . 4))))
  ;; (add-hook 'flymake-mode-hook
  ;;           (lambda ()
  ;;             (set-fringe-style (cons (frame-parameter nil 'left-fringe) 4))))


  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flymake-fringe-bitmap-double-arrow [16 48 112 240 112 48 16] nil nil 'center)

  (setq flymake-error-bitmap '( flymake-fringe-bitmap-double-arrow modus-themes-prominent-error ))
  (setq flymake-warning-bitmap '( flymake-fringe-bitmap-double-arrow modus-themes-prominent-warning ))
  (setq flymake-note-bitmap '( flymake-fringe-bitmap-double-arrow modus-themes-prominent-note ))

  :config
  ;; (setq flymake-show-diagnostics-at-end-of-line 'short)

  (setq flymake-indicator-type 'fringes) 
  (setq flymake-fringe-indicator-position 'right-fringe))

;; This works but the advice method doesn't
;;   (defun flymake-diagnostic-oneliner (diag &optional nopaintp)
;;     "Get truncated one-line text string for diagnostic DIAG.
;; This is useful for displaying the DIAG's text to the user in
;; confined spaces, such as the echo area.  Unless NOPAINTP is t,
;; propertize returned text with the `echo-face' property of DIAG's
;; type."
;;     (let* ((txt (flymake-diagnostic-text diag))
;;            ;; Remove leading 'typescript [####]: ' if present
;;            (txt (if (string-match "^typescript \\[[0-9]+\\]: " txt)
;;                     (substring txt (match-end 0))
;;                   txt))
;;            ;; Truncate text at the first newline
;;            (txt (substring txt 0 (cl-loop for i from 0 for a across txt
;;                                           when (eq a ?\n) return i))))
;;       (if nopaintp txt
;;         (propertize txt 'face
;;                     (flymake--lookup-type-property
;;                      (flymake-diagnostic-type diag) 'echo-face 'flymake-error)))))

;;   (defun flymake-diagnostic-oneliner-remove-typescript-prefix (orig-fun diag &optional nopaintp)
;;   "Advice to remove 'typescript [####]: ' prefix from DIAG before calling ORIG-FUN."
;;   (let ((new-diag diag))
;;     (when-let ((txt (flymake-diagnostic-text diag)))
;;       ;; Check for and remove the prefix 'typescript [####]: '
;;       (when (string-match "^typescript \\[[0-9]+\\]: " txt)
;;         (setf (flymake-diagnostic-text diag)
;;               (substring txt (match-end 0)))))
;;     ;; Call the original function with the modified diagnostic
;;     (funcall orig-fun new-diag nopaintp)))
;; 
;; (advice-add 'flymake-diagnostic-oneliner :around
;;             #'flymake-diagnostic-oneliner-remove-typescript-prefix)
;; )

;; (use-package sideline-flymake
;;   :hook (flymake-mode . sideline-mode)
;;   :init
;;   (setq sideline-flymake-display-mode 'point
;;         sideline-flymake-max-line 10) ; 'point to show errors only on point
;;                                               ; 'line to show errors on the current line
;;   (setq sideline-backends-right '(sideline-flymake)))
;; 
;; (use-package sideline
;;   :init
;;   (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
;;         sideline-backends-right-skip-current-line nil  ; don't display on current line (right)
;;         sideline-order-left 'down                    ; or 'up
;;         sideline-order-right 'down                     ; or 'down
;;         sideline-format-left "%s   "                 ; format for left aligment
;;         sideline-format-right "   %s"                ; format for right aligment
;;         ;; sideline-format-right "%s"                ; format for right aligment
;;         sideline-priority 100                        ; overlays' priority
;;         sideline-display-backend-name nil))          ; display the backend name

;; TODO: Hook
(use-package dumb-jump
  :ensure (dumb-jump :host "github.com" :repo "JordanAnthonyKing/dumb-jump")
  :defer nil
  :config
  (setq dumb-jump-prefer-search 'rg
        dumb-jump-aggressive nil
        dumb-jump-selector 'completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package corfu
  :ensure t
  :defer t
  :hook (elpaca-after-init . global-corfu-mode)
  :general
  (:keymaps 'corfu-mode-map
            ;; :states 'insert
            "C-@" #'completion-at-point
            "C-SPC" #'completion-at-point
            "C-n" #'corfu-next
            "C-p" #'corfu-previous)
  (:keymaps 'corfu-map
            :states 'insert
            "C-SPC" #'corfu-insert-separator
            "C-k" #'corfu-previous
            "C-j" #'corfu-next
            "TAB" #'corfu-next
            "S-TAB" #'corfu-previous
            "C-u" (lambda ()
                    (interactive)
                    (let ((corfu-cycle nil))
                      (call-interactively #'corfu-next (- corfu-count))))
            "C-d" (lambda ()
                    (interactive)
                    (let ((corfu-cycle nil))
                      (call-interactively #'corfu-next corfu-count))))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  (corfu-right-margin-width 0)
  (corfu-left-margin-width 0)
  :config
  (keymap-unset corfu-map "RET")

  (require 'orderless)

  ;; Orderless fast dispatch for small literals
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         (cons 'orderless-literal-prefix word)))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  ;; Configure corfu completion styles and settings
  (setq-local completion-styles '(orderless-fast basic))
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.1
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)

  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

  ;; Minibuffer completion behavior
  (setq global-corfu-minibuffer
        (lambda ()
          (not (or (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map))))))

(defvar +corfu-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-dabbrev'.")

(use-package cape
  :ensure t
  :defer nil
  :config
  (defun +corfu-add-cape-file-h ()
    (add-hook 'completion-at-point-functions #'cape-file))
  (add-hook 'prog-mode-hook #'+corfu-add-cape-file-h)
  ;; (defun +corfu-add-cape-elisp-block-h ()
  ;;   (add-hook 'completion-at-point-functions #'cape-elisp-block))

  ;; (add-hook 'org-mode-hook #'+corfu-add-cape-elisp-block-h)
  ;; (add-hook 'markdown-mode-hook #'+corfu-add-cape-elisp-block-h)

  ;; Enable Dabbrev completion basically everywhere as a fallback.
  (setq cape-dabbrev-min-length 1)
  (setq cape-dabbrev-check-other-buffers t)
  ;; Set up `cape-dabbrev' options.
  (defun +dabbrev-friend-buffer-p (other-buffer)
    (< (buffer-size other-buffer) +corfu-buffer-scanning-size-limit))

  (defun +corfu-add-cape-dabbrev-h ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev))

  (defun add-cape-dabbrev-to-modes ()
    (dolist (hook '(prog-mode-hook
                    text-mode-hook
                    sgml-mode-hook
                    conf-mode-hook
                    comint-mode-hook
                    minibuffer-setup-hook
                    eshell-mode-hook))
      (add-hook hook #'+corfu-add-cape-dabbrev-h)))

  (add-cape-dabbrev-to-modes)

  (require 'dabbrev)
  (setq dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
        dabbrev-ignored-buffer-regexps
        '("\\` "
          "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")
        dabbrev-upcase-means-case-search t)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)

  ;; Make these capfs composable.
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(use-package corfu-history
  :ensure nil
  :hook (corfu-mode . corfu-history-mode)
  :config
  (require 'savehist)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; TODO: Elisp?
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

;; (use-package ws-butler
;;   :ensure (ws-butler :host github :repo "lewang/ws-butler")
;;   :defer t
;;   :hook (prog-mode . ws-butler-mode))

(use-package highlight-parentheses
  :ensure (highlight-parentheses :host "github.com" :repo "emacsmirror/highlight-parentheses")
  :defer t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-colors '("#000000")))

(use-package editorconfig
  :ensure t
  :defer t
  :hook (prog-mode . editorconfig-mode))

(use-package prettier
  :ensure t
  :defer t
  :hook (typescript-ts-mode . prettier-mode)
  :hook (angular-ts-mode . prettier-mode)
  :config
  (add-to-list 'prettier-major-mode-parsers '(angular-ts-mode angular))
  (add-to-list 'prettier-major-mode-parsers '(typescript-ts-mode babel-ts)))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape)
  ;; (setq eldoc-echo-area-use-multiline-p nil)
  )

;; (use-package diff-hl
;;   :ensure (diff-hl :host "github.com" :repo "https://github.com/dgutov/diff-hl")
;;   :hook ((prog-mode . diff-hl-mode)
;;          (prog-mode . diff-hl-margin-mode))
;;   :config
;;   (setq diff-hl-margin-symbols-alist
;;         ((insert . "+") (delete . "-") (change . "!") (unknown . "?") (ignored . "i"))))

;; (use-package indent-bars
;;   :hook (prog-mode . indent-bars-mode)
;;   :hook (angular-ts-mode . indent-bars-mode)
;;   :init
;;   ;; (add-hook 'indent-bars-mode (lambda () (advice-add 'line-move-to-column :around
;;   ;;             (defun my/indent-bars-prevent-passing-newline (orig col &rest r)
;;   ;;               (if-let ((indent-bars-mode)
;;   ;;   	                 (nlp (line-end-position))
;;   ;;   	                 (dprop (get-text-property nlp 'display))
;;   ;;   	                 ((seq-contains-p dprop ?\n))
;;   ;;   	                 ((> col (- nlp (point)))))
;;   ;;                   (goto-char nlp)
;;   ;;                 (apply orig col r))))))
;; 
;;   ;; (setq ;; indent-bars-prefer-character t
;;   ;;  ;; indent-bars-no-stipple-char ?\┃
;;   ;;  indent-bars-color-by-depth nil
;;   ;;  ;; indent-bars-color '("#595959")
;;   ;;  indent-bars-color '("#ddd")
;;   ;;  indent-bars-highlight-current-depth '(:color "#212121"))
;; 
;; (setopt indent-bars-color '(highlight :face-bg t :blend 0.4)
;; 		indent-bars-pattern "."
;;         indent-bars-starting-column 0
;; 		indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 0.8)
;; 		;; indent-bars-highlight-current-depth '(:blend 0.8 :width 0.3 :pad 0.3 :pattern ".")
;; 		indent-bars-highlight-current-depth nil
;; 		indent-bars-pad-frac 0.3)
;; )

