;;; lsp.el --- Configuration for language servers -*- no-byte-compile: t; lexical-binding: t; -*-

(setq text-mode-ispell-word-completion nil) ;; WTF
(setq treesit-font-lock-level 5)
;; (setq display-line-numbers 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(defmacro debounce! (func &optional delay)
  "Return a debounced version of function FUNC.

DELAY defaults to 0.5 seconds."
  `(let ((debounce-timer nil)
         (delay (or ,delay 0.5)))
     (defun ,(intern (concat (symbol-name func) "--debounced")) (&rest args)
       ;; Add the documentation from FUNC with an extra note
       ,(concat
         (documentation func)
         (format "\n\nThis function is debounced -- it runs after a delay of %.3f seconds." delay))
       ;; Add the interactive form from FUNC
       ,(interactive-form func)
       (if (timerp debounce-timer)
           (timer-set-idle-time debounce-timer delay)
         (let ((buf (current-buffer)))
           (setq debounce-timer
                 (run-with-idle-timer
                  delay nil
                  (lambda ()
                    (cancel-timer debounce-timer)
                    (setq debounce-timer nil)
                    (with-current-buffer buf
                      (apply #',func args))))))))))

(require 'treesit)

(defcustom angular-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `angular-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'angular)

(defun my/find-symbol-in-sibling-file ()
  "Search for the symbol under point in the sibling file determined by 'find-sibling-file'.
If the sibling file isn't open, open it without focusing on it. Display matches using completing-read for filtering."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t))
         (current-file (buffer-file-name))
         (sibling-file (when current-file
                         (find-sibling-file current-file)))
         (buffer (and sibling-file
                      (or (get-file-buffer sibling-file)
                          (find-file-noselect sibling-file))))
         (matches nil))
    (if (null symbol)
        (message "No symbol under point.")
      (if (null sibling-file)
          (message "No sibling file found.")
        (with-current-buffer buffer
          (goto-char (point-min))
          (while (search-forward symbol nil t)
            (let ((line-content (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
              (push line-content matches))))
        (if matches
            (let ((selected (completing-read "Matches: " matches)))
              (message "You selected: %s" selected))
          (message "No matches found for symbol '%s' in sibling file." symbol))))))

;; TODO: Update me
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

(defvar angular-syntax-table 
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    ;; (modify-syntax-entry ?< " " table)
    ;; (modify-syntax-entry ?> " " table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. " " table) table))

(define-derived-mode angular-ts-mode html-ts-mode "Angular"
  "Major mode for editing Angular flavoured HTML, powered by tree-sitter."
  :group 'angular
  :syntax-table angular-syntax-table
  (setq-local treesit-simple-indent-rules angular-ts-mode--indent-rules)

  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component.html\\'" "\\1.component.ts"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container.html\\'" "\\1.container.ts")))


(when (treesit-ready-p 'angular)
  (add-to-list 'auto-mode-alist '("\\.component.html\\'" . angular-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.container.html\\'" . angular-ts-mode)))

(provide 'angular-ts-mode)

(use-package angular-ts-mode
  :ensure nil
  :general 
  (:keymaps 'angular-ts-mode-map
            :states 'normal
            "gd" #'xref-find-definitions)
  ;; :config
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
               '("\\(.+\\)\\.component\\.ts\\'" "\\1.component.spec.ts"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container\\.ts\\'" "\\1.container.html"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container\\.spec\\.ts\\'" "\\1.container.ts"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.spec\\.ts\\'" "\\1.component.ts"))

  (add-to-list 'compilation-error-regexp-alist 'node)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(node "at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3)))


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
  (add-hook 'angular-ts-mode-hook #'(lambda () (treesit-hl-toggle 'on)))
  (add-hook 'typescript-ts-mode-hook #'(lambda () (treesit-hl-toggle 'on))))

(setq project-vc-ignores '("target/" "bin/" "obj/" "node_modules/")
      project-vc-extra-root-markers '(".project" "go.mod" "Cargo.toml"
                                      "project.clj" "pom.xml" "package.json"
                                      "angular.json" "Makefile" "README.org"
                                      "README.md"))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook (typescript-ts-mode . eglot-ensure)
  :defer t
  :init
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-auto-display-help-buffer nil)

  :config
  ;; (setq eglot-stay-out-of '(xref))
  (cl-callf plist-put eglot-events-buffer-config :size 0))

;; TODO: Embark
(use-package consult-eglot
  :ensure t
  :commands consult-eglot-symbols
  :defer t)

(use-package npm
  :ensure t
  :defer t
  :init
  (setq compilation-scroll-output t)
  :config
  (add-to-list 'compilation-error-regexp-alist-alist
               '(angular-warning
                 "^Deprecation Warning on line \\([0-9]+\\), column \\([0-9]+\\) of \\(.+\\):"
                 3 1 2 1))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(angular-error
                 "^Error: \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) - error TS[0-9]+:"
                 1 2 3 2))
  (add-to-list 'compilation-error-regexp-alist 'angular-warning)
  (add-to-list 'compilation-error-regexp-alist 'angular-error)
  (add-hook 'npm-mode-hook (lambda ()
                             (setq-local compilation-error-regexp-alist (list 'angular-warning 'angular-error))))
  ;; (add-hook 'npm-mode-hook (lambda () (visual-line-mode t)))
  (add-hook 'npm-mode-hook (lambda () (toggle-truncate-lines nil)))
  (add-hook 'npm-mode-hook 'ansi-colorful-mode)
  (defun ansi-enable-disable ()
    (progn
      (ansi-colorful--disable)
      (ansi-colorful--enable)))
  (debounce! ansi-enable-disable 0.2)
  (add-hook 'compilation-filter-hook #'ansi-enable-disable)
  (advice-add 'npm-common--compile :around
              (lambda (orig-fun &rest args)
                (let ((default-directory (project-root (project-current t))))
                  (apply orig-fun args)))))

(use-package flymake
  :ensure nil
  :defer t
  :hook ((angular-ts-mode typescript-ts-mode sgml-mode) . flymake-mode)
  :config
  ;; (setq flymake-show-diagnostics-at-end-of-line 'short)

  ;; This works but the advice method doesn't
  (defun flymake-diagnostic-oneliner (diag &optional nopaintp)
    "Get truncated one-line text string for diagnostic DIAG.
This is useful for displaying the DIAG's text to the user in
confined spaces, such as the echo area.  Unless NOPAINTP is t,
propertize returned text with the `echo-face' property of DIAG's
type."
    (let* ((txt (flymake-diagnostic-text diag))
           ;; Remove leading 'typescript [####]: ' if present
           (txt (if (string-match "^typescript \\[[0-9]+\\]: " txt)
                    (substring txt (match-end 0))
                  txt))
           ;; Truncate text at the first newline
           (txt (substring txt 0 (cl-loop for i from 0 for a across txt
                                          when (eq a ?\n) return i))))
      (if nopaintp txt
        (propertize txt 'face
                    (flymake--lookup-type-property
                     (flymake-diagnostic-type diag) 'echo-face 'flymake-error)))))

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
  )

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'point) ; 'point to show errors only on point
                                              ; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)))

(use-package sideline
  :init
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line nil  ; don't display on current line (right)
        sideline-order-left 'down                    ; or 'up
        sideline-order-right 'down                     ; or 'down
        sideline-format-left "%s   "                 ; format for left aligment
        sideline-format-right "   %s"                ; format for right aligment
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name nil))          ; display the backend name

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
            :states 'insert
            "C-@" #'completion-at-point
            "C-SPC" #'completion-at-point
            "C-n" #'corfu-next
            "C-p" #'corfu-previous)
  (:keymaps 'corfu-map
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
  (setq highlight-parentheses-colors '("#fda50f")))

(use-package editorconfig
  :ensure t
  :defer t
  :hook (prog-mode . editorconfig-mode))

(use-package prettier
  :ensure t
  :config
  (add-to-list 'prettier-major-mode-parsers '(angular-ts-mode angular))
  (global-prettier-mode))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape)
  (setq eldoc-echo-area-use-multiline-p nil))

