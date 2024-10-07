;;; lsp.el --- Configuration for language servers -*- no-byte-compile: t; lexical-binding: t; -*-

(require 'comint)
(require 'ansi-color)
(require 'project)

(defvar ng-project "angular-app"
  "The Angular project name to use for `ng test`.")

(defvar ng-karma-config-path nil
  "The path to the Karma configuration file to use for `ng test`.")

(defun +project-root ()
  "Return the root directory of the current project as determined by `project.el`."
  (let ((project (project-current)))
    (if project
        (file-name-as-directory (project-root project))
      (error "Not in a project!"))))

(defun ng-path ()
  "Return the path to the Angular CLI executable `ng`."
  (expand-file-name "node_modules/@angular/cli/bin/ng" (+project-root)))

(defun run-ng-command (command buffer-name)
  "Run the given COMMAND in a comint buffer named BUFFER-NAME."
  (let* ((default-directory (+project-root))  ;; Ensure we are in the project root
         (buffer (get-buffer-create buffer-name))
         (comint-buffer (make-comint-in-buffer "Angular Test" buffer "bash" nil "-c" command)))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (comint-mode)
      (add-hook 'comint-output-filter-functions 'ansi-color-process-output nil t)
      (setq-local comint-scroll-to-bottom-on-output t))))

(defun run-ng-test-project (&optional headless)
  "Run `ng test` for the whole Angular project.
If HEADLESS is non-nil, run tests in Chrome headless mode."
  (interactive "P")  ;; Use "P" to pass the prefix argument interactively
  (let ((ng-path (ng-path))
        (karma-config-arg (if ng-karma-config-path
                              (concat "--karma-config " ng-karma-config-path)
                            ""))
        (browsers-arg "--browsers=Chrome --watch"))
    ;; Prepend the command with `node`
    (run-ng-command (format "node %s test %s %s %s" ng-path ng-project karma-config-arg browsers-arg
                               )
                       "*Angular Test*")))

(defun run-ng-test-file (&optional headless)
  "Run `ng test` for the current file, targeting its corresponding `.spec.ts` file.
If HEADLESS is non-nil, run tests in Chrome headless mode."
  (interactive "P")  ;; Use "P" to pass the prefix argument interactively
  (let* ((file (buffer-file-name))
         (relative-file (file-relative-name file (+project-root)))
         (spec-file (if (string-match-p "\\.spec\\.ts$" file)
                        relative-file
                      (replace-regexp-in-string "\\.ts$" ".spec.ts" relative-file)))
         (ng-path (ng-path))
         (karma-config-arg (if ng-karma-config-path
                               (concat "--karma-config " ng-karma-config-path)
                             ""))
         (browsers-arg "--browsers=Chrome --watch"))
    ;; Prepend the command with `node`
    (run-ng-command (format "node %s test %s --include=%s %s %s" ng-path ng-project spec-file karma-config-arg browsers-arg
                               )
                       "*Angular Test - Current File*")))


(setq ng-project "location")
(setq ng-karma-config-path "./apps/location/karma.conf.js")


(require 'treesit)

(define-derived-mode angular-ts-mode html-ts-mode "Angular"
  "Major mode for editing Angular flavoured HTML, powered by tree-sitter."
  :group 'angular
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.component\\.html\\'" "\\1.component.ts"))
  (add-to-list 'find-sibling-rules
               '("\\(.+\\)\\.container\\.html\\'" "\\1.container.ts")))

(when (treesit-ready-p 'angular)
  (add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . angular-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.container\\.html\\'" . angular-ts-mode)))

(provide 'angular-ts-mode)

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
               '("\\(.+\\)\\.spec\\.ts\\'" "\\1.component.ts"))

  (add-to-list 'compilation-error-regexp-alist 'node)

  (add-to-list 'compilation-error-regexp-alist-alist
          '(node "at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3)))

(setq treesit-font-lock-level 5)

;; TODO: Update angular in treesit langs fork
(use-package treesit-langs
  :ensure (treesit-langs
           :host github
           :repo "JordanAnthonyKing/treesit-langs"
           :files ("treesit-*.el" "queries"))
  :defer t
  :hook ((angular-ts-mode typescript-ts-mode) . treesit-hl-toggle)
  :custom
  (treesit-langs-git-dir nil)
  (treesit-langs-grammar-dir (expand-file-name "tree-sitter" user-emacs-directory))
  :config
  (add-to-list 'treesit-extra-load-path treesit-langs-grammar-dir))

(setq project-vc-ignores '("target/" "bin/" "obj/" "node_modules/")
      project-vc-extra-root-markers '(".project" "go.mod" "Cargo.toml"
                                      "project.clj" "pom.xml" "package.json"
                                      "angular.json" "Makefile" "README.org"
                                      "README.md"))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
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
  (add-to-list 'eglot-server-programs
               '(angular-ts-mode "ngserver"
                                 "--stdio"
                                 "--tsProbeLocations"
                                 "c:/Program Files/nodejs/node_modules"
                                 "--ngProbeLocations"
                                 "c:/Program Files/nodejs/node_modules/@angular/language-server/node_modules/"))

  (cl-callf plist-put eglot-events-buffer-config :size 0)

  (add-hook 'after-revert-hook 'eglot-reconnect))

;; (use-package dape
;;   :preface
;;   ;; By default dape shares the same keybinding prefix as `gud'
;;   ;; If you do not want to use any prefix, set it to nil.
;;   ;; (setq dape-key-prefix "\C-x\C-a")
;;
;;   :hook
;;   ;; Save breakpoints on quit
;;   ((kill-emacs . dape-breakpoint-save)
;;   ;; Load breakpoints on startup
;;    (after-init . dape-breakpoint-load))
;;
;;   :init
;;   ;; To use window configuration like gud (gdb-mi)
;;   ;; (setq dape-buffer-window-arrangement 'gud)
;;
;;   :config
;;   ;; Info buffers to the right
;;   (setq dape-buffer-window-arrangement 'right)
;;
;;   ;; Global bindings for setting breakpoints with mouse
;;   ;; (dape-breakpoint-global-mode)
;;
;;   ;; Pulse source line (performance hit)
;;   (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
;;
;;   ;; To not display info and/or buffers on startup
;;   ;; (remove-hook 'dape-start-hook 'dape-info)
;;   ;; (remove-hook 'dape-start-hook 'dape-repl)
;;
;;   ;; To display info and/or repl buffers on stopped
;;   (add-hook 'dape-stopped-hook 'dape-info)
;;   (add-hook 'dape-stopped-hook 'dape-repl)
;;
;;   ;; Kill compile buffer on build success
;;   ;; (add-hook 'dape-compile-hook 'kill-buffer)
;;
;;   ;; Save buffers on startup, useful for interpreted languages
;;   ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
;;
;;   ;; Projectile users
;;   ;; (setq dape-cwd-fn 'projectile-project-root)
;;   ;; (add-to-list 'dape-configs
;;   ;;          `(angular
;;   ;;   	 modes (js-mode js-ts-mode)
;;   ;;   	 command "node"
;;   ;;   	 command-cwd ,(concat user-emacs-directory "debug-adapters/js-debug")
;;   ;;   	 command-args "dapDebugAdapter.js"
;;   ;;   	 ;; port dape-configs-port
;;   ;;   	 :name "DEBUG"
;;   ;;   	 :userDataDir nil
;;   ;;   	 :type "chrome"
;;   ;;        :request "launch"
;;   ;;        :url "https://ied015.corp.soti.net/sotixsight/i/liveView"))
;;
;;   ;; (add-to-list 'dape-configs
;;   ;;              `(angular
;;   ;;                modes (typescript-mode typescript-ts-mode angular-ts-mode)
;;   ;;                command "node"
;;   ;;                port 9222
;;   ;;                command-cwd ,(concat user-emacs-directory "debug-adapters/js-debug")
;;   ;;                ;; command-args "dapDebugAdapter.js"
;;   ;;                command-args (,(concat user-emacs-directory "debug-adapters/js-debug/src/dapDebugServer.js") "9222")
;;   ;;                ;; :name "https://ied015.corp.soti.net/sotixsight/i/liveView"
;;   ;;                ;; ;; :userDataDir nil
;;   ;;                ;; :type "chrome"
;;   ;;                ;; :request "launch"
;;   ;;                ;; :url "https://ied015.corp.soti.net/sotixsight/i/liveView"
;;   ;;                :name "DEBUG"
;;   ;;   	         ;;:userDataDir nil
;;   ;;   	         :type "pwa-chrome"
;;   ;;   	         ;;:trace nil
;;   ;;                :request "attach"
;;   ;;   	         ;; :url  "https://ied015.corp.soti.net/sotixsight/i/liveView"
;;   ;;                ;; :port 9222
;;   ;;   	         ;; :webRoot "webpack://"
;;   ;;                ;;:sourceMaps t
;;   ;;               :webRoot ,(lambda ()
;;   ;;                               (read-string "Root: "
;;   ;;                                   (funcall dape-cwd-fn))))
;;   ;;
;;   ;;                )
;;
;;
;; (setq dape-configs-adapter-dir (file-name-as-directory (concat user-emacs-directory "debug-adapters")))
;;   (setq dape-configs-port 8123)
;;
;;   (add-to-list 'dape-configs
;; 	       `(js-debug-angular
;; 		 modes (js-mode js-ts-mode)
;; 		 command "node"
;; 		 command-cwd ,(concat dape-configs-adapter-dir "js-debug")
;; 		 command-args (,(concat user-emacs-directory "debug-adapters/js-debug/src/dapDebugServer.js") "8123")
;; 		 port dape-configs-port
;; 		 :name "DEBUG"
;; 		 :userDataDir nil
;; 		 :type "pwa-chrome"
;; 		 :trace nil
;; 		 ;; :url ,(lambda ()
;; 			 ;; (read-string "Url: "
;; 				      ;; "http://localhost:3000"))
;; 		 :webRoot ,(lambda ()
;; 			     (read-string "Root: "
;; 					  (funcall dape-cwd-fn))))))

(use-package flymake
  :ensure nil
  :defer t
  :hook ((angular-ts-mode typescript-ts-mode sgml-mode) . flymake-mode)
  :config
  (setq flymake-show-diagnostics-at-end-of-line 'short))

(use-package dumb-jump
  :ensure t
  :defer t
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
  (:keymaps 'corfu-mode-map
            :states 'normal
            "C-SPC" (lambda ()
                      (interactive)
                      (call-interactively #'evil-insert-state)
                      (call-interactively #'completion-at-point)))
  (:keymaps 'corfu-map
            "C-k" #'corfu-previous
            "C-j" #'corfu-next
            "TAB" #'corfu-next
            "[tab]" #'corfu-next
            "S-TAB" #'corfu-previous
            "[backtab]" #'corfu-previous
            "C-u" (lambda ()
                    (interactive)
                    (let ((corfu-cycle nil))
                      (call-interactively #'corfu-next (- corfu-count))))
            "C-d" (lambda ()
                    (interactive)
                    (let ((corfu-cycle nil))
                      (call-interactively #'corfu-next corfu-count))))

  (:keymaps 'corfu-map
            :states 'insert
            "C-SPC" #'corfu-insert-separator)
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  :config
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
        corfu-auto-delay 0
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)

  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                         #'corfu-send))))

  ;; Quit corfu when leaving insert state
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

  ;; Minibuffer completion behavior
  (setq global-corfu-minibuffer
        (lambda ()
          (not (or (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map))))))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure nil
  :config
  ;; TODO: Doom's ignore regexp
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'comint-mode))

(use-package cape
  :ensure t
  :init
  (setq cape-dabbrev-check-other-buffers t)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-sgml))

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

(use-package ws-butler
  :ensure (ws-butler :host github :repo "lewang/ws-butler")
  :defer t
  :hook (prog-mode . ws-butler-mode))

(use-package highlight-parentheses
  :ensure (highlight-parentheses :host "github.com" :repo "emacsmirror/highlight-parentheses")
  :defer t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-colors '("#ff6c6b")))

(use-package editorconfig
  :ensure t
  :defer t
  :hook (prog-mode . editorconfig-mode))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape)
  (setq eldoc-echo-area-use-multiline-p nil))
