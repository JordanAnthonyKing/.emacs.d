;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

(use-package auto-compile
  :demand t
  :custom
  (auto-compile-check-parens nil)
  (auto-compile-display-buffer nil)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package gcmh
  :ensure t
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-low-cons-threshold minimal-emacs-gc-cons-threshold)
  :config (gcmh-mode +1))

(minimal-emacs-load-user-init "evil.el")
(minimal-emacs-load-user-init "completion.el")
;;(minimal-emacs-load-user-init "bindings.el")
;;(minimal-emacs-load-user-init "elisp.el")
;;(minimal-emacs-load-user-init "bindings.el")
(minimal-emacs-load-user-init "lsp.el")

(setq scroll-margin 10
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-progressive-speed nil)

(defun reload-init-file ()
  "Reload the Emacs configuration file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(setq default-directory (getenv "HOME"))



(set-face-attribute 'default nil :font "Berkeley Mono-10")
(setq widget-image-enable nil)
(use-package nano-theme
  :ensure (nano-theme :host github :repo "https://github.com/rougier/nano-theme")
  :config
  (nano-light)
  (load-theme 'nano t))

(use-package svg-tag-mode
  :ensure (svg-tag-mode :host github :repo "https://github.com/rougier/svg-tag-mode")
  :config
  ;;(setq svg-tag-tags
  ;;  '((":TODO:" . ((lambda (tag) (svg-tag-make " ! " :face 'nano-critical))))))
  (global-svg-tag-mode +1))

(setq project-vc-ignores '("target/" "bin/" "obj/")
      project-vc-extra-root-markers '(".project"
                                      "go.mod"
                                      "Cargo.toml"
                                      "project.clj"
                                      "pom.xml"
                                      "package.json"
                                      "angular.json"
                                      "Makefile"
                                      "README.org"
                                      "README.md"))


(use-package tabspaces
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "¯\_(ツ)_/¯")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; (tabspaces-initialize-project-with-todo t)
  ;; (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  :config
  (setq tab-bar-show nil)
  ;; Filter Buffers for Consult-Buffer
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                           :predicate #'tabspaces--local-buffer-p
                           :sort 'visibility
                           :as #'buffer-name)))
  
    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace)

  (tabspaces-mode))
