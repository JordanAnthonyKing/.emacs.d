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

(use-package rainbow-mode)
(use-package olivetti
  :config
  (setq olivetti-body-width 120))

(set-face-attribute 'default nil :font "Berkeley Mono-10")
(setq widget-image-enable nil)
;; (use-package nano-theme
  ;; :ensure (nano-theme :host github :repo "https://github.com/rougier/nano-theme")
  ;; :custom
  ;; (nano-light-foreground "#424142")
  ;; (nano-light-highlight "#e7e7e8")
  ;; (nano-light-subtle "#d0d1d3")
  ;; (nano-light-faded "#929496")
  ;; (nano-light-salient "#573690")
  ;; ;; (nano-light-strong "#573690")
  ;; (nano-light-popout "#a64e2c")
  ;; (nano-light-critical "#a72434")
;; 
;; 
  ;; (nano-dark-foreground "#d1d6df")
  ;; (nano-dark-background "#000000")
  ;; ;; (nano-dark-highlight "#e7e7e8")
  ;; ;; (nano-dark-subtle "#d0d1d3")
  ;; ;; (nano-dark-faded "#929496")
  ;; (nano-dark-salient "#875afc")
  ;; ;; (nano-dark-strong "#573690")
  ;; (nano-dark-popout "#ffc200")
  ;; (nano-dark-critical "#ca2837")
  ;; :config
  ;; (defcustom nano-dark-string "#019a5e" 
    ;; "Popout colour is used for information that needs attention."
    ;; :type 'color 
    ;; :group 'nano-theme-dark)
  ;; (defcustom nano-dark-blue "#08a2ff" 
    ;; "Popout colour is used for information that needs attention."
    ;; :type 'color 
    ;; :group 'nano-theme-dark)
;; 
  ;; ;; Define the `nano-string` face directly and then apply it.
  ;; (defface nano-string 
    ;; `((t (:foreground ,nano-dark-string)))
    ;; "Face for strings in nano light theme."
    ;; :group 'nano-theme-dark)
  ;; (defface nano-blue
    ;; `((t (:foreground ,nano-dark-blue)))
    ;; "Face for strings in nano light theme."
    ;; :group 'nano-theme-dark)
;; 
  ;; ;; Apply the `nano-string` face to `font-lock-string-face`
  ;; (custom-set-faces
   ;; '(font-lock-string-face ((t (:inherit nano-string))))
   ;; '(font-lock-built-in-face ((t (:inherit nano-blue))))
   ;; '(font-lock-keyword-face ((t (:inherit nano-blue))))
   ;; '(font-lock-type-face ((t (:inherit nano-popout))))
   ;; )
 ;;  
  ;; ;; (nano-dark)
  ;; ;; (load-theme 'nano t)
  ;; )

(setq custom-safe-themes t)
;; (use-package doom-themes
  ;; :ensure t
  ;; :config
  ;; ;; Global settings (defaults)
  ;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        ;; doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; 
  ;; (load-theme 'doom-one t)
 ;;  
  ;; ;; Enable flashing mode-line on errors
  ;; ;; (doom-themes-visual-bell-config)
  ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config))

  (use-package kanagawa-theme
  :demand
  :config
  (load-theme 'kanagawa))


(use-package svg-tag-mode
  :ensure (svg-tag-mode :host github :repo "https://github.com/rougier/svg-tag-mode")
  :config
  ;;(setq svg-tag-tags
  ;;  '((":TODO:" . ((lambda (tag) (svg-tag-make " ! " :face 'nano-critical))))))
  (global-svg-tag-mode +1))

(setq project-vc-ignores '("target/" "bin/" "obj/" "node_modules/")
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


(use-package tab-bar-echo-area
  :ensure t
  :config
  (tab-bar-echo-area-mode 1))

(use-package project-tab-groups
  :after tab-bar-echo-area
  :ensure t
  :config
  (setq tab-bar-show nil)
  (setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-add-tab))
  (push #'project-switch-project tab-bar-echo-area-trigger-display-functions)
  (tab-bar-echo-area-apply-display-tab-names-advice)
  (project-tab-groups-mode 1))

(use-package helpful)
