;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-hook 'elpaca-after-init-hook #'global-auto-revert-mode)
;; TODO: Defer this to consult-recentf like Doom
(add-hook 'elpaca-after-init-hook #'recentf-mode)
(add-hook 'elpaca-after-init-hook #'savehist-mode)
(add-hook 'elpaca-after-init-hook #'save-place-mode)

;; TODO: Make this work wtih minimal emacs paths
(setq auto-save-file-name-transforms '((".*" "c:/dev/.emacs.d/var/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "c:/dev/.emacs.d/var/backups/")))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "c:/dev/.emacs.d/var/autosaves/" t)

(setq message-truncate-lines t)

(setq scroll-margin 10)
;; mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
;; mouse-wheel-progressive-speed nil)

;; (set-window-fringes (selected-window) 0 0 nil)
(setq-default left-fringe-width 2)
(setq-default right-fringe-width 2)
(fringe-mode '2)

(global-visual-line-mode)

(setq window-divider-default-bottom-width 0)

(use-package msgu
   :ensure (msgu :host "github.com" :repo "jcs-elpa/msgu"))

(use-package elenv
   :ensure (elenv :host "github.com" :repo "jcs-elpa/elenv"))

(use-package auto-scroll-bar
  :ensure (auto-scroll-bar :host "github.com" :repo "emacs-vs/auto-scroll-bar")
  :config
  (setq auto-scroll-bar-horizontal nil)
  (auto-scroll-bar-mode))

(use-package ansi-colorful
   :ensure (ansi-colorful :host "github.com" :repo "jcs-elpa/ansi-colorful"))

;; (setq-default indicate-buffer-boundaries nil)
;; (setq-default indicate-empty-lines nil)

(use-package general
  :ensure (:wait t)
  :demand t)

(use-package vundo
  :ensure t
  :commands vundo)

(use-package rainbow-mode
  :ensure nil
  :commands rainbow-mode)

(use-package olivetti
  :ensure t
  :commands olivetti-mode)

(use-package pulsar
  :ensure t
  :hook (elpaca-after-init . pulsar-global-mode))

(set-face-attribute 'default nil :font "Berkeley Mono-10")

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t)
;;   (doom-themes-org-config))

;; (use-package mood-line
  ;; :config (mood-line-mode))

;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   ;; (require 'modus-themes)
;;
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil)
;;
;;   ;; Load the theme of your choice.
;;   (load-theme 'modus-operandi :no-confirm))

;; (use-package nothing-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nothing t))

(use-package almost-mono-themes
  :ensure t
  :config
  (load-theme 'almost-mono-white t))

(use-package minions
  :ensure t
  :hook (elpaca-after-init . minions-mode))

;; (use-package persistent-scratch
;; :ensure t
;; :config (persistent-scratch-setup-default))

(minimal-emacs-load-user-init "bindings.el")
(minimal-emacs-load-user-init "evil.el")
;; TODO: Rename to vertico
(minimal-emacs-load-user-init "completion.el")
(minimal-emacs-load-user-init "magit.el")
(minimal-emacs-load-user-init "lsp.el")
(minimal-emacs-load-user-init "bindings.el")

;; (use-package svg-tag-mode
;;   :ensure (svg-tag-mode :host github :repo "https://github.com/rougier/svg-tag-mode")
;;   :config
;;   (setq svg-tag-tags
;;     ;; '((":TODO:" . ((lambda (tag) (svg-tag-make " ! " :face 'nano-critical))))))
;;     '(("Head:" . ((lambda (tag) (svg-tag-make " HEAD " :face 'default))))
;;       ("Merge:" . ((lambda (tag) (svg-tag-make " MERGE " :face 'default)))))
;;   (global-svg-tag-mode +1)))

(use-package helpful)

;; TODO: steal doom's word wrap

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :ensure t
  :defer t
  :hook (elpaca-after-init . global-ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://")))

(use-package eat
  :ensure (eat :host codeberg
               :repo "thearcticcat/emacs-eat"
               :branch "windows-hack"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))
  :commands (eat)
  :config
  ;; TODO: Set scroll margin locally
  ;; (add-hook 'eat-mode-hook (lambda () (setq-local scroll-conservatively 10000)))
  )

(use-package current-window-only
  :ensure (current-window-only
           :host github
           :repo "FrostyX/current-window-only")
  ;; :hook (elapca-after-init . current-window-only-mode)
  :config
  (setq magit-commit-diff-inhibit-same-window t)
  (current-window-only-mode))

