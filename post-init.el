;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; (add-hook 'elpaca-after-init-hook 'global-auto-revert-mode)
;; TODO: Defer this to consult-recentf like Doom
(add-hook 'elpaca-after-init-hook 'recentf-mode)
(add-hook 'elpaca-after-init-hook 'savehist-mode)
(add-hook 'elpaca-after-init-hook 'save-place-mode)

(set-display-table-slot standard-display-table 0 ?\ ) 

;; TODO: Make this work wtih minimal emacs paths
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name ".emacs.d/var/autosaves/\\1" minimal-emacs-user-directory) t)))
(setq backup-directory-alist `((".*" . ,(expand-file-name ".emacs.d/var/backups/" minimal-emacs-user-directory))))
(make-directory (expand-file-name ".emacs.d/var/autosaves/" minimal-emacs-user-directory) t)
;; (setq message-truncate-lines t)
(setq scroll-margin 10)
;; mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
;; mouse-wheel-progressive-speed nil)

(setf (cdr (assoc 'truncation fringe-indicator-alist)) '(nil nil))
(setf (cdr (assoc 'continuation fringe-indicator-alist)) '(nil nil))

(setq widget-button-prefix " ")
(setq widget-button-suffix " ")
(setq widget-link-prefix " ")
(setq widget-link-suffix " -> ")
(setq widget-push-button-prefix " ")
(setq widget-push-button-suffix " ")

;; (use-package ultra-scroll
;;   :ensure (ultra-scroll :host "github.com" :repo "jdtsmith/ultra-scroll")
;;   :init
;;   (setq scroll-conservatively 101
;;         scroll-margin 0
;;         pixel-scroll-precision-use-momentum nil
;;         pixel-scroll-precision-interpolate-page t)
;;   :config
;;   (ultra-scroll-mode 1))

;; Add the local lisp directory to the load-path
(add-to-list 'load-path (expand-file-name "themes" minimal-emacs-user-directory))

(use-package modus-themes
  :ensure nil
  :load-path "themes/"
  :config
  (load-theme 'modus-operandi :no-confirm))

;; Use use-package to load the local package
;; (use-package lambda-themes
;;   :ensure nil
;;   :load-path "themes/"    ; Relative path to the local package directory
;;   :config
;;   ;; Your package configuration goes here
;;   (load-theme 'lambda-dark))

;; (use-package evangelion-theme
;;   :ensure t
;;   ;; :custom
;;   ;; `nil' to disable background for comments
;;   ;; (evangelion-comment-background-enabled . t)
;;   :config
;;   (load-theme 'evangelion t))

;; (use-package miasma-theme
;;   :ensure (miasma-theme :host "github.com" :repo "daut/miasma-theme.el")
;;   :config
;;   (load-theme 'miasma t))

;; (use-package msgu
;;    :ensure (msgu :host "github.com" :repo "jcs-elpa/msgu"))
;; 
;; (use-package elenv
;;    :ensure (elenv :host "github.com" :repo "jcs-elpa/elenv"))

;; (use-package auto-scroll-bar
;;   :ensure (auto-scroll-bar :host "github.com" :repo "emacs-vs/auto-scroll-bar")
;;   :config
;;   (setq auto-scroll-bar-horizontal nil)
;;   (auto-scroll-bar-mode))

;; (use-package ansi-colorful
;;   :ensure (ansi-colorful :host "github.com" :repo "jcs-elpa/ansi-colorful")
;;   :commands ansi-colorful-mode
;;   :defer t)

(use-package general
  :ensure (:wait t)
  :demand t)

(use-package rainbow-mode
  :ensure nil
  :commands rainbow-mode)

(use-package olivetti
  :ensure t
  :commands olivetti-mode)

(use-package pulsar
  :ensure t
  :commands pulsar-global-mode
  :hook (elpaca-after-init . pulsar-global-mode))

;; (set-face-attribute 'default nil :font "Berkeley Mono-10")
(set-face-attribute 'default nil :font "Berkeley Mono ExtraCondensed-10")


;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t)
;;   (doom-themes-org-config))

;; (use-package doom-modeline
;;   :ensure (doom-modeline :host "github.com" :repo "JordanAnthonyKing/doom-modeline")
;;   :config
;;   (setq doom-modeline-bar-width 2
;;         doom-modeline-hud nil
;;         doom-modeline-project-detection 'project
;;         doom-modeline-buffer-file-name-style 'file-name
;;         doom-modeline-icon nil
;;         doom-modeline-buffer-state-icon nil
;;         doom-modeline-lsp-icon nil
;;         doom-modeline-time-icon nil
;;         doom-modeline-buffer-encoding nil
;;         doom-modeline-vcs-icon nil
;;         doom-modeline-project-name nil
;;         doom-modeline-workspace-name nil
;;         doom-modeline-persp-name nil
;;         doom-modeline-modal-icon nil
;;         doom-modeline-modal nil
;;         doom-modeline-gnus nil
;;         doom-modeline-battery nil
;;         doom-modeline-time nil)
;;   (doom-modeline-mode 1))

;; (use-package miasma-theme
;;   :ensure (miasma-theme :host github :repo "daut/miasma-theme.el")
;;   :config
;;   (load-theme 'miasma t))

;; (use-package almost-mono-themes
  ;; :ensure t
  ;; :config
  ;; (load-theme 'almost-mono-white t))

;; (use-package lambda-line
;;   :ensure (lambda-line :type git :host github :repo "lambda-emacs/lambda-line") 
;;   :custom
;;   ;; (lambda-line-icon-time t) ;; requires ClockFace font (see below)
;;   ;; (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
;;   (lambda-line-position 'top) ;; Set position of status-line 
;;   (lambda-line-abbrev t) ;; abbreviate major modes
;;   (lambda-line-hspace " ")  ;; add some cushion
;;   (lambda-line-prefix t) ;; use a prefix symbol
;;   (lambda-line-prefix-padding 0) ;; no extra space for prefix 
;;   (lambda-line-status-invert nil)  ;; no invert colors
;;   (lambda-line-gui-ro-symbol  " RO") ;; symbols
;;   (lambda-line-gui-mod-symbol " **") 
;;   (lambda-line-gui-rw-symbol  " RW") 
;;   ;; (lambda-line-space-top +.50)  ;; padding on top and bottom of line
;;   ;; (lambda-line-space-bottom -.50)
;;   (lambda-line-symbol-position -.10) ;; adjust the vertical placement of symbol
;;   (lambda-line-vc-symbol nil)
;;   ;; (lambda-line-hspace 0)
;;   :config
;;   ;; activate lambda-line 
;;   (lambda-line-mode) 
;;   ;; set divider line in footer
;;   ;; (when (eq lambda-line-position 'top)
;;   (setq-default mode-line-format nil)
;;   (setq mode-line-format nil)
;;   ;; )
;; 
;;   (customize-set-variable 'flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
;;   (customize-set-variable 'flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters)))

(minimal-emacs-load-user-init "bindings.el")
(minimal-emacs-load-user-init "evil.el")
;; TODO: Rename to vertico
(minimal-emacs-load-user-init "completion.el")
(minimal-emacs-load-user-init "magit.el")
(minimal-emacs-load-user-init "lsp.el")
;; (minimal-emacs-load-user-init "bindings.el")

;; (use-package svg-tag-mode
;;   :ensure (svg-tag-mode :host github :repo "https://github.com/rougier/svg-tag-mode")
;;   :config
;;   (setq svg-tag-tags
;;     ;; '((":TODO:" . ((lambda (tag) (svg-tag-make " ! " :face 'nano-critical))))))
;;     '(("Head:" . ((lambda (tag) (svg-tag-make " HEAD " :face 'default))))
;;       ("Merge:" . ((lambda (tag) (svg-tag-make " MERGE " :face 'default)))))
;;   (global-svg-tag-mode +1)))

;; (use-package helpful)

;; TODO: steal doom's word wrap

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :ensure t
  :defer t
  :hook (elpaca-after-init . global-ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www" "->"))
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

(use-package org
  :ensure nil
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'olivetti-mode))

(use-package sticky-scroll-mode
  :ensure (sticky-scroll-mode :host "github.com" :repo "jclasley/sticky-scroll-mode")
  :defer t
  :config
  (setq sticky-scroll--max-window-height 5))

;; (use-package tab-bar-echo-area
;;   :ensure t
;;   :defer t
;;   :hook (elpaca-after-init . tab-bar-echo-area-mode))

;; (use-package otpp
;;   :ensure t
;;   ;; :after project
;;   :init
;;   ;; If you like to define some aliases for better user experience
;;   (defalias 'one-tab-per-project-mode 'otpp-mode)
;;   (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
;;   (setq otpp-preserve-non-otpp-tabs nil)
;;   (setq tab-bar-show nil)
;;   ;; Enable `otpp-mode` globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands`
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   (otpp-override-mode 1))

;; (use-package current-window-only
;;   :ensure (current-window-only
;;            :host github
;;            :repo "FrostyX/current-window-only")
;;   :defer nil
;;   ;; :hook (elapca-after-init . current-window-only-mode)
;;   :config
;;   (setq magit-commit-diff-inhibit-same-window t)
;;   (current-window-only-mode))


(add-hook 'prog-mode-hook #'(lambda ()
                              (setq mode-line-format
                                    '("%e" mode-line-front-space
                                      (:propertize
                                       ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                                        mode-line-window-dedicated)
                                       ;; display (min-width (6.0))
                                       )
                                      mode-line-frame-identification mode-line-buffer-identification "   "
                                      mode-line-position anzu--update-mode-line (project-mode-line project-mode-line-format)
                                      mode-line-format-right-align
                                      (vc-mode vc-mode) " " mode-line-misc-info flymake-mode-line-counters " " mode-line-end-spaces))))

;; (setq-default mode-line-format nil)
;; (setq mode-line-format nil)

;; Add the local lisp directory to the load-path
(add-to-list 'load-path (expand-file-name "lisp" minimal-emacs-user-directory))

(use-package nano-vertico
  :ensure nil
  :load-path "lisp/"
  :after vertico)

(minimal-emacs-load-user-init "chatgpt.el")
