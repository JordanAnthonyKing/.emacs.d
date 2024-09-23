;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-hook 'elpaca-after-init-hook #'global-auto-revert-mode)
;; TODO: Defer this to consult-recentf like Doom
(add-hook 'elpaca-after-init-hook #'recentf-mode)
(add-hook 'elpaca-after-init-hook #'savehist-mode)
(add-hook 'elpaca-after-init-hook #'save-place-mode)

;; TODO: Make this work wtih minimal emacs paths
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/var/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/var/backups/")))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/var/autosaves/" t)

(setq message-truncate-lines t)

(setq scroll-margin 10)
;; mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
;; mouse-wheel-progressive-speed nil)

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

(set-face-attribute 'default nil :font "Berkeley Mono-10")


(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one-light t)
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package mood-line
  :config (mood-line-mode))

;; (use-package persistent-scratch
;; :ensure t
;; :config (persistent-scratch-setup-default))

(minimal-emacs-load-user-init "bindings.el")
(minimal-emacs-load-user-init "evil.el")
;; TODO: Rename to vertico
(minimal-emacs-load-user-init "completion.el")
(minimal-emacs-load-user-init "magit.el")
(minimal-emacs-load-user-init "lsp.el")
;;(minimal-emacs-load-user-init "bindings.el")

;; (use-package mindre-theme
;; :ensure t
;; :custom
;; (mindre-use-more-bold nil)
;; (mindre-use-faded-lisp-parens t)
;; :config
;; (load-theme 'mindre t))

;; (use-package svg-tag-mode
;; :ensure (svg-tag-mode :host github :repo "https://github.com/rougier/svg-tag-mode")
;; :config
;; ;;(setq svg-tag-tags
;; ;;  '((":TODO:" . ((lambda (tag) (svg-tag-make " ! " :face 'nano-critical))))))
;; (global-svg-tag-mode +1))

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
  :defer t
  :hook (elpaca-after-init . tab-bar-echo-area-mode))

(use-package otpp
  :ensure (otpp :host github :repo "abougouffa/one-tab-per-project")
  :hook (elpaca-after-init . one-tab-per-project-mode)
  :init
  ;; If you like to define some aliases for better user experience
  (defalias 'one-tab-per-project-mode 'otpp-mode)
  (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
  ;; Enable `otpp-mode` globally
  (setq tab-bar-show nil)
  ;; (otpp-mode 1)
  ;; If you want to advice the commands in `otpp-override-commands`
  ;; to be run in the current's tab (so, current project's) root directory
  ;; (otpp-override-mode 1)
  )

;; (use-package helpful)

;; (use-package visual-fill-column
;; :ensure (visual-fill-column :host codeberg
;; :repo "joostkremers/visual-fill-column"))
;; (defun mini-frame-dynamic-parameters ()
;; (let* ((edges       (window-pixel-edges))      ;; (left top right bottom)
;; (body-edges  (window-body-pixel-edges)) ;; (left top right bottom)
;; (left   (nth 0 edges))      ;; Take margins into account
;; (top    (nth 1 edges)) ;; Drop header line
;; (right  (nth 2 edges))      ;; Take margins into account
;; (bottom (nth 3 body-edges)) ;; Drop header line
;; ;; (left   (if (eq left-fringe-width 0)
;; ;; left
;; ;; (- left (frame-parameter nil 'left-fringe))))
;; ;; (right  (nth 2 edges))
;; ;; (right  (if (eq right-fringe-width 0)
;; ;; right
;; ;; (+ right (frame-parameter nil 'right-fringe))))
;; (fringe-left 0)
;; (fringe-right 0)
;; (border 1)
;; ;; (width (- (frame-pixel-width) (* 2 (+ fringe border))))
;; (width (- right left fringe-left fringe-right (* 1 border)))
;; (y (- top border)))
;; `((left . ,(- left border))
;; (top . ,y)
;; (alpha . 1.0)
;; (width . (text-pixels . ,width))
;; (left-fringe . ,fringe-left)
;; (right-fringe . ,fringe-right)
;; (child-frame-border-width . ,border)
;; (internal-border-width . ,border)
;; (foreground-color . ,(face-foreground 'nano-default))
;; (background-color . ,(face-background 'highlight)))))
;; 
;; (use-package mini-frame
;; :ensure t
;; :after (vertico)
;; :config
;; (set-face-background 'child-frame-border (face-foreground 'nano-faded))
;; (setq mini-frame-default-height vertico-count)
;; (setq mini-frame-create-lazy t)
;; (setq mini-frame-show-parameters 'mini-frame-dynamic-parameters)
;; (setq mini-frame-ignore-commands
;; '("edebug-eval-expression" debugger-eval-expression))
;; (setq mini-frame-internal-border-color (face-foreground 'nano-subtle-i))
;; ;; (setq mini-frame-resize 'grow-only) ;; -> buggy as of 01/05/2021
;; ;; (setq mini-frame-resize 'not-set)
;; ;; (setq mini-frame-resize nil)
;; (setq mini-frame-resize t)
;; (setq mini-frame-resize-min-height 3))

;; TODO: steal doom's word wrap

;; This assumes you've installed the package via MELPA.
;; (use-package ligature
;;   :ensure t
;;   :defer t
;;   :hook (elpaca-after-init . global-ligature-mode)
;;   :config
;;   ;; Enable the "www" ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))
;;   ;; Enable traditional ligature support in eww-mode, if the
;;   ;; `variable-pitch' face supports it
;;   (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
;;   ;; Enable all Cascadia Code ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                                        "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                                        "\\\\" "://"))
;;   ;; Enables ligature checks globally in all buffers. You can also do it
;;   ;; per mode with `ligature-mode'.
;;   ;; (global-ligature-mode t)
;;   )

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
  (add-hook 'eat-mode-hook (lambda () (setq-local scroll-conservatively 10000)))
  (add-hook 'eat-mode-hook 'compilation-minor-mode))

;; (use-package current-window-only
;;   :ensure (current-window-only
;;            :host github
;;            :repo "FrostyX/current-window-only")
;;   :config
;;   (setq magit-commit-diff-inhibit-same-window t)
;;   (current-window-only-mode))

(use-package popwin
  :ensure t
  :hook (elpaca-after-init . popwin-mode))
