;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-low-cons-threshold minimal-emacs-gc-cons-threshold))

(use-package auto-compile
  :demand t
  :custom
  (auto-compile-check-parens nil)
  (auto-compile-display-buffer nil)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(minimal-emacs-load-user-init "completion.el")
(minimal-emacs-load-user-init "bindings.el")
(minimal-emacs-load-user-init "elisp.el")
;; (minimal-emacs-load-user-init "bindings.el")

(setq scroll-margin 10
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-progressive-speed nil)

(defun reload-init-file ()
  "Reload the Emacs configuration file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(setq default-directory (getenv "HOME"))

(set-face-attribute 'default nil :font "Berkeley Mono-10")
(use-package nano-theme
  :vc (:url "https://github.com/rougier/nano-theme")
  :config
  (nano-light)
  (load-theme 'nano t))




