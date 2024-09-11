;;; pre-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)

(setq default-directory (getenv "HOME"))

;; (setq default-frame-alist '((internal-border-width . 10)
                            ;; (drag-with-header-line . t)))
;; (scroll-bar-mode 1)
;; (setq window-divider-default-right-width 10)
;; (setq window-divider-default-bottom-width 10)
(setq widget-image-enable nil)
