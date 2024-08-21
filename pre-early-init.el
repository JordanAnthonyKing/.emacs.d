;;; pre-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; (setq minimal-emacs-user-directory user-emacs-directory)
;; (setq minimal-emacs-var-dir
;;       (expand-file-name "var/" minimal-emacs-user-directory))
;; (setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
;; (setq user-emacs-directory minimal-emacs-var-dir)

;;; pre-early-init.el --- Pre early init -*- no-byte-compile: t; lexical-binding: t; -*-
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)
