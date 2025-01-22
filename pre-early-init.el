;;; pre-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)

;; (setq-default frame-title-format nil)

(setq widget-image-enable nil)
(setq-default widget-image-enable nil)
;; (setq use-package-compute-statistics t)
;; (setq mode-line-format nil)
;; (setq-default mode-line-format nil)

(setq default-frame-alist
      (append (list
               '(internal-border-width . 4))))

(setq left-fringe-width 4)
(setq right-fringe-width 4)
(setq-default left-fringe-width 4)
(setq-default right-fringe-width 4)

(setq window-divider-default-right-width 4)
(setq window-divider-default-bottom-width 4)

;; (setq line-spacing 0.1)
;; (setq-default line-spacing 0.1)
                     
;; (setq line-spacing 1)
;; (setq-default line-spacing 1)

