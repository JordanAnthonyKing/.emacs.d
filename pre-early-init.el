;;; pre-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; By default, minimal-emacs-package-initialize-and-refresh is set to t, which
;; makes minimal-emacs.d call the built-in package manager. Since Elpaca will
;; replace the package manager, there is no need to call it.
(setq minimal-emacs-package-initialize-and-refresh nil)

;; ;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)

;; (setq-default frame-title-format nil)

;; (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
;; (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(setq default-frame-alist
      '((height . 40) (width  . 75) (left-fringe . 4) (right-fringe . 4)
        ;; (internal-border-width . 4)
        (vertical-scroll-bars . t)
        (bottom-divider-width . 0) ;; (right-divider-width . 4)
        ))

(modify-frame-parameters nil default-frame-alist)
;; (setq-default pop-up-windows nil)

(setq widget-image-enable nil)
(setq-default widget-image-enable nil)
;; (setq mode-line-format nil)
;; (setq-default mode-line-format nil)

;; (setq default-frame-alist
;;       (append (list
;;                '(internal-border-width . 4))))

;; (setq left-fringe-width 4)
;; (setq right-fringe-width 4)
;; (setq-default left-fringe-width 4)
;; (setq-default right-fringe-width 4)

;; (setq window-divider-default-right-width 3)
;; (setq window-divider-default-bottom-width 1)

;; (setq line-spacing 0.1)
;; (setq-default line-spacing 0.1)
                     
;; (setq line-spacing 1)
;; (setq-default line-spacing 1)

