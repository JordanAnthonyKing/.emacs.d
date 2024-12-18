;;; evil.el --- >:L -*- no-byte-compile: t; lexical-binding: t; -*-

(require 'general)

(use-package avy
  :ensure t
  :defer t
  :commands (avy-goto-char-timer))

(use-package anzu
  :ensure t
  :defer t)

(use-package evil-anzu
  :ensure t
  :defer nil
  :config
  (global-anzu-mode +1))

(defun +evil/shift-left ()
  "vnoremap > >gv"
  (interactive)
  (call-interactively #'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t)

;; (use-package undo-fu
;;   :ensure t
;;   :hook (elpaca-after-init . undo-fu-mode)
;;   :config
;;   (setq undo-limit 400000
;;         undo-strong-limit 3000000
;;         undo-outer-limit 48000000))
;; 
;; (use-package undo-fu-session
;;   :ensure t
;;   :hook (undo-fu-mode . global-undo-fu-session-mode)
;;   :config
;;   (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :ensure t
  :commands vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        ;; vundo-compact-display t
        vundo-compact-display nil)
  (define-key vundo-mode-map [remap doom/escape] #'vundo-quit))

(use-package expreg
  :ensure (expreg :host github :repo "casouri/expreg")
  :defer t
  :general
  ;; TODO: Change this to work on v in v 
  (:states '(visual)
           "v" #'expreg-expand
           "V" #'expreg-contract))

(defun +evil/shift-right ()
  "vnoremap < <gv"
  (interactive)
  (call-interactively #'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

(setq evil-want-integration t
      evil-want-keybinding nil)
(use-package evil
  :ensure (:wait t)
  :defer t
  :hook (elpaca-after-init . evil-mode)
  :demand t
  :general
  (:states '(motion normal visual)
           "s"     #'evil-avy-goto-char-timer
           "]a"    #'evil-forward-arg
           "[a"    #'evil-backward-arg
           "]e"    #'flymake-goto-next-error
           "[e"    #'flymake-goto-previous-error
           "]h"    #'outline-next-visible-heading
           "[h"    #'outline-previous-visible-heading
           "C-e"   #'evil-beginning-of-line)
  (:states 'visual
           "<"     #'+evil/shift-left
           ">"     #'+evil/shift-right)
  (:states 'insert
           ;; TODO: These probably don't need to be the evil versions
           "C-h"   #'evil-backward-char
           "C-l"   #'evil-forward-char
           ;; Conflict with corfu
           ;; "C-j"   #'evil-next-line
           ;; "C-k"   #'evil-previous-char
           )
  (:keymaps 'evil-window-map
            "d" #'evil-window-delete)
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-undo-system 'undo-fu
        evil-ex-search-vim-style-regexp t
        evil-mode-line-format 'nil
        evil-search-module 'evil-search ;; TODO: investigate isearch without regex
        evil-ex-visual-char-range t
        evil-symbol-word-search t
        evil-visual-state-cursor 'hollow
        evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macros-suppress-motion-error t
        evil-disable-insert-state-bindings t)
  :config
  ;; (setq evil-undo-system 'undo-fu)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-visual-update-x-selection-p nil)

  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (setq-local evil-shift-width tab-width)))

  (defun +evil-disable-ex-highlights-h ()
    "Disable ex search buffer highlights."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)
      t))

  (add-hook 'doom-escape-hook #'+evil-disable-ex-highlights-h)

  (with-eval-after-load 'eldoc
    ;; Allow eldoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (defun +evil--dont-move-cursor-a (fn &rest args)
    "Prevent the cursor from moving when `evil-indent` is called."
    (save-excursion
      (apply fn args)))

  (advice-add 'evil-indent :around #'+evil--dont-move-cursor-a)

  (defun +evil--make-numbered-markers-global-a (char)
    "Make numbered markers 2-9 global in Evil."
    (and (>= char ?2) (<= char ?9)))

  (advice-add 'evil-global-marker-p :after-until #'+evil--make-numbered-markers-global-a)

  (defun +evil-escape-a (&rest _)
    "Call `doom/escape' if `evil-force-normal-state' is called interactively."
    (when (called-interactively-p 'any)
      (call-interactively #'doom/escape)))

  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a)

  (setq features (delq 'evil-ex features)))

;; TODO: Add support for ts and angular
;; (use-package evil-ts-obj
;;   :ensure (evil-ts-obj :host github :repo "dvzubarev/evil-ts-obj")
;;   :defer t
;;   :hook
;;   ((angular-ts-mode typescript-ts-mode) . evil-ts-obj-mode))

(use-package evil-collection
  :ensure t
  :defer t
  :hook (evil-mode . evil-collection-init)
  :config
  (setq evil-collection-want-unimpaired-p nil))

(use-package evil-lion
  :ensure t
  :defer t
  :general
  (:states '(normal visual)
           "gl"   #'evil-lion-left
           "gL"   #'evil-lion-right))

(use-package evil-nerd-commenter
  :ensure t
  :defer t
  :general
  (:states '(normal visual)
           "gc"    #'evilnc-comment-operator)
  ([remap comment-line] #'evilnc-comment-or-uncomment-lines)
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))


(use-package evil-matchit
  :ensure t
  :defer t
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-numbers
  :ensure t
  :defer t
  :general
  (:states '(normal visual)
           "g+" #'evil-numbers/inc-at-pt
           "g-" #'evil-numbers/dec-at-pt)
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-surround
  :ensure t
  :defer t
  :hook (evil-mode . global-evil-surround-mode)
  :general
  (:states '(visual operator)
           "s"    #'evil-surround-region)
  (:states 'operator
           "s"    #'evil-surround-edit)
  :config (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure t
  :defer t
  :general
  (:states '(visual)
           "*" #'evil-visualstar/begin-search-forward
           "#" #'evil-visualstar/begin-search-backward)
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward))
