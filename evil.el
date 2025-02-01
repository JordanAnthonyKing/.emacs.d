'; evil.el --- >:L -*- no-byte-compile: t; lexical-binding: t; -*-

(require 'general)

(use-package avy
  :ensure t
  :defer t
  :commands (avy-goto-char-timer))

(use-package anzu
  :ensure t
  :init (setq anzu-cons-mode-line-p nil)
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

;; (define-minor-mode undo-fu-mode
;;     "Enables `undo-fu' for the current session."
;;     :keymap (let ((map (make-sparse-keymap)))
;;               (define-key map [remap undo] #'undo-fu-only-undo)
;;               (define-key map [remap redo] #'undo-fu-only-redo)
;;               (define-key map (kbd "C-_")     #'undo-fu-only-undo)
;;               (define-key map (kbd "M-_")     #'undo-fu-only-redo)
;;               (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
;;               (define-key map (kbd "C-x r u") #'undo-fu-session-save)
;;               (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
;;               map)
;;     :init-value nil
;;     :global t)

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
           ;; "]a"    #'evil-forward-arg
           ;; "[a"    #'evil-backward-arg
           "]e"    #'flymake-goto-next-error ;; Being overridden by unimpaired?
           "[e"    #'flymake-goto-previous-error
           "]h"    #'outline-next-visible-heading
           "[h"    #'outline-previous-visible-heading
           "C-e"   #'evil-beginning-of-line)
  (:states 'visual
           "<"     #'+evil/shift-left
           ">"     #'+evil/shift-right)
  (:states 'insert
           "C-h"   #'evil-backward-char
           "C-l"   #'evil-forward-char
           ;; Conflict with corfu
           ;; "C-j"   #'evil-next-line
           ;; "C-k"   #'evil-previous-char
           )
  (:keymaps 'evil-window-map
            "d" #'evil-window-delete)
  :custom
  ;; TODO: Investigate this
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
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a))

(use-package evil-collection
  :ensure t
  :defer t
  :hook (evil-mode . evil-collection-init)
  :config
  (setq evil-collection-want-unimpaired-p t))

(use-package evil-swap-keys
  :ensure t
  :hook (evil-mode . global-evil-swap-keys-mode)
  :config
    (add-hook 'prog-mode-hook #'evil-swap-keys-swap-number-row))

(use-package god-mode
  :ensure t (god-mode :host "github.com" :repo "DogLooksGood/god-mode")
  :config
  (add-to-list 'god-mode-translate-alist '("C-x C-p" "C-x p"))
  (add-to-list 'god-mode-translate-alist '("C-p" "C-x p"))
  (add-to-list 'god-mode-translate-alist '("C-b" "C-x b"))
  (add-to-list 'god-mode-translate-alist '("C-x C-b" "C-x b"))
  (add-to-list 'god-mode-translate-alist '("C-SPC" "M-x"))
  (add-to-list 'god-mode-translate-alist '("C-x C-t" "C-x t"))
  (add-to-list 'god-mode-translate-alist '("C-t" "C-x t"))
  (setq god-mode-can-omit-literal-key 't))

(use-package evil-god-state
  :ensure (evil-god-state :host "github.com" :repo "gridaphobe/evil-god-state")
  :config
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
  (evil-define-key '(normal visual) global-map " " 'evil-execute-in-god-state))

;; TODO: Meow-like bindings for this
;; (use-package evil-easymotion
;;   :ensure t
;;   :defer t
;;   :after (evil-collection)
;;   :config
;;   ;; Use evil-search backend, instead of isearch
;;   (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
;;                       :bind ((evil-ex-search-highlight-all nil)))
;;   (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
;;                       :bind ((evil-ex-search-highlight-all nil)))
;;   (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
;;                       :bind ((evil-ex-search-highlight-all nil)))
;;   (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
;;                       :bind ((evil-ex-search-highlight-all nil)))
;; 
;;   ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
;;   ;; buffer, rather than just the current line.
;;   (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
;;   (evilem-make-motion easy-dick #'evil-forward-word-begin :scope 'visible :pre-hook #'evil-forward-word-begin)
;;   (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible :pre-hook 'evil-forward-WORD-begin)
;;   (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible :pre-hook 'evil-forward-word-end)
;;   (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible :pre-hook 'evil-forward-WORD-end)
;;   (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible :pre-hook 'evil-backward-word-begin)
;;   (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible :pre-hook 'evil-backward-WORD-begin)
;;   (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible :pre-hook 'evil-backward-word-end)
;;   (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible :pre-hook 'evil-backward-WORD-end)
;; 
;;   )

;; TODO: Come back to these
(use-package evil-textobj-tree-sitter
  :ensure t
  :defer t
  :after (evil-collection)
  :config
  ;; (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "assignment.rhs"))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "attribute.outer"))
  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "attribute.inner"))
  (define-key evil-outer-text-objects-map "b" (evil-textobj-tree-sitter-get-textobj "block.outer"))
  (define-key evil-inner-text-objects-map "b" (evil-textobj-tree-sitter-get-textobj "block.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "call.inner"))
  (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "k" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  ;; This might not exist:
  (define-key evil-inner-text-objects-map "k" (evil-textobj-tree-sitter-get-textobj "comment.inner"))
  (define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (define-key evil-outer-text-objects-map "e" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "e" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (define-key evil-outer-text-objects-map "s" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "s" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "F" (evil-textobj-tree-sitter-get-textobj "frame.outer"))
  (define-key evil-inner-text-objects-map "F" (evil-textobj-tree-sitter-get-textobj "frame.inner"))
  (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "w" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "w" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "d" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "d" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-inner-text-objects-map "n" (evil-textobj-tree-sitter-get-textobj "number.inner"))
  (define-key evil-outer-text-objects-map "p" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  (define-key evil-inner-text-objects-map "p" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (define-key evil-outer-text-objects-map "R" (evil-textobj-tree-sitter-get-textobj "regex.outer"))
  (define-key evil-inner-text-objects-map "R" (evil-textobj-tree-sitter-get-textobj "regex.inner"))
  (define-key evil-outer-text-objects-map "r" (evil-textobj-tree-sitter-get-textobj "return.outer"))
  (define-key evil-inner-text-objects-map "r" (evil-textobj-tree-sitter-get-textobj "return.inner"))
  ;; S would conflict with surround
  ;; (define-key evil-outer-text-objects-map "S" (evil-textobj-tree-sitter-get-textobj "scopename.inner"))
  ;; (define-key evil-inner-text-objects-map "S" (evil-textobj-tree-sitter-get-textobj "statement.outer"))

  (defun next-attribute-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "attribute.outer"))
  (defun prev-attribute-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "attribute.outer" t))
  (defun next-attribute-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "attribute.outer" nil t))
  (defun prev-attribute-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "attribute.outer" t t))

  (define-key evil-normal-state-map (kbd "]a") 'next-attribute-start)
  (define-key evil-normal-state-map (kbd "[a") 'prev-attribute-start)
  (define-key evil-normal-state-map (kbd "]A") 'next-attribute-end)
  (define-key evil-normal-state-map (kbd "[A") 'prev-attribute-end)

  (defun next-block-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "block.outer"))
  (defun prev-block-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "block.outer" t))
  (defun next-block-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "block.outer" nil t))
  (defun prev-block-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "block.outer" t t))

  (define-key evil-normal-state-map (kbd "]b") 'next-block-start)
  (define-key evil-normal-state-map (kbd "[b") 'prev-block-start)
  (define-key evil-normal-state-map (kbd "]B") 'next-block-end)
  (define-key evil-normal-state-map (kbd "[B") 'prev-block-end)

  (defun next-call-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "call.outer"))
  (defun prev-call-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "call.outer" t))
  (defun next-call-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "call.outer" nil t))
  (defun prev-call-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "call.outer" t t))

  (define-key evil-normal-state-map (kbd "]c") 'next-call-start)
  (define-key evil-normal-state-map (kbd "[c") 'prev-call-start)
  (define-key evil-normal-state-map (kbd "]C") 'next-call-end)
  (define-key evil-normal-state-map (kbd "[C") 'prev-call-end)

  (defun next-comment-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "comment.outer"))
  (defun prev-comment-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "comment.outer" t))
  (defun next-comment-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t))
  (defun prev-comment-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "comment.outer" t t))

  (define-key evil-normal-state-map (kbd "]k") 'next-comment-start)
  (define-key evil-normal-state-map (kbd "[k") 'prev-comment-start)
  (define-key evil-normal-state-map (kbd "]K") 'next-comment-end)
  (define-key evil-normal-state-map (kbd "[K") 'prev-comment-end)

  (defun next-conditional-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "conditional.outer"))
  (defun prev-conditional-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t))
  (defun next-conditional-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "conditional.outer" nil t))
  (defun prev-conditional-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t t))

  (define-key evil-normal-state-map (kbd "]i") 'next-conditional-start)
  (define-key evil-normal-state-map (kbd "[i") 'prev-conditional-start)
  (define-key evil-normal-state-map (kbd "]I") 'next-conditional-end)
  (define-key evil-normal-state-map (kbd "[I") 'prev-conditional-end)
  (define-key evil-normal-state-map (kbd "]e") 'next-conditional-start)
  (define-key evil-normal-state-map (kbd "[e") 'prev-conditional-start)
  (define-key evil-normal-state-map (kbd "]E") 'next-conditional-end)
  (define-key evil-normal-state-map (kbd "[E") 'prev-conditional-end)
  (define-key evil-normal-state-map (kbd "]s") 'next-conditional-start)
  (define-key evil-normal-state-map (kbd "[s") 'prev-conditional-start)
  (define-key evil-normal-state-map (kbd "]S") 'next-conditional-end)
  (define-key evil-normal-state-map (kbd "[S") 'prev-conditional-end)

  (defun next-loop-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "loop.outer"))
  (defun prev-loop-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "loop.outer" t))
  (defun next-loop-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "loop.outer" nil t))
  (defun prev-loop-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "loop.outer" t t))

  (define-key evil-normal-state-map (kbd "]l") 'next-loop-start)
  (define-key evil-normal-state-map (kbd "[l") 'prev-loop-start)
  (define-key evil-normal-state-map (kbd "]L") 'next-loop-end)
  (define-key evil-normal-state-map (kbd "[L") 'prev-loop-end)
  (define-key evil-normal-state-map (kbd "]w") 'next-loop-start)
  (define-key evil-normal-state-map (kbd "[w") 'prev-loop-start)
  (define-key evil-normal-state-map (kbd "]W") 'next-loop-end)
  (define-key evil-normal-state-map (kbd "[W") 'prev-loop-end)
  (define-key evil-normal-state-map (kbd "]d") 'next-loop-start)
  (define-key evil-normal-state-map (kbd "[d") 'prev-loop-start)
  (define-key evil-normal-state-map (kbd "]D") 'next-loop-end)
  (define-key evil-normal-state-map (kbd "[D") 'prev-loop-end)

  (defun next-parameter-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "parameter.outer"))
  (defun prev-parameter-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "parameter.outer" t))
  (defun next-parameter-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "parameter.outer" nil t))
  (defun prev-parameter-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "parameter.outer" t t))

  (define-key evil-normal-state-map (kbd "]p") 'next-parameter-start)
  (define-key evil-normal-state-map (kbd "[p") 'prev-parameter-start)
  (define-key evil-normal-state-map (kbd "]P") 'next-parameter-end)
  (define-key evil-normal-state-map (kbd "[P") 'prev-parameter-end)

  (defun next-return-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "return.outer"))
  (defun prev-return-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "return.outer" t))
  (defun next-return-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "return.outer" nil t))
  (defun prev-return-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "return.outer" t t))

  (define-key evil-normal-state-map (kbd "]r") 'next-return-start)
  (define-key evil-normal-state-map (kbd "[r") 'prev-return-start)
  (define-key evil-normal-state-map (kbd "]R") 'next-return-end)
  (define-key evil-normal-state-map (kbd "[R") 'prev-return-end)

  (defun next-function-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "function.outer"))
  (defun prev-function-start () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "function.outer" t))
  (defun next-function-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t))
  (defun prev-function-end () (interactive)
         (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))

  (define-key evil-normal-state-map (kbd "]f") 'next-function-start)
  (define-key evil-normal-state-map (kbd "[f") 'prev-function-start)
  (define-key evil-normal-state-map (kbd "]F") 'next-function-end)
  (define-key evil-normal-state-map (kbd "[F") 'prev-function-end))


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
  :hook (evil-mode . global-evil-matchit-mode)
  :config
  (evilmi-load-plugin-rules '(angular-ts-mode) '(template simple html)))

(use-package evil-numbers
  :ensure t
  :defer t
  :general
  (:states '(normal visual)
           "g+" #'evil-numbers/inc-at-pt
           "g-" #'evil-numbers/dec-at-pt)
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

;; TODO: Change to S
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

(use-package evil-textobj-anyblock
  :ensure t
  :defer t
  :config
  (setq evil-textobj-anyblock-blocks
        '(("(" . ")")
          ("{" . "}")
          ("\\[" . "\\]")
          ("<" . ">"))))

;; TODO: This doesn't behave as expected, probably best on ctrl
(use-package transform-symbol-at-point
  :ensure (transform-symbol-at-point :host "github.com" :repo "waymondo/transform-symbol-at-point")
  :defer t
  :general
  (:states '(visual)
           "~" #'transform-symbol-at-point))

