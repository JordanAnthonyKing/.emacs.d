;;; evil.el --- >:L -*- no-byte-compile: t; lexical-binding: t; -*-

(require 'general)

(general-create-definer my-leader-def
  :states '(normal visual motion)  ;; Apply to these evil states
  :keymaps 'override                ;; Ensure it overrides other keymaps
  :prefix "SPC")                    ;; Set "SPC" as the prefix key

(my-leader-def
  "b" '(:ignore t :which-key "buffers")
  "b b" #'consult-buffer
  "b k" #'kill-buffer
  "b d" #'kill-buffer
  "b p" #'previous-buffer
  "b n" #'next-buffer
  "b B" #'consult-buffer
  "b r" #'rename-buffer
  "b R" #'revert-buffer

  "f" '(:ignore t :which-key "files")
  "f f" #'find-file
  "f d" #'dired-jump
  "f r" #'consult-recentf

  "p" '(:ignore t :which-key "projects")
  "p f" #'project-find-file
  "p !" #'project-shell-command
  "p &" #'project-async-shell-command
  "p c" #'project-compile
  "p r" #'project-recompile
  "p d" #'project-find-dir
  "p D" #'project-dired
  "p p" #'project-switch-project
  ;; "p p" #'tabspaces-open-or-create-project-and-workspace
  "p o" #'find-sibling-file
  ;; "p K" #'tabspaces-kill-buffers-close-workspace
  ;; "p Q" #'tabspaces-kill-buffers-close-workspace

  "w" '(:ignore t :which-key "windows")
  "w h" #'windmove-left
  "w j" #'windmove-down
  "w k" #'windmove-up
  "w l" #'windmove-right
  "w H" #'windmove-swap-states-left
  "w J" #'windmove-swap-states-down
  "w K" #'windmove-swap-states-up
  "w L" #'windmove-swap-states-right
  "w d" #'delete-window
  "w D" #'delete-other-windows
  "w s" #'split-window-vertically
  "w v" #'split-window-horizontally

  "TAB" '(:ignore t :which-key "workspaces")
  "TAB TAB" #'tab-bar-echo-area-display-tab-names

  "SPC" #'execute-extended-command
  ";" #'eval-expression
  "h" help-map
  "." #'vertico-repeat)


(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'doom-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'doom/escape)

;; evil-want-keybinding must be declared before Evil and Evil Collection
(setq evil-want-keybinding nil)
(use-package evil
  :ensure (:wait t)
  :demand t
  :general
  :defines my-evil-join
  :general
  ;; (:states 'visual
  ;; "@"     #'+evil:apply-macro)
  (:states 'motion
           [C-i]   #'evil-jump-forward
           "]a"    #'evil-forward-arg
           "[a"    #'evil-backward-arg
           "]e"    #'next-error
           "[e"    #'previous-error
           "]h"    #'outline-next-visible-heading
           "[h"    #'outline-previous-visible-heading)
  (:states '(normal visual)
           "gc"    #'evilnc-comment-operator
           "gx"    #'evil-exchange
           "gd"    #'+lookup/definition
           "gD"    #'+lookup/references
           "gf"    #'+lookup/file
           "gI"    #'+lookup/implementations)
  (:states 'visual
           "gR"    #'+eval:replace-region
           "<"     #'+evil/shift-left
           ">"     #'+evil/shift-right)
  (:states 'normal
           "gR"    #'+eval/buffer)
  :preface
  (setq evil-search-module 'evil-search ;; TODO: investigate isearch without regex
        evil-ex-visual-char-range t
        evil-symbol-word-search t
        ;; evil-default-cursor '+evil-default-cursor-fn
        ;; evil-normal-state-cursor 'box
        ;; evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        ;; evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow

        evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macros-suppress-motion-error t
        evil-undo-system 'undo-redo)

  (defun +evil--persist-state-a (fn &rest args)
    "When changing major modes, Evil's state is lost. This advice preserves it."
    (if evil-state
        (evil-save-state (apply fn args))
      (apply fn args)))

  (advice-add 'set-auto-mode :around #'+evil--persist-state-a)

  (defun set-evil-ex-hl-update-delay ()
    "Set `evil-ex-hl-update-delay` to 0.25 in specific modes."
    (setq-local evil-ex-hl-update-delay 0.25))

  (add-hook 'magit-mode-hook #'set-evil-ex-hl-update-delay)
  (add-hook 'so-long-minor-mode-hook #'set-evil-ex-hl-update-delay)

  :config
  (setq evil-visual-update-x-selection-p nil)
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (setq-local evil-shift-width tab-width)))

  (with-eval-after-load 'wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  (defun +evil-disable-ex-highlights-h ()
    "Disable ex search buffer highlights."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)
      t))

  (add-hook 'doom-escape-hook #'+evil-disable-ex-hightlights-h)

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

  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a)

  (setq features (delq 'evil-ex features))
  (evil-mode 1))

;;;###autoload
(defun +evil-escape-a (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

;;;###autoload (autoload '+evil-delete "editor/evil/autoload/evil" nil t)
(evil-define-operator +evil-delete (beg end type register yank-handler)
  "A wrapper around `evil-delete' for `wgrep' buffers that will invoke
`wgrep-mark-deletion' on lines you try to delete."
  (interactive "<R><x><y>")
  (condition-case _ex
      (evil-delete beg end type register yank-handler)
    ('text-read-only
     (evil-apply-on-block
      (lambda (beg _)
        (goto-char beg)
        (call-interactively #'wgrep-mark-deletion))
      beg (1- end) nil))))

;;;###autoload
(defun +evil/shift-right ()
  "vnoremap < <gv"
  (interactive)
  (call-interactively #'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/shift-left ()
  "vnoremap > >gv"
  (interactive)
  (call-interactively #'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-numbers
  :ensure t)

(use-package evil-easymotion
  :ensure t
  :after evil
  :commands evilem-create evilem-default-keybindings)

(use-package evil-exchange
  :ensure t
  :commands evil-exchange
  :config
  (defun +evil--escape-exchange-h ()
    "Cancel `evil-exchange` if active when escape is triggered."
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))

  (add-hook 'doom-escape-hook #'+evil--escape-exchange-h))

(use-package evil-lion
  :ensure t
  :general
  (:states '(normal visual)
           "gl"   #'evil-lion-left
           "gL"   #'evil-lion-right))

(use-package evil-nerd-commenter
  :ensure t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

;; TODO: Investigate bindings fo rthis
(use-package evil-surround
  :ensure t
  :general
  (:states '(visual operator)
           "S"    #'evil-surround-region
           :states 'operator
           "s"    #'evil-surround-edit
           "S"    #'evil-Surround-edit)
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package evil-textobj-anyblock
  :ensure t
  :defer t
  :config
  (setq evil-textobj-anyblock-blocks
        '(("(" . ")")
          ("{" . "}")
          ("\\[" . "\\]")
          ("<" . ">"))))

(use-package evil-traces
  :ensure t
  :after evil-ex
  :config
  (require 'cl-lib) ; Ensure cl-lib is loaded

  (cl-pushnew '(+evil:align . evil-traces-global) evil-traces-argument-type-alist :test #'equal)
  (cl-pushnew '(+evil:align-right . evil-traces-global) evil-traces-argument-type-alist :test #'equal)
  (cl-pushnew '(+multiple-cursors:evil-mc . evil-traces-substitute) evil-traces-argument-type-alist :test #'equal)

  (evil-traces-mode))

(use-package evil-visualstar
  :ensure t
  :after evil
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))


(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil
  :config

  (defun add-language-to-mode-alist (mode lang)
    "Add or update the association of MODE with LANG in `evil-textobj-tree-sitter-major-mode-language-alist`."
    (let ((entry (assq mode evil-textobj-tree-sitter-major-mode-language-alist)))
      (if entry
          ;; Update existing entry
          (setcdr entry lang)
        ;; Add new entry
        (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist
                     (cons mode lang) t))))


  (add-language-to-mode-alist 'typescript-ts-mode "typescript")
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner")))


