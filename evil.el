;;; evil.el --- >:L -*- no-byte-compile: t; lexical-binding: t; -*-

(defun my-consult-buffer-or-project-buffer ()
  "Call `consult-project-buffer` if the current buffer belongs to a known project.
Otherwise, call `consult-buffer`."
  (interactive)
  (if (project-current)
      (consult-project-buffer)
    (consult-buffer)))

(use-package general
  :ensure (:wait t)
  :demand t)

(require 'general)

(general-create-definer my-leader-def
  :states '(normal visual motion)  ;; Apply to these evil states
  :keymaps 'override                ;; Ensure it overrides other keymaps
  :prefix "SPC")                    ;; Set "SPC" as the prefix key

(my-leader-def
  "b" '(:ignore t :which-key "buffers")
  "b b" #'my-consult-buffer-or-project-buffer
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

;; evil-want-keybinding must be declared before Evil and Evil Collection
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

;; (use-package vim-tab-bar
  ;; :ensure t
  ;; :commands vim-tab-bar-mode
  ;; :hook (elpaca-after-init-hook . vim-tab-bar-mode))

;; (use-package vdiff
  ;; :ensure t
  ;; :defer t
  ;; :commands (vdiff-buffers
             ;; vdiff-buffers3
             ;; vdiff-quit
             ;; vdiff-files
             ;; vdiff-files3)
  ;; :custom
  ;; (vdiff-auto-refine t)
  ;; (vdiff-only-highlight-refinements t))

(defun add-language-to-mode-alist (mode lang)
  "Add or update the association of MODE with LANG in `evil-textobj-tree-sitter-major-mode-language-alist`."
  (let ((entry (assq mode evil-textobj-tree-sitter-major-mode-language-alist)))
    (if entry
        ;; Update existing entry
        (setcdr entry lang)
      ;; Add new entry
      (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist
                   (cons mode lang) t))))

(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil
  :config
  (add-language-to-mode-alist 'typescript-ts-mode "typescript")
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner")))

(use-package evil-visualstar
  :after evil
  :ensure t
  :defer t
  :commands global-evil-visualstar-mode
  :config (global-evil-visualstar-mode +1))

(use-package evil-surround
  :after evil
  :ensure t
  :defer t
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))
     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))
     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :config (global-evil-surround-mode +1))

;; TODO: Command to comment a line
;; (with-eval-after-load "evil"
;;   (evil-define-operator my-evil-comment-or-uncomment (beg end)
;;     "Toggle comment for the region between BEG and END."
;;     (interactive "<r>")
;;     (comment-or-uncomment-region beg end))
;;   (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))
