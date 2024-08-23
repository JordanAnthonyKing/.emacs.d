;;; evil.el --- >:L -*- no-byte-compile: t; lexical-binding: t; -*-

(defun my-consult-buffer-or-project-buffer ()
  "Call `consult-project-buffer` if the current buffer belongs to a known project.
Otherwise, call `consult-buffer`."
  (interactive)
  (if (project-current)
      (consult-project-buffer)
    (consult-buffer)))

;; TODO: Consultify various and load after completion
(defvar-keymap my-buffer-prefix-keymap
  :doc "buffers"
  "b" #'my-consult-buffer-or-project-buffer
  "k" #'kill-this-buffer
  "d" #'kill-this-buffer
  "p" #'previous-buffer
  "n" #'next-buffer
  "B" #'consult-buffer
  "r" #'rename-buffer
  "R" #'revert-buffer)

(defvar-keymap my-file-prefix-keymap
  :doc "Files"
  "f" #'find-file
  "d" #'dired-jump
  "r" #'consult-recentf)

(defvar-keymap my-project-prefix-keymap
  :doc "Projects"
  "f" #'project-find-file
  "!" #'project-shell-command
  "&" #'project-async-shell-command
  "c" #'project-compile
  "r" #'project-recompile
  "d" #'project-find-dir
  "D" #'project-dired
  "p" #'project-switch-project
  ;; "p" #'tabspaces-open-or-create-project-and-workspace
  "o" #'find-sibling-file
  ;;"K" #'tabspaces-kill-buffers-close-workspace
  ;;"Q" #'tabspaces-kill-buffers-close-workspace
  )

(defvar-keymap my-window-prefix-keymap
  :doc "Windows"
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "H" #'windmove-swap-states-left
  "J" #'windmove-swap-states-down
  "K" #'windmove-swap-states-up
  "L" #'windmove-swap-states-right
  "d" #'delete-window
  "D" #'delete-other-windows
  "s" #'split-window-vertically
  "v" #'split-window-horizontally)

(defvar-keymap my-tabs-prefix-keymap
  :doc "Workspaces"
  "TAB" #'tab-bar-echo-area-display-tab-names)

(defvar-keymap my-leader-prefix-keymap
  :doc leader
  "b" my-buffer-prefix-keymap
  "f" my-file-prefix-keymap
  "p" my-project-prefix-keymap
  "w" my-window-prefix-keymap
  "SPC" #'execute-extended-command
  ";" #'eval-expression
  "h" help-map
  "." #'vertico-repeat
  "TAB" my-tabs-prefix-keymap)

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
  (evil-set-leader nil (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>") my-leader-prefix-keymap)
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
  ;; :config (vim-tab-bar-mode +1))

(use-package vdiff
  :ensure t
  :defer t
  :commands (vdiff-buffers
             vdiff-buffers3
             vdiff-quit
             vdiff-files
             vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))

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

;;(with-eval-after-load "evil"
;;  (evil-define-operator my-evil-comment-or-uncomment (beg end)
;;    "Toggle comment for the region between BEG and END."
;;    (interactive "<r>")
;;    (comment-or-uncomment-region beg end))
;;  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

