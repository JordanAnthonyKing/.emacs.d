;;; bindings.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; TODO: Consultify various and load after completion
(defvar-keymap my-buffer-prefix-keymap
  :doc "buffers"
  "b" #'consult-buffer
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
  "o" #'find-sibling-file
  "K" #'project-kill-buffers
  "Q" #'project-kill-buffers)

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

;; TODO: Needs to not move beyond the newline char
(defun my-meow-append (arg)
  "Move cursor right by one character if no active selection, then call `meow-append`.
If there is an active selection, just call `meow-append`."
  (interactive "p")
  (if (use-region-p)
      (meow-append)
    (progn
      (forward-char 1)
      (meow-append))))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("SPC" . execute-extended-command)
   '(";" . eval-expression)
   '("h" . help-map)
   '("j" . "H-j")
   '("k" . "H-k")
   '("." . vertico-repeat) ; TODO Resume last search
   (cons "b" my-buffer-prefix-keymap)
   (cons "f" my-file-prefix-keymap)
   (cons "p" my-project-prefix-keymap)
   (cons "w" my-window-prefix-keymap)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   ;; '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '(": w RET" . save-buffer)
   '(": q RET" . kill-buffer)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . my-meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-kill) ; Swap with backspace
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-till) ; I'd rather be before the char not after
   ;; Need an F --- actually swap with t
   ; '("g" . meow-cancel-selection)
   '("g g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . (lambda (arg)
             (interactive "p")
             (meow-join (- arg)))) ;; This probably won't work for all cases because of how negative arg works
   '("M" . meow-join)
   '("n" . meow-search)
   '("N" . (lambda (arg)
             (interactive "p")
             (meow-search (- 1))))
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill) ; Have S go backwards, but I think I'd rather have all this on d and bind s to snipe
   '("t" . meow-till) ; swap with f
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-line)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol) ; swap these
   '("x" . meow-delete) ; Change to delete? No shift v so set this there
   '("X" . meow-goto-line)
   '("y" . meow-save) ; make a bit more vimmy
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("~" . upcase-region)
   '("/" . meow-visit)
   '("<escape>" . meow-cancel-selection))) ; Cancel selection?


(use-package meow
  :config
  (setq meow-use-clipboard t)
  (meow-setup)
  (meow-global-mode +1))

(use-package avy)



