;;; bindings.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(setq w32-lwindow-modifier 'super
      w32-rwindow-modifier 'super)

;; HACK: Emacs can't distinguish C-i from TAB, or C-m from RET, in either GUI or
;;   TTY frames.  This is a byproduct of its history with the terminal, which
;;   can't distinguish them either, however, Emacs has separate input events for
;;   many contentious keys like TAB and RET (like [tab] and [return], aka
;;   "<tab>" and "<return>"), which are only triggered in GUI frames, so here, I
;;   create one for C-i. Won't work in TTY frames, though. Doom's :os tty module
;;   has a workaround for that though.
(pcase-dolist (`(,key ,fallback . ,events)
               '(([C-i] [?\C-i] tab kp-tab)
                 ([C-m] [?\C-m] return kp-return)))
  (define-key
   input-decode-map fallback
   (lambda (&rest _)
     (interactive)
     (let ((keys (this-single-command-raw-keys)))
       (if (and (display-graphic-p)
                (not (cl-loop for event in events
                              if (cl-position event keys)
                              return t))
                ;; Use FALLBACK if nothing is bound to KEY, otherwise we've
                ;; broken all pre-existing FALLBACK keybinds.
                (key-binding
                 (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                          key) nil t))
           (setq unread-command-events (listify-key-sequence (vector key)))
         (setq unread-command-events (listify-key-sequence (vector fallback))))))))

;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like diff-hl and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

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

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))

(use-package which-key
  :ensure nil
  :hook (elpaca-after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-setup-side-window-bottom))

(require 'general)

(general-def (evil-ex-completion-map evil-ex-search-keymap)
  "C-a" #'evil-beginning-of-line
  "C-b" #'evil-backward-char
  "C-f" #'evil-forward-char
  "C-h" #'evil-backward-char
  "C-l" #'evil-forward-char)

(general-define-key
 :states '(global insert)
 :keymaps '(evil-ex-completion-map evil-ex-search-keymap)
 "C-j" #'next-complete-history-element
 "C-k" #'previous-complete-history-element)

(general-def minibuffer-mode-map
  [escape] #'abort-recursive-edit
  "C-a"    #'move-beginning-of-line
  "C-r"    #'evil-paste-from-register
  "C-u"    #'evil-delete-back-to-indentation
  "C-v"    #'yank
  "C-w"    #'evil-delete-backward-word
  "C-z"    #'undo
  "C-j"    #'next-line
  "C-k"    #'previous-line
  "C-h"    #'backward-char
  "C-l"    #'forward-char
  "C-S-j"  #'scroll-up-command
  "C-S-k"  #'scroll-down-command)

(general-define-key
 :states 'insert
 :keymaps 'minibuffer-mode-map
 "C-j" #'next-line
 "C-k" #'previous-line
 "C-l" #'forward-char
 "C-h" #'backward-char)

(general-def read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

(general-def 'normal
  "C-="    #'text-scale-increase
  "C--"    #'text-scale-decrease)

(general-create-definer my-leader-def
  :states '(normal visual motion)   ;; Apply to these evil states
  :keymaps 'override                ;; Ensure it overrides other keymaps
  :prefix "SPC")                    ;; Set "SPC" as the prefix key

(my-leader-def
  "SPC" #'execute-extended-command
  ";" #'pp-eval-expression
  "h" help-map
  "." #'vertico-repeat
  "w" 'evil-window-map
  "u" #'universal-argument

  "n p m" #'npm

  "a i" '(:ignore t :which-key "AI")
  "a i a" #'gptel-add
  "a i A" #'gptel-add-file
  "a i m" #'gptel-menu
  "a i c" #'gptel

  "b" '(:ignore t :which-key "buffers")
  "b b" #'consult-buffer
  "b B" #'consult-buffer-other-window
  "b k" #'kill-current-buffer
  "b d" #'kill-current-buffer
  "b p" #'previous-buffer
  "b n" #'next-buffer
  "b [" #'previous-buffer
  "b ]" #'next-buffer
  "b B" #'consult-buffer
  "b r" #'rename-buffer
  "b R" #'revert-buffer
  "b i" #'ibuffer
  "b c" #'clone-indirect-buffer
  "b C" #'clone-indirect-buffer-other-window
  "b l" #'evil-switch-to-windows-last-buffer
  "b m" #'bookmark-set
  "b M" #'bookmark-delete
  "b N" #'evil-buffer-new
  "b R" #'revert-buffer
  "b s" #'save-buffer
  "b S" #'evil-write-all
  "b z" #'bury-buffer
  "b Z" #'unbury-buffer
  "b /" #'consult-line

  "c" '(:ignore t :which-key "code")
  "c a" #'eglot-code-actions
  "c A" #'eglot-code-action-quickfix
  "c c" #'compile
  "c C" #'recompile
  "c e" #'eval-region
  "c E" #'eval-buffer
  "c w" #'delete-trailing-whitespace
  "c I" #'indent-region
  "c j" #'consult-eglot-symbols
  "c d" #'xref-find-definitions
  "c D" #'xref-find-references
  "c r" #'eglot-rename
  "c R" #'eglot-reconnect
  "c i" #'eglot-find-implementation
  "c t" #'eglot-find-typeDefinition
  "c n" #'flymake-goto-next-error
  "c p" #'flymake-goto-prev-error
  "c f" #'eglot-format

  "f" '(:ignore t :which-key "files")
  "f f" #'find-file
  "f F" #'find-file-other-window
  "f d" #'dired-jump
  "f r" #'consult-recentf
  "f c" #'editorconfig-find-current-editorconfig
  "f D" #'delete-file
  "f l" #'consult-locate
  "f r" #'recentf-open-files
  "f R" #'rename-file
  "f s" #'save-buffer
  "f S" #'write-file

  "g" '(:ignore t :which-key "magit")
  "g R" #'vc-revert
  ;; "g t" #'git-timemachine-toggle

  ;; TODO: hunks
  "g /"   #'magit-dispatch
  "g ."   #'magit-file-dispatch
  "g '"   #'forge-dispatch
  "g b"   #'magit-branch-checkout
  "g g"   #'magit-status
  "g G"   #'magit-status-here
  "g D"   #'magit-file-delete
  "g B"   #'magit-blame-addition
  "g C"   #'magit-clone
  "g F"   #'magit-fetch
  "g L"   #'magit-log-buffer-file
  "g S"   #'magit-stage-buffer-file
  "g U"   #'magit-unstage-buffer-file

  "g f" '(:ignore t :which-key "find")
  "g f f"   #'magit-find-file
  "g f g"   #'magit-find-git-config-file
  "g f c"   #'magit-show-commit
  "g f i"   #'forge-visit-issue
  "g f p"   #'forge-visit-pullreq

  "g o" '(:ignore t :which-key "open in browser")
  ;; "g o o"   #'+vc/browse-at-remote
  ;; "g o h"   #'+vc/browse-at-remote-homepage
  "g o r"   #'forge-browse-remote
  "g o c"   #'forge-browse-commit
  "g o i"   #'forge-browse-issue
  "g o p"   #'forge-browse-pullreq
  "g o I"   #'forge-browse-issues
  "g o P"   #'forge-browse-pullreqs

  "g l" '(:ignore t :which-key "list")
  ;; (:when (modulep! :tools gist)
  ;; :desc "List gists"              "g"   #'+gist:list)
  "g l r"   #'magit-list-repositories
  "g l s"   #'magit-list-submodules
  "g l i"   #'forge-list-issues
  "g l p"   #'forge-list-pullreqs
  "g l n"   #'forge-list-notifications

  "g c" '(:ignore t :which-key "create")
  "g c r"   #'magit-init
  "g c R"   #'magit-clone
  "g c c"   #'magit-commit-create
  "g c f"   #'magit-commit-fixup
  "g c b"   #'magit-branch-and-checkout
  "g c i"   #'forge-create-issue
  "g c p"   #'forge-create-pullreq

  "i" '(:ignore t :which-key "insert")
  "i e" #'emoji-search
  "i p" (lambda () (evil-ex "R!echo "))
  "i r" #'evil-show-registers
  ;; TODO: Snippets
  ;; "i s" #'some-snippet-command
  "i u" #'insert-char
  "i y" #'yank-pop

  "o" '(:ignore t :which-key "open")
  "o b" #'browse-url-of-file
  "o f" #'make-frame
  "o F" #'select-frame-by-name
  "o d" #'dirvish
  "o -" #'dired-jump
  "o p" #'dirvish-side
  "o t" #'eat

  "p" '(:ignore t :which-key "projects")
  "p f" #'project-find-file
  "p !" #'project-shell-command
  "p &" #'project-async-shell-command
  "p c" #'project-compile
  "p r" #'project-recompile
  "p d" #'project-find-dir
  "p D" #'project-dired
  "p p" #'project-switch-project
  "p o" #'find-sibling-file
  "p b" #'consult-project-buffer
  "p s" #'consult-ripgrep
  "p t" #'eat-project
  "p T" #'eat-project-other-window

  "q" '(:ignore t :which-key "quit/session")
  ;; "d" #'+default/restart-server
  "q f" #'delete-frame
  ;; "F" #'doom/kill-all-buffers
  "q K" #'save-buffers-kill-emacs
  "q q" #'save-buffers-kill-terminal
  "q Q" #'evil-quit-all-with-error-code

  "s" '(:ignore t :which-key "search")
  "s b" #'consult-line
  "s B" #'consult-line-multi
  "s d" #'consult-ripgrep
  "s f" #'consult-locate
  "s i" #'consult-imenu
  "s I" #'consult-imenu-multi
  "s l" #'ffap-menu
  "s j" #'evil-show-jumps
  "s m" #'bookmark-jump
  "s r" #'evil-show-marks
  "s u" #'vundo
  "s p" #'consult-ripgrep

  "t" '(:ignore t :which-key "toggle")
  "t c" #'global-display-fill-column-indicator-mode
  "t d" #'diff-hl-mode
  "t f" #'flycheck-mode
  "t F" #'toggle-frame-fullscreen
  ;; "t g" #'evil-goggles-mode
  "t i" #'indent-bars-mode
  "t v" #'visible-mode
  "t w" #'visual-line-mode

  ;; "w" '(:ignore t :which-key "windows")
  ;; "w h" #'windmove-left
  ;; "w j" #'windmove-down
  ;; "w k" #'windmove-up
  ;; "w l" #'windmove-right
  ;; "w H" #'windmove-swap-states-left
  ;; "w J" #'windmove-swap-states-down
  ;; "w K" #'windmove-swap-states-up
  ;; "w L" #'windmove-swap-states-right
  ;; "w d" #'delete-window
  ;; "w D" #'delete-other-windows
  ;; "w s" #'split-window-vertically
  ;; "w v" #'split-window-horizontally

  ;; "TAB" '(:ignore t :which-key "workspaces")
  ;; "TAB TAB" #'tab-bar-echo-area-display-tab-names

  )
