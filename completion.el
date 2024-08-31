;;; completion.el --- Configuration for completion system -*- no-byte-compile: t; lexical-binding: t; -*-

;;;###autoload
(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command."
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (if (fboundp 'embark-dwim)
        (save-selected-window
          (embark-dwim))  ;; Removed local definition of embark-quit-after-action
      (user-error "Embark not installed, aborting..."))))

;;;###autoload
(defun +vertico/enter-or-preview ()
  "Enter directory or embark preview on current candidate."
  (interactive)
  (when (> 0 vertico--index)
    (user-error "No vertico session is currently active"))
  (if (and (let ((cand (vertico--candidate)))
             (or (string-suffix-p "/" cand)
                 (and (vertico--remote-p cand)
                      (string-suffix-p ":" cand))))
           (not (equal vertico--base ""))
           (eq 'file (vertico--metadata-get 'category)))
      (vertico-insert)
    (condition-case _
        (+vertico/embark-preview)
      (user-error (vertico-directory-enter)))))

(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :general
  (:keymaps 'vertico-map
            "M-RET"   #'vertico-exit-input
            "C-SPC"   #'+vertico/embark-preview  ;; TODO: Maybe make this a function
            "C-j"     #'vertico-next
            "C-M-j"   #'vertico-next-group
            "C-k"     #'vertico-previous
            "C-M-k"   #'vertico-previous-group
            "C-h"     (lambda ()
                        (interactive)
                        (if (eq 'file (vertico--metadata-get 'category))
                            (vertico-directory-up)))
            "C-l"     (lambda ()
                        (interactive)
                        (if (eq 'file (vertico--metadata-get 'category))
                            (+vertico/enter-or-preview)))
            "DEL"     #'vertico-directory-delete-char)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)

  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defun +vertico--suppress-completion-help-a (fn &rest args)
    "Suppress minibuffer completion help when using `ffap-menu-ask`."
    (cl-letf (((symbol-function 'minibuffer-completion-help) #'ignore))
      (apply fn args)))
  (advice-add 'ffap-menu-ask :around #'+vertico--suppress-completion-help-a)

  (vertico-multiform-mode)
  ;; Define transformation functions
  (defvar +vertico-transform-functions nil)

  ;; Custom method for formatting candidates
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  ;; Highlight directories with a specific face
  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)

  ;; Highlight enabled modes with a specific face
  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
        (if (or (eq sym major-mode)
                (and (memq sym minor-mode-list)
                     (boundp sym)
                     (symbol-value sym)))
            (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
      cmd))

  ;; Add custom transformations to categories and commands
  (add-to-list 'vertico-multiform-categories
               '(file
                 (+vertico-transform-functions . +vertico-highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode))))

(use-package orderless
  :ensure t
  :config
  ;; Set up orderless dispatch alist
  (setq orderless-affix-dispatch-alist
        '((?! . orderless-without-literal)
          (?& . orderless-annotation)
          (?% . char-fold-to-regexp)
          (?` . orderless-initialism)
          (?= . orderless-literal)
          (?^ . orderless-literal-prefix)
          (?~ . orderless-flex)))

  ;; Custom orderless dispatcher for vertico
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))))

  (add-to-list 'orderless-style-dispatchers '+vertico-orderless-dispatch)

  ;; Set completion styles and related configurations
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)

  ;; Adjust face attributes for completions
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package consult
  :ensure t
  :defer t
  :general
  ([remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap evil-show-jumps]               #'+vertico/jump-list
   [remap evil-show-registers]           #'consult-register
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap Info-search]                   #'consult-info
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap man]                           #'consult-man
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop
   [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  :config
  ;; Advice to enable recentf-mode for consult-recent-file and consult-buffer
  (defun +vertico--consult-recentf-a (&rest _args)
    "`consult-recent-file' needs `recentf-mode' on to work correctly.
`consult-buffer' needs `recentf-mode' to show file candidates."
    (recentf-mode +1))
  (advice-add #'consult-recent-file :before #'+vertico--consult-recentf-a)
  (advice-add #'consult-buffer :before #'+vertico--consult-recentf-a)

  ;; Consult settings
  (setq ;; consult-project-function #'doom-project-root
        consult-narrow-key "<"
        ;; consult-line-numbers-widen t
        ;; consult-async-min-input 2
        consult-async-min-input 3
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-fd-args
        '((if (executable-find "fdfind") "fdfind" "fd")
          "--color=never"
          "--full-path --absolute-path"
          "--hidden --exclude .git"
          (if (eq system-type 'windows-nt) "--path-separator=/")))

  ;; Customization of consult commands
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")

  ;; TODO: Replace or implement these functions
  ;; Additional customization if the default config module is present
  ;; (when (featurep 'doom-defaults)
    ;; (consult-customize
     ;; +default/search-project +default/search-other-project
     ;; +default/search-project-for-symbol-at-point
     ;; +default/search-cwd +default/search-other-cwd
     ;; +default/search-notes-for-symbol-at-point
     ;; +default/search-emacsd
     ;; :preview-key "C-SPC"))

  ;; Customization for consult-theme
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any)))

(use-package consult-dir
  :defer t
  :general
  ;; Key remappings and bindings for vertico-map
  ([remap list-directory] #'consult-dir)
   (:keymaps 'vertico-map
            "C-x C-d" #'consult-dir
            "C-x C-j" #'consult-dir-jump-file))

;;;###autoload
(defun +vertico/embark-magit-status (file)
  "Run `magit-status` on repo containing the embark target."
  (interactive "GFile: ")
  (magit-status (locate-dominating-file file ".git")))

;;;###autoload
(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

(use-package embark
  :ensure t
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :general
  ([remap describe-bindings] #'embark-bindings
   "C-;" #'embark-act)  
   (:keymaps  'minibuffer-local-map
              "C-;" #'embark-act
              "C-c C-;" #'embark-export
              "C-c C-l" #'embark-collect
              :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write)
   ;; TODO: Remove this as it will conflict with AI
   ;; (:leader
    ;; :desc "Actions" "a" #'embark-act) ; to be moved to :config default if accepted
  :config
  (require 'consult)

  ;; (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)

  ;; Ensure which-key is loaded before configuring which-key integration
  (with-eval-after-load 'which-key
    (defun +vertico--embark-which-key-prompt-a (fn &rest args)
                "Hide the which-key indicator immediately when using the completing-read prompter."
                (which-key--hide-popup-ignore-command)
                (let ((embark-indicators
                       (remq #'embark-which-key-indicator embark-indicators)))
                  (apply fn args)))

    (advice-add #'+vertico--embark-which-key-prompt-a :around #'embark-completing-read-prompter)
    (cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators))

  ;; Additional keybindings for embark-file-map
  (general-def 'embark-file-map
              ;; :desc "Open target with sudo"       "s"   #'doom/sudo-find-file
              :desc "Open magit-status of target" "g"   #'+vertico/embark-magit-status))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode)
  :general
  (:keymaps 'minibuffer-local-map
            :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  ;; If the +icons module is enabled, set up nerd-icons for marginalia
  ;; (when (featurep '+icons)
  ;; (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
  
  ;; Override marginalia--project-root with doom-project-root
  ;; (advice-add #'marginalia--project-root :override #'doom-project-root)
  
  ;; Add custom command categories to marginalia-command-categories
  ;; (pushnew! marginalia-command-categories
  ;; '(+default/find-file-under-here . file)
  ;; '(doom/find-file-in-emacsd . project-file)
  ;; '(doom/find-file-in-other-project . project-file)
  ;; '(doom/find-file-in-private-config . file)
  ;; '(doom/describe-active-minor-mode . minor-mode))
  (setq marginalia-align 'right))

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package which-key
  :hook (elpaca-after-init . which-key-mode))

;;; Corfu
;; TODO: Bindings
(use-package corfu
  :ensure t
  :defer t
  :general
  (:keymaps 'corfu-mode-map
            "C-SPC" #'completion-at-point)
  (:keymaps 'corfu-map
            "C-SPC"    #'corfu-insert-separator
            "C-k"      #'corfu-previous
            "C-j"      #'corfu-next
            "TAB"      #'corfu-next
            "RET"      nil
            [remap meow-insert-exit] #'corfu-quit)
  (:keymaps 'corfu-popupinfo-map
            "C-h"      #'corfu-popupinfo-toggle
            "C-S-k"    #'corfu-popupinfo-scroll-down
            "C-S-j"    #'corfu-popupinfo-scroll-up)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.18
        corfu-auto-prefix 1
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)

  ;; Enable corfu-history to maintain completion history
  (corfu-history-mode +1)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))

  ;; Enable popup info for detailed information on completion
  (corfu-popupinfo-mode +1)
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :init
  ;; Enable Dabbrev completion globally
  (setq cape-dabbrev-check-other-buffers t)

  (defvar my-dabbrev-ignored-buffer-modes '(pdf-view-mode doc-view-mode tags-table-mode)
    "List of modes to ignore when using dabbrev.")

  (defun +dabbrev-friend-buffer-p (other-buffer)
    "Check if OTHER-BUFFER should be considered for dabbrev completion."
    (not (memq (buffer-local-value 'major-mode other-buffer) my-dabbrev-ignored-buffer-modes)))

  (setq dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p)
  :general
  (:keymaps 'corfu-map
            "C-M-/"  #'cape-dabbrev
            "C-M-]"  #'cape-file
            "C-c p"  #'cape-file
            "C-M-i"  #'cape-symbol))

(use-package corfu-terminal
  :ensure t
  :defer t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package kind-icon
  :ensure t
  :defer t
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default)  ;; Use corfu's default face for icons
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (setq kind-icon-use-icons nil))  ;; Disable icons and use text instead

(provide 'completion)

;;; completion.el ends here
