;;; completion.el --- Configuration for completion system -*- no-byte-compile: t; lexical-binding: t; -*-

;;;###autoload
(defun +vertico/embark-preview ()
  "Preview candidate in vertico buffer, unless it's a consult command."
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (if (fboundp 'embark-dwim)
        (save-selected-window
          (let (embark-quit-after-action)
            (embark-dwim)))
      (user-error "Embark not installed, aborting..."))))

;;;###autoload
(defun +vertico/enter-or-preview ()
  "Enter directory or embark preview on current candidate."
  (interactive)
  (when (>= vertico--index 0)
    (if (and (let ((cand (vertico--candidate)))
               (or (string-suffix-p "/" cand)
                   (and (vertico--remote-p cand)
                        (string-suffix-p ":" cand))))
             (not (equal vertico--base "")))
        (vertico-insert)
      (condition-case _
          (+vertico/embark-preview)
        (user-error (vertico-directory-enter))))))


;;; Completion and Navigation Configuration

(use-package vertico
  :ensure t
  :defer t
  :commands vertico-mode
  :hook ((after-init . vertico-mode)
         (vertico-mode . vertico-multiform-mode))  ;; Activate multiform mode with vertico
  :bind (:map vertico-map
              ("C-SPC" . +vertico/embark-preview)
              ("C-j"   . vertico-next)
              ("C-k"   . vertico-previous)
              ("C-h"   . vertico-directory-up)
              ("C-l"   . +vertico/enter-or-preview))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)

  ;; Ensure vertico-multiform is loaded before configuring
  (require 'vertico-multiform)

  ;; Vertico-multiform customizations
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)

  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
        (if (or (eq sym major-mode)
                (and
                 (memq sym minor-mode-list)
                 (boundp sym)
                 (symbol-value sym)))
            (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
      cmd))

  ;; Set up multiform categories and commands
  (add-to-list 'vertico-multiform-categories
               '(file
                 (+vertico-transform-functions . +vertico-highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode))))


(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)
  ;; Consistent appearance in `find-file`.
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-align 'right))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (global-set-key [remap bookmark-jump]                 #'consult-bookmark)
  (global-set-key [remap goto-line]                     #'consult-goto-line)
  (global-set-key [remap imenu]                         #'consult-imenu)
  (global-set-key [remap Info-search]                   #'consult-info)
  (global-set-key [remap locate]                        #'consult-locate)
  (global-set-key [remap load-theme]                    #'consult-theme)
  (global-set-key [remap man]                           #'consult-man)
  (global-set-key [remap recentf-open-files]            #'consult-recent-file)
  (global-set-key [remap switch-to-buffer]              #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
  (global-set-key [remap yank-pop]                      #'consult-yank-pop)
  
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-fd-args
        '((if (executable-find "fdfind") "fdfind" "fd")
          "--color=never"
          "--full-path" "--absolute-path"
          "--hidden" "--exclude" ".git"
          (if (eq system-type 'windows-nt) "--path-separator=/")))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any)))

(use-package consult-dir
  :after vertico
  :bind (:map vertico-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file))
  :init
  (global-set-key [remap list-directory] #'consult-dir))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-yasnippet
  :defer t
  :init
  (global-set-key [remap yas-insert-snippet] #'consult-yasnippet))

(use-package embark
  :ensure t
  :defer t
  :bind (("C-;" . embark-act)
         :map minibuffer-local-map
         ("C-;"     . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect))
  :config
  (global-set-key [remap describe-bindings] #'embark-bindings)
  
  ;; Hide the mode line of Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Additional Packages

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

(use-package which-key
  :hook (after-init . which-key-mode))

;;; Corfu
;; TODO: Bindings
(use-package corfu
  :ensure t
  :defer t
  :bind (:map corfu-mode-map
              ("C-SPC" . #'completion-at-point)
              :map corfu-map
              ("C-SPC" . #'corfu-insert-separator)
              ("C-k" . #'corfu-previous)
              ("C-j" . #'corfu-previous)
              ("TAB" . #'corfu-next)
              ("RET" . nil)
              ;; Remap 'meow-insert-exit' to 'corfu-quit'
              ([remap meow-insert-exit] . #'corfu-quit)
              :map corfu-popupinfo-map
        ("C-h"      . #'corfu-popupinfo-toggle)
        ;; Reversed.  because popupinfo assumes opposite of what feels intuitive
        ;; with evi. l.
        ("C-S-k"    . #'corfu-popupinfo-scroll-down)
        ("C-S-j"    . #'corfu-popupinfo-scroll-up))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         ;; (minibuffer-setup . +corfu-enable-in-minibuffer)
         ) ;; Custom hook for minibuffer
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config

;;  (setq global-corfu-minibuffer
;;      (lambda ()
;;        (not (or (bound-and-true-p vertico--input)
;;                 (eq (current-local-map) read-passwd-map)))))
;;
;;    (global-corfu-mode)
;;
;;  (defun +corfu-enable-in-minibuffer ()
;;    "Enable Corfu in the minibuffer."
;;    (when (and (not (bound-and-true-p mct--active))
;;               (not (bound-and-true-p vertico--input))
;;               (or (and (featurep 'auth-source)
;;                        (eq (current-local-map) read-passwd-map))
;;                   (and (featurep 'helm-core) (helm--alive-p))
;;                   (and (featurep 'ido) (ido-active))
;;                   (where-is-internal 'minibuffer-complete (list (current-local-map)))
;;                   (memq #'ivy--queue-exhibit post-command-hook)
;;                   (where-is-internal #'completion-at-point (list (current-local-map)))))
;;      (setq-local corfu-echo-delay nil)
;;      (corfu-mode +1)))

  (setq corfu-auto t
        corfu-auto-delay 0.18
        corfu-auto-prefix 2
        global-corfu-modes '((not erc-mode circe-mode help-mode gud-mode vterm-mode) t)
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)

  (add-to-list 'completion-category-overrides '(lsp-capf (styles ,@completion-styles)))
  (add-to-list 'corfu-auto-commands #'lispy-colon)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-smart-sep-toggle-escape)

  ;; Update visual hints after completing minibuffer commands
;;  (advice-add 'exit-minibuffer :before
;;              (lambda ()
;;                (when (or (and (frame-live-p corfu--frame)
;;                               (frame-visible-p corfu--frame))
;;                          (and (featurep 'corfu-terminal)
;;                               (popon-live-p corfu-terminal--popon)))
;;                  (dolist (timer '(isearch-lazy-highlight-timer
;;                                   anzu--update-timer
;;                                   evil--ex-search-update-timer))
;;                    (when (member timer timer-idle-list)
;;                      (apply (timer--function timer)
;;                             (timer--args timer)))))))

  ;; Enable terminal support if not in a graphical environment
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))

  ;; Enable corfu-history to maintain completion history
  (corfu-history-mode +1)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))

  ;; Enable popup info for detailed information on completion
  (corfu-popupinfo-mode +1)
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))



(use-package cape
  :defer t
  :init
  (add-hook 'prog-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-file -10 t)))

  (dolist (hook '(org-mode-hook markdown-mode-hook))
    (add-hook hook
              (lambda () (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))))

  ;; Enable Dabbrev completion globally
  (setq cape-dabbrev-check-other-buffers t)

  (defvar my-dabbrev-ignored-buffer-modes '(pdf-view-mode doc-view-mode tags-table-mode)
    "List of modes to ignore when using dabbrev.")

  (defun +dabbrev-friend-buffer-p (other-buffer)
    "Check if OTHER-BUFFER should be considered for dabbrev completion."
    (let ((other-mode (buffer-local-value 'major-mode other-buffer)))
      (and (< (buffer-size other-buffer) 500000) ;; Adjust size limit for efficiency
           (not (memq other-mode my-dabbrev-ignored-buffer-modes)))))

  (dolist (hook '(prog-mode-hook
                  text-mode-hook
                  conf-mode-hook
                  comint-mode-hook
                  minibuffer-setup-hook
                  eshell-mode-hook))
    (add-hook hook
              (lambda () (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))))

  (setq dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
        dabbrev-ignored-buffer-regexps
        '("\\` " "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?")
        dabbrev-upcase-means-case-search t)

  ;; Make capfs composable with LSP and other modes
  (dolist (command '(lsp-completion-at-point
                     comint-completion-at-point
                     eglot-completion-at-point
                     pcomplete-completions-at-point))
    (advice-add command :around #'cape-wrap-nonexclusive))

  ;; Compatibility with older Emacs versions for Eshell
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))
