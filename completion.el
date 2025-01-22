;;; completion.el --- Configuration for completion system -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package orderless
  :ensure t
  :defer t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space))

(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :general
  (:keymaps 'vertico-map
            "M-RET"   #'vertico-exit-input
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
                            (vertico-insert)))
            "DEL"     #'vertico-directory-delete-char)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function #'consult-completion-in-region)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package vertico-multiform
  :ensure nil
  :defer t
  :hook (vertico-mode . vertico-multiform-mode)
  :config
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

  (add-to-list 'vertico-multiform-categories
               '(file
                 (+vertico-transform-functions . +vertico-highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode))))

(use-package consult
  :ensure t
  :defer t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :general
  ([remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   ;; [remap evil-show-jumps]               #'+vertico/jump-list
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
   [remap switch-to-buffer-other-tab]    #'consult-buffer-other-tab
   [remap yank-pop]                      #'consult-yank-pop
   [remap repeat-complex-command]        #'consult-complex-command
   [remap project-switch-to-buffer]      #'consult-project-bruffer
   [remap isearch-edit-string]           #'consult-isearch-history
   [remap next-matching-history-element] #'consult-history
   [remap previous-matching-history-element] #'consult-history)
  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "C-RET")
  (setq consult-narrow-key "<"))

;; (use-package consult-dir
  ;; :ensure t
  ;; :after vertico
  ;; :general
  ;; ("C-x C-d" #'consult-dir)
  ;; (:keymaps vertico-map
            ;; "C-x C-d" #'consult-dir
            ;; "C-x C-j" #'consult-dir-jump-file))

(use-package consult-xref
  :ensure nil
  :defer t
  :after consult
  :init
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :ensure t
  :defer t
  :general
  ([remap describe-bindings] #'embark-bindings
   "C-;" #'embark-act)
  (:keymaps  'minibuffer-local-map
             "C-;" #'embark-act
             "M-;" #'embark-dwim
             "C-c C-;" #'embark-export
             "C-c C-l" #'embark-collect)
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :config
  (keymap-set embark-general-map "s" #'consult-ripgrep)

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))
  
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :defer nil
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; TODO
;; (use-package consule-eglot-embark)

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode)
  :general
  (:keymaps 'minibuffer-local-map
            :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (setq marginalia-align 'right))

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

;;; completion.el ends here
