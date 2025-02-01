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
   [remap occur]                         #'consult-line
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
  :general
  (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)                  ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

;; (setq read-file-name-function #'consult-find-file-with-preview)
;; 
;; (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
;;   (interactive)
;;   (let ((default-directory (or dir default-directory))
;;         (minibuffer-completing-file-name t))
;;     (consult--read #'read-file-name-internal :state (consult--file-preview)
;;                    :prompt prompt
;;                    :initial initial
;;                    :require-match mustmatch
;;                    :predicate pred)))

    ;; (defvar my-consult-line-map
    ;; (let ((map (make-sparse-keymap)))
    ;;     (define-key map "\C-s" #'previous-history-element)
    ;;     map))

;;     (consult-customize consult-line :keymap my-consult-line-map)
  
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "C-RET")
  (setq consult-narrow-key "<")

;;; TEST


(defun my-consult--source-recentf-items-uniq ()
  (let ((ht (consult--buffer-file-hash))
        file-name-handler-alist ;; No Tramp slowdown please.
        items)
    (dolist (file (my-recentf-list-uniq) (nreverse items))
      ;; Emacs 29 abbreviates file paths by default, see
      ;; `recentf-filename-handlers'.
      (unless (eq (aref (cdr file) 0) ?/)
        (setcdr file (expand-file-name (cdr file))))
      (unless (gethash (cdr file) ht)
        (push (propertize
               (car file)
               'multi-category `(file . ,(cdr file)))
              items)))))

(plist-put consult--source-recent-file
           :items #'my-consult--source-recentf-items-uniq)

(defun my-recentf-list-uniq ()
  (let* ((proposed (mapcar (lambda (f)
                             (cons (file-name-nondirectory f) f))
                           recentf-list))
         (recentf-uniq proposed)
         conflicts resol file)
    ;; collect conflicts
    (while proposed
      (setq file (pop proposed))
      (if (assoc (car file) conflicts)
          (push (cdr file) (cdr (assoc (car file) conflicts)))
        (if (assoc (car file) proposed)
            (push (list (car file) (cdr file)) conflicts))))
    ;; resolve conflicts
    (dolist (name conflicts)
      (let* ((files (mapcar (lambda (f)
                              ;; data structure:
                              ;; (file remaining-path curr-propos)
                              (list f
                                    (file-name-directory f)
                                    (file-name-nondirectory f)))
                            (cdr name)))
             (curr-step (mapcar (lambda (f)
                                  (file-name-nondirectory
                                   (directory-file-name (cadr f))))
                                files)))
        ;; Quick check, if there are no duplicates, we are done.
        (if (eq (length curr-step) (length (seq-uniq curr-step)))
            (setq resol
                  (append resol
                          (mapcar (lambda (f)
                                    (cons (car f)
                                          (file-name-concat
                                           (file-name-nondirectory
                                            (directory-file-name (cadr f)))
                                           (file-name-nondirectory (car f)))))
                                  files)))
          (while files
            (let (files-remain)
              (dolist (file files)
                (let ((curr-propos (caddr file))
                      (curr-part (file-name-nondirectory
                                  (directory-file-name (cadr file))))
                      (rest-path (file-name-directory
                                  (directory-file-name (cadr file))))
                      (curr-step
                       (mapcar (lambda (f)
                                 (file-name-nondirectory
                                  (directory-file-name (cadr f))))
                               files)))
                  (cond ((length= (seq-uniq curr-step) 1)
                         ;; If all elements of curr-step are equal, we skip
                         ;; this path part.
                         (push (list (car file)
                                     rest-path
                                     curr-propos)
                               files-remain))
                        ((member curr-part (cdr (member curr-part curr-step)))
                         ;; There is more than one curr-part in curr-step
                         ;; for this candidate.
                         (push (list (car file)
                                     rest-path
                                     (file-name-concat curr-part curr-propos))
                               files-remain))
                        (t
                         ;; There is no repetition of curr-part in curr-step
                         ;; for this candidate.
                         (push (cons (car file)
                                     (file-name-concat curr-part curr-propos))
                               resol)))))
              (setq files files-remain))))))
    ;; apply resolved conflicts
    (let (items)
      (dolist (file recentf-uniq (nreverse items))
        (let ((curr-resol (assoc (cdr file) resol)))
          (if curr-resol
              (push (cons (cdr curr-resol) (cdr file)) items)
            (push file items)))))))





  ;;; TEST
  )

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
