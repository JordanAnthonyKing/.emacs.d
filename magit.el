;;; magit.el --- Configuration for magit -*- no-byte-compile: t; lexical-binding: t; -*-


(use-package ghub :ensure (ghub :branch "main" :host github :repo "magit/ghub"))
(use-package with-editor :ensure (with-editor :branch "main" :host github :repo "magit/with-editor"))
(use-package transient :ensure (transient :branch "main" :host github :repo "magit/transient"))

(use-package magit
  :ensure (magit :branch "main" :host github :repo "magit/magit" :pre-build ("make" "info"))
  :commands (magit-file-delete)
  :init
  (setq magit-auto-revert-mode t)
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode)

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers. It's disorienting, especially on
  ;; larger projects.
  (defvar +magit--pos nil)

  ;; Save the window state before Magit refreshes.
  (defun +magit--set-window-state-h ()
    "Save the current buffer, point, and window start position before Magit refresh."
    (setq-local +magit--pos (list (current-buffer) (point) (window-start))))

  ;; Restore the window state after Magit refreshes.
  (defun +magit--restore-window-state-h ()
    "Restore the buffer, point, and window start position after Magit refresh."
    (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
      (goto-char (cadr +magit--pos))
      (set-window-start nil (caddr +magit--pos) t)
      (kill-local-variable '+magit--pos)))

  ;; Add hooks for saving and restoring window state during Magit refreshes.
  (add-hook 'magit-pre-refresh-hook #'+magit--set-window-state-h)
  (add-hook 'magit-post-refresh-hook #'+magit--restore-window-state-h)

  (define-key magit-mode-map "q" #'+magit/quit)
  (define-key magit-mode-map "Q" #'+magit/quit-all)

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one)

  ;; An optimization that particularly affects macOS and Windows users: by
  ;; resolving `magit-git-executable' Emacs does less work to find the
  ;; executable in your PATH, which is great because it is called so frequently.
  ;; However, absolute paths will break magit in TRAMP/remote projects if the
  ;; git executable isn't in the exact same location.

  (defun +magit-optimize-process-calls-h ()
    "Optimize Magit's process calls by resolving the absolute path of `magit-git-executable'."
    (when-let ((path (executable-find magit-git-executable t)))
      (setq-local magit-git-executable path)))

  (add-hook 'magit-status-mode-hook #'+magit-optimize-process-calls-h))

(setq auth-sources '("~/.authinfo"))

(use-package forge
  :ensure (forge :branch "main" :host github :repo "magit/forge")
  :defer t
  :commands (forge-create-pullreq forge-create-issue)
  :preface
  (setq forge-database-file (concat user-emacs-directory "data/forge/forge-database.sqlite"))
  (setq forge-add-default-bindings t)
  :config
  ;; Rebind "q" to kill the buffer in all forge topic list modes
  (general-def 'normal forge-topic-list-mode-map
    "q" #'kill-current-buffer)

  ;; Additional custom bindings if forge's default bindings are disabled
  (unless forge-add-default-bindings
    (general-def 'normal magit-mode-map
      [remap magit-browse-thing] #'forge-browse)
    (general-def 'normal magit-remote-section-map
      [remap magit-browse-thing] #'forge-browse-remote)
    (general-def 'normal magit-branch-section-map
      [remap magit-browse-thing] #'forge-browse-branch))

  ;; Set up popup rules
  ;; (set-popup-rule! "^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)" :size 0.45 :modeline t :ttl 0 :quit nil)
  ;; (set-popup-rule! "^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" :ignore t)
  (add-to-list 'forge-alist
               '("ghe.soti.net" "ghe.soti.net/api/v3" "ghe.soti.net" forge-github-repository)))

;; (use-package org-jira
;; :ensure (org-jira :host github :branch "custom-fields" :repo "Ruin0x11/org-jira")
;; :defer t
;; :config
;; (setq jiralib-url "https://jira.soti.net"))

;; (use-package org-jira
;; :ensure (org-jira :host github :repo "ahungry/org-jira")
;; :defer t
;; :config
;; (setq jiralib-url "https://jira.soti.net"))

;;;###autoload
(defun +magit/start-code-review (arg)
  (interactive "P")
  (call-interactively
    (if (or arg (not (featurep 'forge)))
        #'code-review-start
      #'code-review-forge-pr-at-point)))

(use-package code-review
  :ensure (code-review :host github :branch "fix/closql-update" :repo "phelrine/code-review")
  :after magit forge
  :init
  (setq code-review-db-database-file (concat user-emacs-directory "data/code-review/code-review-db-file.sqlite")
        code-review-log-file (concat user-emacs-directory "data/code-review/code-review-error.log")
        code-review-download-dir (concat user-emacs-directory "data/code-review/"))
  (setq code-review-github-host "ghe.soti.net/api/v3"
        code-review-github-graphql-host "ghe.soti.net/api"
        code-review-github-base-url "ghe.soti.net")
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-code-review))
  (with-eval-after-load 'forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-code-review)))
  ;; :config
  ;; (setq code-review-auth-login-marker 'forge)
  )
