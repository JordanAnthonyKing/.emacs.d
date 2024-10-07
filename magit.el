;;; magit.el --- Configuration for magit -*- no-byte-compile: t; lexical-binding: t; -*-

(setq auth-sources '("~/.authinfo"))

(use-package ghub :ensure (ghub :branch "main" :host github :repo "magit/ghub") :defer t)
(use-package with-editor :ensure (with-editor :branch "main" :host github :repo "magit/with-editor") :defer t)
(use-package transient :ensure (transient :branch "main" :host github :repo "magit/transient") :defer t)

(use-package magit
  :ensure (magit :branch "main" :host github :repo "magit/magit" :pre-build ("make" "info"))
  :commands (magit-status magit-file-delete)
  ;; :init
  ;; (setq magit-auto-revert-mode t)
  :config
  (setq magit-refresh-status-buffer nil)
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

  (define-key magit-mode-map "q" #'+magit/quit)
  (define-key magit-mode-map "Q" #'+magit/quit-all)
  (define-key transient-map [escape] #'transient-quit-one)

  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

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

(use-package forge
  :ensure (forge :branch "main" :host github :repo "magit/forge")
  :defer t
  :commands (forge-create-pullreq forge-create-issue)
  :preface
  ;; TODO: Minimal emacs dir
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

(use-package code-review
  :ensure (code-review :host github :repo "doomelpa/code-review")
  :after magit forge
  :init
  (setq code-review-db-database-file (concat user-emacs-directory "data/code-review/code-review-db-file.sqlite")
        code-review-log-file (concat user-emacs-directory "data/code-review/code-review-error.log")
        code-review-download-dir (concat user-emacs-directory "data/code-review/"))
  (setq code-review-github-host "ghe.soti.net/api/v3"
        code-review-github-graphql-host "ghe.soti.net/api"
        code-review-github-base-url "ghe.soti.net")
  (defun +magit/start-code-review (arg)
    (interactive "P")
    (call-interactively
     (if (or arg (not (featurep 'forge)))
         #'code-review-start
       #'code-review-forge-pr-at-point)))

  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-code-review))
  (with-eval-after-load 'forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-code-review)))
  (setq code-review-auth-login-marker 'forge))

(use-package org-jira
  ;; :ensure (org-jira :host github :repo "ahungry/org-jira")
  ;; :ensure (org-jira :host github :branch "custom-fields" :repo "Ruin0x11/org-jira")
  :ensure (org-jira :host github :branch "custom-fields" :repo "JordanAnthonyKing/org-jira")
  :defer t
  :config
  (setq jiralib-user "jking")
  (setq jiralib-host "jira.soti.net")
  (setq jiralib-url "https://jira.soti.net")
  (setq jiralib-update-issue-fields-exclude-list '(reporter assignee))
  (setq jiralib-token
        (cons "Authorization"
              (concat "Bearer " (auth-source-pick-first-password
                                 :host "jira.soti.net")))))

