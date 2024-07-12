(use-package emacs
  :init
  (electric-pair-mode)
  (global-display-line-numbers-mode t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (set-default-coding-systems 'utf-8-unix)
  (set-language-environment "UTF-8")
  (tool-bar-mode -1)
  (setq confirm-kill-processes nil)
  (setq gc-cons-threshold 100000000)
  (setq inhibit-startup-message t)
  (setq lsp-idle-delay 0.500)
  (setq read-process-output-max (* 1024 1024))
  (setq sentence-end-double-space nil)
  (setq use-short-answers t)
  (set-frame-font "PragmataPro 17" nil t)
  (setq create-lockfiles nil)
  (setq truncate-lines t)
  (load-theme 'modus-vivendi t)

  (setq tab-always-indent 'complete)
  (setq text-mode-ispell-word-completion nil)
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
	(bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (defun load-directory (dir)
    (let ((load-it (lambda (f)
		     (load-file (concat (file-name-as-directory dir) f)))
		   ))
      (mapc load-it (directory-files dir nil "\\.el$"))))

  (load-directory "~/.config/emacs/lisp"))

(use-package diminish :ensure t :straight t)

(use-package multiple-cursors
  :ensure t
  :straight t
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-*"         . mc/mark-all-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(use-package corfu
  :ensure t
  :straight t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :init (global-corfu-mode))

(use-package envrc
  :ensure t
  :straight t
  :hook (after-init . envrc-global-mode))

(use-package consult
  :ensure t
  :straight t)

(use-package orderless
  :ensure t
  :straight t
  :init
  (icomplete-mode)
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :straight t
  :init
  (vertico-mode +1))

(use-package marginalia
  :ensure t
  :straight t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package expand-region
  :ensure t
  :straight t
  :bind ("C-@" . er/expand-region))

(use-package rainbow-delimiters
  :ensure t
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  
  (set-face-attribute 'rainbow-delimiters-base-face nil :bold t)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :bold t :foreground "white" :background "red")
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil :bold t :foreground "black" :background "yellow"))

(use-package magit
  :ensure t
  :straight t
  :bind ("C-c g" . magit-status)
  :init
  (use-package with-editor :ensure t)

  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))


(use-package diff-hl
  :ensure t
  :straight t
  :config
  (with-eval-after-load 'magit
     (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
     (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  :custom
  (diff-hl-show-staged-changes nil)
  :hook
  ((prog-mode text-mode) . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode)
  :bind (("s-n"         . diff-hl-next-hunk)
         ("s-p"         . diff-hl-previous-hunk)))

(use-package all-the-icons
  :ensure t
  :straight t
  :if (display-graphic-p))

(use-package rg
  :ensure t
  :straight t
  :config
  (rg-enable-default-bindings))

(use-package exec-path-from-shell
  :ensure t
  :straight t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package exercism
  :ensure t
  :straight t
  :commands exercism
  :config
  (defun my-eval-and-run-all-tests-in-buffer ()
    "Deletes all loaded tests from the runtime, evaluates the current buffer and runs all loaded tests with ert."
    (interactive)
    (ert-delete-all-tests)
    (eval-buffer)
    (ert 't)))

(use-package yasnippet
  :ensure t
  :straight t
  :config (yas-global-mode 1))

(use-package project
  :bind
  (("C-x p g" . consult-ripgrep)))
