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

(use-package projectile
  :ensure t
  :straight t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package consult
  :ensure t
  :straight t)

(use-package consult-projectile
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
  :commands magit-status
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :ensure t
  :straight t
  :config
  (define-fringe-bitmap 'me/diff-hl-insert [240] nil nil '(center t))
  (define-fringe-bitmap 'me/diff-hl-change [240] nil nil '(center t))
  (define-fringe-bitmap 'me/diff-hl-delete (make-vector 6 240) nil nil 'top)
  ;; (with-eval-after-load 'magit
  ;;   (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  ;;   (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  :custom
  (diff-hl-fringe-bmp-function #'me/diff-hl-fringe-bitmap)
  (diff-hl-show-staged-changes nil)
  :hook
  ((prog-mode text-mode) . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode)
  :preface
  (defun me/diff-hl-fringe-bitmap (type _position)
    "Return the name of the bitmap to use for a given change TYPE."
    (intern (format "me/diff-hl-%s" type))))

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
