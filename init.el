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
  (set-frame-font "PragmataPro 15" nil t)

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


(use-package projectile
  :ensure t
  :straight t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package consult
  :ensure t
  :straight t)

(use-package orderless
  :ensure t
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :straight t
  :init
  (vertico-mode +1))

(use-package vertico-posframe
  :ensure t
  :straight t
  :config (vertico-posframe-mode 1))

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

(use-package git-gutter
  :ensure t
  :straight t
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

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
