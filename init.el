(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(load-directory "~/.config/emacs/lisp")

(use-package emacs
  :init
  (setq sentence-end-double-space nil)
  (setq inhibit-startup-message t)
  (setq visible-bell t)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-display-line-numbers-mode t)
  (electric-pair-mode)
  )

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
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
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
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
